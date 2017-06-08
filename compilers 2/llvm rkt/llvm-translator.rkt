#lang racket

(require "llvm-emitter.rkt"
         "names.rkt"
         (prefix-in t: "types.rkt")
         "niparser.rkt"
         "typecheck.rkt"
         "errors.rkt"
         "log.rkt")

(provide (all-defined-out))

; simple translator that only outputs the stuff that you've added, not all
; the preamble and postamble materials
(define (trans str)
  (clear-errors)
  (clear-writers)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse error")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (begin
                (ast->llvm (first ast))
                (finish-emission)
                (clear-writers)))))))


; actually translates the string and emits the entire 
(define (translate-str str)
  ; clear the errors first
  (clear-errors)
  (clear-writers)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse errors")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (begin
                (translate-ast ast)))))))

(define (translate-ast ast)
  ; begin with the prelude
  (emit-header)
  (emit-main-header)
  (ast->llvm (first ast))
  (emit-main-trailer)
  (finish-emission))

; the main recursive entry into emitting LLVM code--here we are 
(define (ast->llvm ast)
  (let ([result (match ast
                  ; deal with lists, like in let expressions
                  ['() '()]
                  [(list node) (ast->llvm node)]
                  [(cons first rest) (ast->llvm first) (ast->llvm rest)]
    
                  ; integer literals
                  [(NumExpr val) (numexpr->llvm ast val)]

                  [(StringExpr val) (stringexpr->llvm ast val)]

                  [(BoolVal val) (boolval->llvm ast val)]

                  ; simple math expressions
                  [(MathExpr _ _ _) (math->llvm ast)]

                  ; greater than, less than, etc
                  [(BoolExpr _ _ _) (boolexpr->llvm ast)]

                  ; logic expression, & or |
                  [(LogicExpr _ _ _) (logicexpr->llvm ast)]
    
                  ; variable declarations!
                  [(VarDecl _ _ _) (vardecl->llvm ast)]
    
                  ; function calls
                  [(FuncallExpr _ _) (funcall->llvm ast)]
       
                  ; variable expressions
                  [(VarExpr _) (var->llvm ast)]

                  ; assignment
                  [(AssignmentExpr _ _) (assign->llvm ast)]

                  ; let expressions--need these for any declarations to work!
                  [(LetExpr _ _) (letexpr->llvm ast)]

                  ; if expressions, yay!
                  [(IfExpr _ _ '()) (if-then->llvm ast)]
                  [(IfExpr _ _ _) (if-then-else->llvm ast)]

                  ; while expressions
                  [(WhileExpr _ _) (while->llvm ast)]

                  ; with expressions
                  [(WithExpr _ _ _ _) (with->llvm ast)]

                  ; now for more challenge--the fundecl
                  [(FunDecl _ _ _ _ _) (fundecl->llvm ast)]

                  ; declare array types, note that arraytype-llvm handles recursive calls to its next field
                  [(ArrayType _ _ next) (arraytype->llvm ast)]

                  ; new array creation
                  [(NewArrayExpr _ _ _) (newarray->llvm ast)]

                  ; array indices
                  [(ArrayExpr _ _) (arrayindex->llvm ast)]
    
                  [_ (error "Translation node " ast " not implemented yet!")])])
    ; here I'm assuming that each AST node will return the result of its operation
    ; so I add a note to it with its result
    (add-note ast 'result result)
    ;(printf "adding note to: ~a with result ~a~n" ast result)
    result))


; emit the creation of a new array, which really consists of calling the NiStdLib functions
(define (newarray->llvm node)
  (let ([name (NewArrayExpr-name node)]
        [num-elements (NewArrayExpr-num-elements node)]
        [init-value (NewArrayExpr-init-values node)]
        [array-type (get-note node 'type)])
    ; now emit code to calculate the count and init values
    (emit-comment (string-append "initializing new array of kind " (symbol->string name)))
    (let ([count (ast->llvm num-elements)]
          [init (ast->llvm init-value)])
      (emit-newarray array-type count init))))
          

; emit an array index!
(define (arrayindex->llvm node)
  (emit-comment "  array index")
  (let ([lval (ArrayExpr-name node)]
        [expr (ArrayExpr-expr node)]
        [islvalue? (if (has-note? node 'islvalue?) (get-note node 'islvalue?) #f)]
        [arrtype (get-note node 'type)])
    ; now the name here, the lval, needs to be further evaluated, and if we are
    ; an lvalue, we should pass this on
    (let ([lval-result (ast->llvm lval)]
          [index (ast->llvm expr)])
      ; if it's an lvalue, we need to emit and return a label, or address, of the thingy
      (if islvalue?
          (emit-get-array-location arrtype lval-result index)
          (emit-get-array-element arrtype lval-result index)))))
  
    
; emits an array type
(define (arraytype->llvm node)
  (let ([name (ArrayType-name node)]
        [ty (get-note node 'type)])
    (let ([label (emit-array-type ty)])
      (t:set-ArrayType-label! ty label)
      ; now emit on any recursive types
      (ast->llvm (ArrayType-next node))
      label)))

; emits a function call
(define (funcall->llvm node)
  (emit-comment (string-append "calling function: " (symbol->string (FuncallExpr-name node))))
  (let* ([ty (get-note node 'type)]
         ; grab the associated funval with this funcall
         [funval (get-note node 'funval)]
         [funname (if (eq? (t:FunValue-label funval) #f)
                      (symbol->string (FuncallExpr-name node))
                      (Label-name (t:FunValue-label funval)))]
         [argtypes (map (λ (arg)
                          (get-note arg 'type)) (FuncallExpr-args node))]
         [argresults (map (λ (arg)
                            (ast->llvm arg)
                            (get-note arg 'result)) (FuncallExpr-args node))])
    ; now construct things properly
    (let ([result (emit-funcall funname (get-note node 'type)
                                argresults argtypes)])
      ; now, the result may be void at this point if emit-funcall didn't return
      ; a result for the function call, and if it is, we don't have a result
      ; which is fine, since we shouldn't try to access it, otherwise we want to
      ; save it with this node
      result)))

; emits a variable expression, this is like a variable name/lvalue, not a declaration
(define (var->llvm node)
  ; the type here will be a VarValue (or should be, haha)
  (let* ([varval (get-note node 'var)]
         [ty (t:VarValue-type varval)]
         [result (t:VarValue-result varval)])
    (cond
      ; now see if the result is global or in frame (and not in register), or a pointer type
      [(should-emit-load? ty result)
       ; if it is a global or in a frame location, it's a pointer,
       ; so we need to load it into a temporary in order to use it
       (let ([location (emit-load ty result)])
         ;(add-note node 'result location)
         location)]
      [else
       ;(begin (add-note node 'result result)
       result])))

; translate a bool expression, such as <, <=, etc, to llvm
(define (boolexpr->llvm node)
  (let ([expr1 (BoolExpr-expr1 node)]
        [expr2 (BoolExpr-expr2 node)])
    (let ([res1 (ast->llvm expr1)]
          [res2 (ast->llvm expr2)])
      (if (t:StringType? (get-note expr1 'type))
          (emit-stringcomp (BoolExpr-op node) res1 res2)
          (emit-boolexpr (BoolExpr-op node) res1 res2)))))

; translate a logic expression using & or |
(define (logicexpr->llvm node)
  (let ([expr1 (LogicExpr-expr1 node)]
        [expr2 (LogicExpr-expr2 node)])
    (let ([res1 (ast->llvm expr1)]
          [res2 (ast->llvm expr2)])
      (emit-logic (LogicExpr-op node) res1 res2))))


; translate all math ops!
(define (math->llvm node)
  ; get the expressions
  (let ([expr1 (MathExpr-expr1 node)]
        [expr2 (MathExpr-expr2 node)])
    ; now get the results from translating those expressions
    (let ([res1 (ast->llvm expr1)]
          [res2 (ast->llvm expr2)])
      ; now call math on them!
      (emit-math (MathExpr-op node) res1 res2))))


; turn the assignment into llvm
(define (assign->llvm node)
  ; get the expression, then the lvalue
  (emit-comment "assignment")
  (let* ([name (AssignmentExpr-name node)]
         [expr (AssignmentExpr-expr node)]
         [nametype (get-note name 'type)])
    ; add a note so we can emit proper code based on whether or not it's an lvalue
    (add-note name 'islvalue? #t)
    (add-note name 'islvalue? #f)
    ; then emit code for each of these expressions
    (let ([nameres (ast->llvm name)]
          [exprres (ast->llvm expr)])
      ; if the name is a var expression, it could be something
      ; that has escaped, so we need to handle it as a regular
      ; store, but if it hasn't escaped, it may have a temp associated
      ; with it at this point, so we want to modify that temp result!
      (cond
        [(VarExpr? name)
         ; get the var value associated with it
         (let* ([var (get-note name 'var)]
                [varres (t:VarValue-result var)])
           ; if it's in the register, then our assignment can simply be renaming
           ; the varvalue associated with the expression being assigned
           (if (in-register? varres)
               (t:set-VarValue-result! var exprres)
               ; otherwise we emit an assignment as usual
               (emit-assign nametype varres exprres)))]
        [else
         (emit-assign nametype nameres exprres)]))))


; emits a bool literal, we'll use an i1 for fun!
(define (boolval->llvm node val)
  (emit-boolval val))
         
; emits a numeric literal
(define (numexpr->llvm node val)
  ; literal nums can go in registers
  (emit-math 'add val "0"))
    
; emit a string literal
(define (stringexpr->llvm node val)
  ; we have to allocate a string first
  (emit-literal-string val))

; handle plain-old if/then without an else
(define (if-then->llvm node)
  (let ([test (IfExpr-test node)]
        [trbr (IfExpr-true-branch node)])
    (emit-comment "if/then")
    (let ([testres (ast->llvm test)]
          [truelabel (make-label)]
          [endlabel (make-label)])
      ; emit a conditional, but our false label will be the 'end' label
      (emit-conditional-branch testres truelabel endlabel)
      ; now emit the true label
      (emit-comment "true branch")
      (emit-label truelabel)
      (ast->llvm trbr)
      (emit-jump endlabel)
      (emit-label endlabel))))
      
; emit code for branches
(define (if-then-else->llvm node)
  (let ([test (IfExpr-test node)]
        [trbr (IfExpr-true-branch node)]
        [fabr (IfExpr-false-branch node)]
        [result (make-temp-result)])
    (emit-comment "if/then/else")
    ; grab the test result first, then we can branch on it
    (let ([testres (ast->llvm test)]
          [truelabel (make-label)]
          [falselabel (make-label)]
          [endlabel (make-label)])
      ; emit the branch 
      (emit-conditional-branch testres truelabel falselabel)
      ; now emit the true label
      (emit-comment "true branch")
      (emit-label truelabel)
      ; emit true branch stuff
      (let ([trueres (ast->llvm trbr)])
        (emit-jump endlabel)
        (emit-comment "false branch")
        (emit-label falselabel)
        (let ([falseres (ast->llvm fabr)])
          (emit-jump endlabel)
          (emit-label endlabel)
          (emit-phi (get-note node 'type) trueres truelabel falseres falselabel))))))
      

; while loops!
(define (while->llvm node)
  (emit-comment "while loop:")
  (let ([test (WhileExpr-test node)]
        [body (WhileExpr-body node)]
        [testlabel (make-label)]
        [bodylabel (make-label)]
        [endlabel (make-label)])
    ; first, emit the loop label, we will branch back to it
    (emit-jump testlabel)
    (emit-comment "  while condition")
    (emit-label testlabel)
    (let ([testres (ast->llvm test)])
      ; then test the condition, branch to the end if it's false
      (emit-conditional-branch testres bodylabel endlabel)
      (emit-comment "  while body")
      (emit-label bodylabel)
      ; now emit the body
      (ast->llvm body)
      (emit-jump testlabel)
      (emit-comment "  while exit")
      (emit-label endlabel))))
      

; with loops
(define (with->llvm node)
  (emit-comment "with loop:")
  (let ([from (WithExpr-fromexpr node)]
        [to (WithExpr-toexpr node)]
        [body (WithExpr-body node)]
        [varval (get-note node 'var)]
        [varresult (make-frame-result)]
        [testlabel (make-label)]
        [bodylabel (make-label)]
        [endlabel (make-label)]
        [i64ty (t:make-IntType)])
    ; first, set our loop counter's result to the new one we created
    (t:set-VarValue-result! varval varresult)
    ; first, emit the loop label, we will branch back to it
    (emit-comment "  loop initializer")
    ; now let's emit the loop counter
    (let ([initres (ast->llvm from)])
      ; then emit the var declaration
      (emit-vardecl i64ty varresult initres)
      ; now emit the max value, this is tested only once
      (let ([maxres (ast->llvm to)])
        ; then test the condition, branch to the end if it's false
        (emit-comment "  loop condition")      
        (emit-jump testlabel)
        (emit-label testlabel)
        (let* ([tempvar (emit-load i64ty varresult)]
               [testres (emit-boolexpr 'le tempvar maxres)])
          (emit-conditional-branch testres bodylabel endlabel)
          (emit-comment "  with body")
          (emit-label bodylabel)
          ; now emit the body
          (ast->llvm body)
          ; now add to the temp value
          (emit-comment "  increment loop counter"
          (let ([addresult (emit-math '+ "1" tempvar)])
            (emit-store i64ty addresult varresult)
            (emit-jump testlabel)
            (emit-comment "  while exit")
            (emit-label endlabel))))))))


; emit code for a let expression, which primarily consists of emitting code for
; its declarations, then its body, then setting the result of its body
(define (letexpr->llvm node)
  (let ([decls (LetExpr-decs node)]
        [exprs (LetExpr-exprs node)])
    ;(emit-comment "Let expression")
    (ast->llvm decls)
    ; simply iterate through and grab the last result from translating the body of let
    (let ([last-result (foldl (λ (expr last)
                                (ast->llvm expr)
                                (get-note expr 'result)) #f exprs)])
      ; only add a note to this node if the result wasn't false
      last-result)))

; emit code for a variable declaration
(define (vardecl->llvm node)
  ; emit a comment
  (emit-comment (string-append "declaration of " (symbol->string (VarDecl-id node))))
  ; first, grab the expression, need it to get its value
  (let ([expr (VarDecl-expr node)]
        ; then grab the type, which is a varvalue 
        [varval (get-note node 'type)])
    ; first, emit code for the expression, so we can grab its result
    (ast->llvm expr)
    (let ([expr-result (get-note expr 'result)]
          [escaped? (t:VarValue-escape? varval)])
      ; now we have the expression result, so in theory we can do a store on it
      (if escaped?
          (let ([inframe (make-frame-result)])
            (t:set-VarValue-result! varval inframe)
            (set-Result-value! inframe varval)
            (let ([varresult (emit-vardecl (t:VarValue-type varval)
                                           inframe
                                           expr-result)])
              varresult))
          ; if it didn't escape, set the VarValue result to be
          ; the result of the initializing expression
          (begin
            (t:set-VarValue-result! varval expr-result)
            (set-Result-value! expr-result varval)
            expr-result)))))
      
; turns a function declaration, or set of them, into a bunch of functions           
(define (fundecl->llvm node)
  ; first, walk through each and give them a name using a label, this will
  ; ensure that when we look up their funvalues, we'll have a label associated with it
  (for-each-fun
   (λ (fun)
     (match fun
       [(FunDecl name args rettype body _)
        (let ([funval (get-note fun 'funval)]
              [funlabel (make-label)])
          ; set the label
          (t:set-FunValue-label! funval funlabel)
          ; now we create a frame result for each argument of the function
          (let ([namelist
                 (map (λ (arg)
                        (let ([argval (t:NameTypePair-type arg)]
                              [arg-escaped? (t:VarValue-escape? (t:NameTypePair-type arg))])
                          (if arg-escaped?
                              (begin (printf "escaped: ~a~n" arg-escaped? )(t:set-VarValue-result! argval (make-frame-result)))
                              (t:set-VarValue-result! argval (make-temp-result)))
                          (t:VarValue-result argval)))
                      (t:FunValue-parameters funval))]
                [typelist
                 (map (λ (arg)
                        (let ([argval (t:NameTypePair-type arg)])
                          (t:VarValue-type argval)))
                      (t:FunValue-parameters funval))])
            ; now emit the function
            (begin-fun-defn)
            ;(emit-comment (string-append "function: " (symbol->string name)))
            ;(emit-declare (t:FunValue-label funval)
            ;              (t:FunValue-return-type funval)
            ;              typelist namelist)
            (end-fun-defn)))]))
   node)
  
  ; now, let's emit a fundecl!
  (for-each-fun
   (λ (fun)
     (match fun
       [(FunDecl name args rettype body _)
        (let ([funval (get-note fun 'funval)]
              [funlabel (make-label)])
          ; set the label
          ;(t:set-FunValue-label! funval funlabel)
          ; now we create a frame result for each argument of the function
          (let ([namelist
                 (map (λ (arg)
                        (let ([argval (t:NameTypePair-type arg)])
                          (t:VarValue-result argval)))
                      (t:FunValue-parameters funval))]
                [typelist
                 (map (λ (arg)
                        (let ([argval (t:NameTypePair-type arg)])
                          (t:VarValue-type argval)))
                      (t:FunValue-parameters funval))])
            ; now emit the function
            (begin-fun-defn)
            (emit-comment (string-append "function: " (symbol->string name)))
            (emit-fun-header (t:FunValue-label funval)
                             (t:FunValue-return-type funval)
                             typelist namelist)
            ; emit the body
            (emit-comment "  fun body")
            (let ([res (ast->llvm body)])
              (add-note body 'result res)
              (if (t:VoidType? (t:FunValue-return-type funval))
                  (emit-return (t:make-VoidType) (make-temp-result))
                  (emit-return (t:FunValue-return-type funval) (get-note body 'result))))

            ; and conclude it
            (emit-fun-conclusion)            
            (end-fun-defn)))]))
   node))
     


  