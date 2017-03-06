#lang racket


(require "niparser.rkt"
         "environment.rkt"
         "errors.rkt"
         "log.rkt"
         (prefix-in types: "types.rkt")
         test-engine/racket-tests)

(provide (all-defined-out))

; define initializations here
(define (init-typeEnv)
  (let ([tenv (empty-env)])
    (extend-env tenv 'int (types:make-IntType))
    (extend-env tenv 'string (types:make-StringType))
    (extend-env tenv 'bool (types:make-BoolType))
    (extend-env tenv 'peng (types:make-PengType))
    tenv))

; define initializations here
(define (init-valEnv)
  (let ([venv (empty-env)])
    (extend-env venv 'print (types:FunValue (list (types:NameTypePair (types:make-StringType) 's)) (types:make-VoidType)))
    (extend-env venv 'printi (types:FunValue (list (types:NameTypePair (types:make-IntType) 'x)) (types:make-VoidType)))
    (extend-env venv 'flush (types:FunValue '() (types:make-VoidType)))
    (extend-env venv 'getChar (types:FunValue '() (types:make-StringType)))
    (extend-env venv 'ord (types:FunValue (list (types:NameTypePair (types:make-StringType) 's)) (types:make-IntType)))
    (extend-env venv 'chr (types:FunValue (list (types:NameTypePair (types:make-IntType) 'i)) (types:make-StringType)))
    (extend-env venv 'size (types:FunValue (list (types:NameTypePair (types:make-StringType) 's)) (types:make-IntType)))
    (extend-env venv 'substring (types:FunValue (list (types:NameTypePair (types:make-StringType) 's)
                                                      (types:NameTypePair (types:make-IntType) 'first)
                                                      (types:NameTypePair (types:make-IntType) 'n)) (types:make-StringType)))
    (extend-env venv 'concat (types:FunValue (list (types:NameTypePair (types:make-StringType) 's1)
                                                   (types:NameTypePair (types:make-StringType) 's2)) (types:make-StringType)))
    (extend-env venv 'not (types:FunValue (list (types:NameTypePair (types:make-BoolType) 'i)) (types:make-BoolType)))
    (extend-env venv 'Exit (types:FunValue (list (types:NameTypePair (types:make-IntType) 'i)) (types:make-VoidType)))
    venv))

(define valueEnv (make-parameter (init-valEnv)))
(define typeEnv (make-parameter (init-typeEnv)))


(define (init-typechecker)
  (valueEnv (init-valEnv))
  (typeEnv (init-typeEnv))
  (clear-errors))

(define (tc-str str)
  ; set up the type and value environments first
  (init-typechecker)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (cond
          [(eq? (scan-error) #t) (error "cannot continue after scanning errors")]
          [(eq? (parse-error) #t) (error "cannot continue after parsing errors")]
          [else (error "compiler error! help!")])
        (let ([ty (typecheck ast (typeEnv) (valueEnv) #f)])
          (cond
            [(eq? (scan-error) #t) (error "cannot continue after scanning errors")]
            [(eq? (parse-error) #t) (error "cannot continue after parsing errors")]
            [(eq? (type-error) #t) (error "cannot continue after type errors")])
          ty))))

(define (typecheck-ast ast)
  (valueEnv (init-valEnv))
  (typeEnv (init-typeEnv))
  ; don't clear errors in case we forgot to for some reason...
  (if (error-generated?)
      (cond
        [(eq? (scan-error) #t) (error "cannot continue after scanning errors")]
        [(eq? (parse-error) #t) (error "cannot continue after parsing errors")]
        [else (error "compiler error! help!")])
      (let ([ty (typecheck ast (typeEnv) (valueEnv) #f)])
          (cond
            [(eq? (scan-error) #t) (error "cannot continue after scanning errors")]
            [(eq? (parse-error) #t) (error "cannot continue after parsing errors")]
            [(eq? (type-error) #t) (error "cannot continue after type errors")])
          ty)))

         

; typecheck the ast, note we pass a type and value environment
; in addition, we pass a variable to determine if we're currently
; in a loop. loop? is passed as true down from loops and we can
; check if breaks are in loops
(define (typecheck ast tenv venv loop?)
  (match ast
    ; the base case for lists, this will just be the void type
    ['() (types:make-VoidType)]
    ; the recursive case, we treat a 1 element list as special
    ; because anytime we have a list of expressions, we typically
    ; typecheck such that the type of the list is the type of
    ; the last expression in the list (which is what our condition does)
    [(cons e1 explist) (let ([ty (typecheck e1 tenv venv loop?)])
                         (if (null? explist)
                             ty
                             (typecheck explist tenv venv loop?)))]
    
    ; base cases for types
    [(NumExpr val) (let ([ty (types:make-IntType)])
                     (add-note ast 'type ty)
                     ty)]
    
    [(StringExpr v) (let ([ty (types:make-StringType)])
                      (add-note ast 'type ty)
                      ty)]
    
    [(BoolVal v) (let ([ty (types:make-BoolType)])
                   (add-note ast 'type ty)
                   ty)]
    
    [(PengExpr) (let ([ty (types:make-PengType)])
                  (add-note ast 'type ty)
                  ty)]
    
    [(NoVal) (let ([ty (types:make-VoidType)])
               (add-note ast 'type ty)
               ty)]

   
    ;;; Declarations

    ; var declarations first, here we must extend with a VarValue
    [(VarDecl type id expr) (let ([t1 (types:actual-type (typecheck expr tenv venv loop?))]
                                  [dectype (if (eq? type #f)
                                               #f
                                               (types:actual-type (apply-env tenv type)))])
                              (cond
                                ; first, if the type is #f, then we need to know if
                                ; the expr is peng since if it is, we have to generate an error 
                                [(eq? type #f) (if (types:PengType? t1)
                                                   (begin
                                                     (log-typeerror "cannot assign peng to a variable if you don't declare its record type" ast)
                                                     (types:make-VoidType))
                                                   (let ([varval (types:make-VarValue t1)])
                                                     (add-note ast 'type varval)
                                                     (extend-env venv id varval)))]
                                ; if it's a record type and the init value has a peng type, we're good to go!
                                [(or (peng-match? dectype t1)
                                ; otherwise, both types must match
                                     (types:type=? dectype t1))
                                 (let ([varval (types:make-VarValue dectype)])
                                   (add-note ast 'type varval)
                                   (extend-env venv id varval))]
                                [else
                                 (begin
                                   (log-typeerror "the declared type ~a, and the type of the expressions (~a) don't match" ast type t1)
                                   (types:make-VoidType))]))]
    
    ; handle names, note we don't do anything with actual types yet, and name kinds extend the type environment,
    ; note that we need to further handle mutually recursive types
    [(NameType name kind next) (begin (typecheck-type-decls ast tenv) (types:make-VoidType))]
     
    ; handle record declarations, here we need to make sure the
    ; fields all have valid types, then we extend the environment to include
    ; this record type (which we should create)
    [(RecordType name fields next) (begin (typecheck-type-decls ast tenv) (types:make-VoidType))]

    ; handle array declarations
    [(ArrayType name fields next) (begin (typecheck-type-decls ast tenv) (types:make-VoidType))]

    ; now for function declarations!
    [(FunDecl name args rettype body next) (begin (typecheck-fun-decls ast tenv venv loop?) (types:make-VoidType))]
    
    ;;; Expressions

    ; var expressions (not declarations)
    [(VarExpr name) (let ([t1 (apply-env venv name)])
                      (cond
                        [(eq? t1 #f)
                         (begin
                           (log-typeerror "variable not declared using 'ni' before use" ast)
                           (types:make-VoidType))]
                        ; note, we have to extract the actual type out of the var
                        [(types:VarValue? t1)
                         (let ([ty (types:actual-type (types:VarValue-type t1))])
                           ; if the var value is read-only, set it on the expression, so
                           ; we can look it up if we try to assign to it
                           (cond [(eq? (types:VarValue-read-only? t1) #t)
                                  (add-note ast 'read-only name)])
                           (add-note ast 'type ty)
                           (add-note ast 'var t1)
                           ty)]
                        [else
                         (begin
                           (error "Unexpected type in the environment, should be a VarValue")
                           (types:make-VoidType))]))]
    

    ; handle some basic expressions, math expressions are the easiest, all subtypes must be ints
    [(MathExpr e1 op e2) (let ([t1 (types:actual-type (typecheck e1 tenv venv loop?))]
                               [t2 (types:actual-type (typecheck e2 tenv venv loop?))])
                           (cond
                             [(not (types:IntType? t1)) (log-typeerror "math expressions expect an int type" e1)]
                             [(not (types:IntType? t2)) (log-typeerror "math expressions expect an int type" e2)]
                             [else (types:make-IntType)]))]

    ; bool expressions are more complicated because it depends on the operator
    [(BoolExpr e1 op e2) (typecheck-boolexprs ast e1 op e2 tenv venv loop?)]


    ; logic expressions, pretty much like math expressions
    [(LogicExpr e1 op e2) (let ([t1 (types:actual-type (typecheck e1 tenv venv loop?))]
                               [t2 (types:actual-type (typecheck e2 tenv venv loop?))])
                           (cond
                             [(not (types:BoolType? t1)) (log-typeerror "logic expressions expect a bool type" e1)]
                             [(not (types:BoolType? t2)) (log-typeerror "logic expressions expect a bool type" e2)]
                             [else (types:make-BoolType)]))]
    
    ; handle record creation, here we pass the ast mainly for logging error purposes
    [(NewRecordExpr name assignments) (typecheck-record-creation ast name assignments tenv venv loop?)]
    
    ; now handle record expressions (dot notation)
    [(RecordExpr lvalue fieldname) (let* ([t1 (types:actual-type (typecheck lvalue tenv venv loop?))]
                                          [pair (find-field t1 fieldname)])
                                     (if (eq? pair #f)
                                         (log-typeerror "Unable to find record field ~a" ast fieldname)
                                         (types:NiType-actual pair)))]

    ; typecheck the creation of arrays
    [(NewArrayExpr name num-elements init-val) (let*
                                                   ; we kind of need to know the actual type by the time
                                                   ; we create an array, hahaha
                                                   ([arrty (types:actual-type (apply-env tenv name))]
                                                    [countty (types:actual-type (typecheck num-elements tenv venv loop?))]
                                                    [initty (types:actual-type (typecheck init-val tenv venv loop?))])
                                                 (cond
                                                   [(not (types:ArrayType? arrty))
                                                    (begin (log-typeerror "~a must be an array type" ast name) (types:make-VoidType))]
                                                   [(not (types:IntType? countty))
                                                    (begin (log-typeerror "number of elements in an array must be an int type" ast) (types:make-VoidType))]
                                                   [(not (types:type=? initty (types:ArrayType-element-type arrty)))
                                                    (begin
                                                      (log-typeerror "initialization value for array doesn't match type of array elements" ast)
                                                      (types:make-VoidType))])
                                                 ; we return the type so we can keep typechecking, but we've logged an error if there is
                                                 ; one at this point
                                                 arrty)]

    ; now for subscript access (brackets)
    [(ArrayExpr name-expr index) (let*
                                     ([arrty (types:actual-type (typecheck name-expr tenv venv loop?))]
                                      [indexty (types:actual-type (typecheck index tenv venv loop?))])
                                   (cond
                                     [(not (types:ArrayType? arrty)) (log-typeerror "~a must be an array type" ast arrty)]
                                     [(not (types:IntType? indexty)) (log-typeerror "index into an array must be type int" ast)])
                                   ; note, here we return the element type since we're accessing via an index into the array
                                   (types:ArrayType-element-type arrty))]
    

    ; let expressions--primarily the trick is to push scope before entering and pop it when leaving
    ; (which happens automatically since we're using recursion)
    [(LetExpr decls exprs) (let ([tenv1 (push-scope tenv)]
                                 [venv1 (push-scope venv)])
                             ; use the pushed scoped environment to typecheck the declarations
                             (typecheck decls tenv1 venv1 loop?)
                             ; now typecheck the body of the let using the same env, note that foldl will
                             ; call the function on each element of the list from left to right--the accum value
                             ; contains the result of the last call to foldl
                             ;   * also note, we want the actual-type here, this handles aliases to base types, which is
                             ;     generally ok!
                             (let ([last-type (types:actual-type (foldl (lambda (expr accum)
                                                                          (let ([exty (typecheck expr tenv1 venv1 loop?)])
                                                                           ;( printf "expr type is: ~a~n" (types:actual-type exty))
                                                                            exty))
                                                      (types:make-VoidType) exprs))])
                               ; if the type that's returned is in our base environment (not our extended one)
                               ; then it's fine to return the type, otherwise you need to fail
                               (if (or (types:base-type? last-type) (type-in-env? tenv last-type))
                                   (begin (add-note ast 'type last-type) last-type)
                                   (log-typeerror "types cannot escape let declarations: ~a, env: ~a" ast last-type tenv)
                                   )))]

    ; if! yay!
    [(IfExpr test true-branch false-branch) (let ([test-type (types:actual-type (typecheck test tenv venv loop?))]
                                                  [tty (types:actual-type (typecheck true-branch tenv venv loop?))]
                                                  [fty (if (eq? false-branch #f) (types:make-VoidType) (types:actual-type (typecheck false-branch tenv venv loop?)))])
                                              (cond
                                                [(not (types:BoolType? test-type)) (log-typeerror "condition for branch must evaluate to a boolean value" ast)]
                                                [(and (eq? false-branch #f) (not (types:VoidType? tty)))
                                                 (log-typeerror "true branch of conditional must return no value without an else branch" ast)]
                                                [(not (types:type=? tty fty)) (log-typeerror "both branches of a conditional must evaluate to the same type" ast)])
                                              ; otherwise, return tty, we do this even if we had an error so we can keep typechecking
                                              tty)]
                                                
    
     
     ; function call expressions
    [(FuncallExpr name args) (typecheck-funcall ast name args tenv venv loop?)]
       
       

    ; while expressions
    [(WhileExpr test body) (let ([testtype (typecheck test tenv venv loop?)]
                                 [bodytype (typecheck body tenv venv #t)])
                             (cond
                               [(not (types:BoolType? testtype))
                                (log-typeerror "while test expression must return a bool type, evals to ~a type" ast testtype)]
                               [(not (types:VoidType? bodytype))
                                (log-typeerror "body of while expression must return no value, evals to ~a type" ast bodytype)])
                             (types:make-VoidType))]

    ; with expressions
    [(WithExpr idname fromexpr toexpr body) (typecheck-with-expr ast idname fromexpr toexpr body tenv venv loop?)]

    ; break expressions--here we test to make sure we're in the body of a loop
    [(BreakExpr) (cond
                   [(not loop?) (log-typeerror "break not within the body of a loop (breaks cannot escape function bodies)" ast)]
                   [else (types:make-VoidType)])]

    ; assignment expressions
    [(AssignmentExpr name expr) (let ([lvaluety (typecheck name tenv venv loop?)]
                                      [exprty (typecheck expr tenv venv loop?)]
                                      [has-read-only? (has-note? name 'read-only)])
                                  ; note, we do this after typechecking because it's during then
                                  ; that we might discover the var value was read only
                                  (cond
                                    [has-read-only?
                                      (begin
                                        (log-typeerror "cannot assign to ~a, the variable from the with loop" ast
                                                       (get-note name 'read-only))
                                        (types:make-VoidType))]
                                    ; if the left side is a record or array and the right side is peng, make a void
                                    [(or (peng-match? lvaluety exprty)
                                    ; if both sides are the same
                                         (types:type=? (types:actual-type lvaluety) (types:actual-type exprty)))
                                     (types:make-VoidType)]
                                    [else (begin
                                            (log-typeerror "type mismatch between lvalue (~a) and expression (~a)" ast
                                                           lvaluety exprty)
                                            (types:make-VoidType))]))]

    ; default, error case!
    [_ (begin
         (displayln ast)
         (error "Node not implemented yet!"))]
    ))


; typecheck with expressions--a little extra work here
(define (typecheck-with-expr node idname fromexpr toexpr body tenv venv loop?)
  ; first grab the types and make sure they're ok
  (let ([fromty (typecheck fromexpr tenv venv loop?)]
        [toty (typecheck toexpr tenv venv loop?)]
        [venv1 (push-scope venv)]
        [counterty (types:make-VarValue (types:make-IntType))])
    ; make sure types are correct first
    (cond
      [(not (types:type=? (types:make-IntType) fromty))
       (begin
         (log-typeerror "initializing value in with expression must have int type, has ~a type instead"
                        node fromty)
         (types:make-VoidType))]
      [(not (types:type=? (types:make-IntType) toty))
       (begin
         (log-typeerror "final value in with expression must have int type, has ~a type instead"
                        node fromty)
         (types:make-VoidType))]
      ; now work on the rest
      [else
       (begin
         (types:set-VarValue-read-only?! counterty #t)
         (extend-env venv1 idname counterty)
         (let ([bodyty (typecheck body tenv venv1 #t)])
           (cond
             [(not (types:type=? (types:make-VoidType) bodyty))
              (log-typeerror "body of with cannot return a value, returns ~a" node bodyty)]
             [else (types:make-VoidType)])))])))
                   

    


; typecheck function calls
(define (typecheck-funcall node name args tenv venv loop?)
  ; first, look up the function, the funval
  (let* ([funval (apply-env venv name)]
         [params (types:FunValue-parameters funval)]
         [returnty (types:FunValue-return-type funval)])
    ; add some notes since we've found the things we're looking for
    (add-note node 'type returnty)
    (add-note node 'funval funval)
    ; next, make sure args match those in the function
    (if (= (length args) (length params))
        (begin
          (for-each (λ (arg param)
                      (let ([argty (typecheck arg tenv venv loop?)]
                            [paramty (types:NiType-actual param)])
                        (cond
                          [(not (types:type=? argty paramty))
                           (log-typeerror "type mismatch between arguments and formal parameters, saw ~a, expected ~a"
                                          node argty paramty)]))) args params)
          returnty)
        (begin
          (log-typeerror "~a arguments given to ~a, but it requires ~a parameters"
                         node (length params) (length args))
          returnty))))

; typecheck function declarations
(define (typecheck-fun-decls decl tenv venv loop?)
  ; make sure names are equal 
  (if (not (unique-fun-names? decl))
      (begin
        (log-typeerror "function names must be unique in a set of mutually recursive definitions" decl)
        (types:make-VoidType))
      (begin
      ; first walk through and bind the funvalues in the current environment
      (for-each-fun
       (λ (fun)
         (match fun
           ; before we do anything with the body, we have to typecheck a few things,
           ; like the return type and the argument types
           [(FunDecl name args rettype body _)
            (let ([rty (if (eq? rettype #f) (types:make-VoidType) (apply-env tenv rettype))]
                  [params (map (λ (typefield)
                                 ; check to see if the argument type is valid
                                 (let* ([paramname (TypeField-name typefield)]
                                    [paramtype (TypeField-kind typefield)]
                                    [argty (apply-env tenv paramtype)])
                               (if (eq? argty #f)
                                   (begin
                                     (log-typeerror "~a is an unknown argument type" fun paramtype)
                                     (types:NameTypePair (types:make-VoidType) paramname))
                                   ; otherwise, if argty is not false, it's a valid type, so we can create a name/type pair
                                   (types:NameTypePair argty paramname)))) args)])
            (cond
              [(eq? rty #f) (log-typeerror "~a is an unknown return type for your function" fun rettype)]
              ; now extend the environment
              [else (extend-env venv name (types:FunValue params rty))]))]
       [_ (error "Expected FunDecl")])) decl)
      
  ; now walk through a second time to evaluate their bodies and make sure they are ok
  (for-each-fun
   (λ (fun)
     (match fun
       [(FunDecl name args rettype body _)     
        (let ([tenv1 (push-scope tenv)]
              [venv1 (push-scope venv)]
              [funty (apply-env venv name)])
          (for-each (λ (param)
                      ; now extend the new environment with VarValues of the parameters of the function
                      (extend-env venv1 (types:NameTypePair-name param) (types:make-VarValue (types:NiType-actual param)))) (types:FunValue-parameters funty))
          (let ([bodyty (typecheck body tenv1 venv1 #f)]
                [retty (types:FunValue-return-type funty)])
            (cond
              [(not (types:type=? (types:FunValue-return-type funty) bodyty))
               (log-typeerror "Specified function return type, ~a, does not match body return type, ~a" fun (types:FunValue-return-type funty) bodyty)])))]
       [_ (error "Compiler error: expected FunDecl")])) decl))))
                    
            
                         
          



; check bool expressions
(define (typecheck-boolexprs ast e1 op e2 tenv venv loop?)
  (let ([t1 (types:actual-type (typecheck e1 tenv venv loop?))]
        [t2 (types:actual-type (typecheck e2 tenv venv loop?))])
    (cond
      ; first, void types can't be compared
      [(or (types:VoidType? t1) (types:VoidType? t2))
       (begin (log-typeerror "cannot compare non-value types (void)" ast) (types:make-VoidType))]
      
      ; next, we can use <, <=, >=, and > only on strings and ints
      [(or (eq? op 'lt)
           (eq? op 'gt)
           (eq? op 'ge)
           (eq? op 'le))
       (if (types:type=? t1 t2)
           (if (or (types:IntType? t1)
                   (types:StringType? t2))
               (types:make-BoolType)
               (log-typeerror "<, <=, >=, and > can only be used on strings and ints for comparison, ~a seen instead" ast t1))
           (log-typeerror "<, <=, >=, and > must compare the same types (on strings and ints only)" ast))]
      
      ; the remaining, <> and = can be used also on records and arrays, but we still must have the same type
      ; first we look for records and their types
      [(or (eq? op 'eq)
           (eq? op 'ne))
       ; types are equal, but we can't compare peng
       (let ([typesequal? (and (types:type=? t1 t2) (not (types:PengType? t1)))]
             [pengequal? (peng-match? t1 t2)])
         (if (or pengequal? typesequal?)
             ; true branch
             (types:make-BoolType)
             
             ; false branch: if it wasn't a record and comparing to peng, we just check for type equality
             (begin (log-typeerror "equality and inequality can only be used with the same types (excluding peng)" ast)
                    (types:make-VoidType))))]
      
      [else (error "unexpected comparator operator")])))
  
 
; test if a field name is part of a record
(define (find-field recordty name)
  (findf (lambda (field)
           (let ([fname (types:NameTypePair-name field)])
             (if (eq? fname name)
                 #t
                 #f))) (types:RecordType-fields recordty)))

; walk through lists of assignments in a record and name type pairs in its type definition
; and make sure names and expressions match, return true or false if they all do
(define (fieldassigns-correct? node assignments nametypepairs tenv venv loop?)
  (cond
    ; if both lists are empty, then we match!
    [(and (empty? assignments) (empty? nametypepairs)) #t]
    [else (let* ([assign (first assignments)]
                 [ntpair (first nametypepairs)]
                 [etype (typecheck (FieldAssign-expr assign) tenv venv loop?)]
                 [fieldname (FieldAssign-name assign)]
                 [ntpairname (types:NameTypePair-name ntpair)]
                 [ntpairtype (types:actual-type (types:NiType-actual ntpair))])
            ; symbol names must be the same
            (if (eq? fieldname ntpairname)
                ; types must match too
                (cond
                  [(or (types:type=? etype ntpairtype) (peng-match? etype ntpairtype))
                   (fieldassigns-correct? node (rest assignments) (rest nametypepairs) tenv venv loop?)]
                  [else (begin (log-typeerror "types do not match for record field (~a) and expression type (~a)" node (types:NameTypePair-name ntpair)
                                              etype) #f)])
                (begin (log-typeerror "names and order of field assignments must match record declaration" node) #f)))]))
                

(define (peng-match? t1 t2)
  (cond
    [(and (types:PengType? t1) (types:RecordType? t2)) #t]
    [(and (types:PengType? t1) (types:ArrayType? t2)) #t]
    [(and (types:RecordType? t1) (types:PengType? t2)) #t]
    [(and (types:ArrayType? t1) (types:PengType? t2)) #t]
    [else (begin (printf "t1: ~a, t2: ~a~n" t1 t2) #f)]))

; handle typechecking of record creation
(define (typecheck-record-creation node name assignments tenv venv loop?)
  ; this should get us a record type
  (let ([recty (types:actual-type (apply-env tenv name))])
    (cond
      [(types:RecordType? recty)
       (if (eq? (fieldassigns-correct? node assignments (types:RecordType-fields recty) tenv venv loop?) #t)
           recty
           (types:make-VoidType))]
      [else (begin
              (log-typeerror "~a must refer to a record type" node name)
              (types:make-VoidType))])))

; handle typechecking of any type of declarations, names, arrays and records
(define (typecheck-type-decls decl tenv)
  ; first, make sure we don't have any repeating names
  (cond
    [(eq? #f (unique-type-names? decl))
     (log-typeerror "Type names in a set of mutually recursive definitions cannot repeat")]
    [else
     ; next, bind all the names to name types so we can typecheck their fields
     (begin
       (for-each-type (λ (type)
                        (let ([tyname (match type
                                        [(NameType name _ _) name]
                                        [(ArrayType name _ _) name]
                                         [(RecordType name _ _) name])])
                           ; we walk through each type and create a binding of the
                           ; name to an empty type initially, then later we can mutate
                           ; its binding to the actual type
                           (if (types:base-type-name? tyname)
                               (log-typeerror "You cannot redefine the base type: ~a" decl tyname)
                               (extend-env tenv tyname (types:make-NameType '() tyname)))))
                       decl)
        
        ; now, that we have all the names, it's time for mutation, so we'll walk through all the
        ; declarations again using a for-each-type
        (for-each-type (λ (type)
                         (match type
                           [(NameType name kind _)
                            ; look the name back up from the environment, so we'll be sure to
                            ; pick up its struct, then figure out the type we specified (it
                            ; should be here now in the tenv), and bind to it
                            (let ([namety (apply-env tenv name)]
                                  [ty (apply-env tenv kind)])
                              (if (eq? ty #f)
                                  (log-typeerror "undefined type named ~a in type alias, did you mean to include mutual recursion?" type kind)
                                  (types:bind-nametype! namety ty)))]
                           
                           ; we'll do a similar thing for arrays
                           [(ArrayType name kind _)
                            (let ([namety (apply-env tenv name)]
                                  [ty (apply-env tenv kind)])
                              (types:bind-nametype! namety (types:make-ArrayType name ty)))]

                           ; and finally for records, which is slightly more complicated
                           [(RecordType name fields _)
                            (let ([recordty (apply-env tenv name)]
                                  ; use map to create a list from the typefields to nametypepairs
                                  [recfields (map (λ (field)
                                                    (let* ([fieldname (TypeField-name field)]
                                                           [tyname (TypeField-kind field)]
                                                           [fieldtype (apply-env tenv tyname)])
                                                      (if (eq? fieldtype #f)
                                                          ; field type name wasn't found in the env
                                                          (begin (log-typeerror "Unknown field type: ~a" type tyname)
                                                                 (types:NameTypePair (types:make-VoidType) fieldname))
                                                          (types:NameTypePair fieldtype fieldname)))) fields)])
                              (types:bind-nametype! recordty (types:make-RecordType name recfields)))]))
                       decl)

        (for-each-type (λ (type)
                         (match type
                           [(NameType name kind _)
                            ; grab the actual type, and look for a cycle
                            (let ([ty (apply-env tenv kind)])
                              (cond
                                [(and (types:NameType? ty) (types:name-cycle? ty))
                                 (log-typeerror "You cannot have a mutually recursive set of name types that only refer to each other" decl)]
                                [(and (types:NameType? ty) (types:names-only? ty))
                                 ; this case would really only occur if you had a bad type, it's okay for the mutually
                                 ; recursive types to go through a base type in the end, not just records and arrays
                                 ; because then we can resolve them! see test16.ni, which should fail, but for different
                                 ; reasons than this--primarily that c isn't defined 
                                 (log-typeerror "Mutually recursive types must pass through records or arrays or base types" type)]))]
                           [_ '()])) decl)
        )]))
       
                           

; figure out if our list of mutually recursive fundecls is unique in name
(define (unique-fun-names? fundecls)
  (let ([namelist (map-fun (λ (decl)
                             (match decl
                               [(FunDecl name _ _ _ next) name]
                               [else (raise-arguments-error 'unique-fun-names? "must pass a fundecl to unique-fun-names?"
                                                            "fundecls" fundecls)])) fundecls)])
    (letrec ([no-duplicates? (λ (lst)
                               (cond
                                 [(empty? lst) #t]
                                 [(not (eq? (member (first lst) (rest lst)) #f)) #f]
                                 [else (no-duplicates? (rest lst))]))])
      (no-duplicates? namelist))))
                              

; ensure that a set of mutually recursive type definitions do not have repeated names
(define (unique-type-names? typedecl)
  ; now use map-type to make the list of names
  (letrec ([namelist (map-type 
                      (λ (ty)
                        (cond
                          [(RecordType? ty) (RecordType-name ty)]
                          [(NameType? ty) (NameType-name ty)]
                          [(ArrayType? ty) (ArrayType-name ty)])) typedecl)]
           ; then define a function to test to see if there are duplicates in the list of names
           [no-duplicates? (λ (lst)
                             (cond
                               [(empty? lst) #t]
                               [(member (first lst) (rest lst)) #f]
                               [else (no-duplicates? (rest lst))]))])
    ; and execute this function on the list of names we created
    (no-duplicates? namelist)))


; version of for-each to walk through function lists
(define (for-each-fun fun fundecl)
  (if (empty? fundecl)
      '()
      (begin (fun fundecl)
             (for-each-fun fun (FunDecl-next fundecl)))))

; version of for-each to walk through the list of types
(define (for-each-type fun type)
  (let ([next (cond
                [(RecordType? type) (RecordType-next type)]
                [(NameType? type) (NameType-next type)]
                [(ArrayType? type) (ArrayType-next type)]
                [else (error "ty must be a RecordType, NameType or ArrayType")])])
    (if (empty? next)
        (fun type)
        (begin
          (fun type)
          (for-each-type fun next)))))

; version of map for types
(define (map-type fun type)
  (let ([next
         (cond [(RecordType? type) (RecordType-next type)]
               [(NameType? type) (NameType-next type)]
               [(ArrayType? type) (ArrayType-next type)]
               [else "ty must be a RecordType, NameType or ArrayType"])])
    (if (empty? next)
        (list (fun type))
        (cons (fun type) (map-type fun next)))))

; map version for functions
(define (map-fun fun fundecl)
  (if (empty? fundecl)
      '()
      (let ([next (FunDecl-next fundecl)])
        (cons (fun fundecl) (map-fun fun next)))))

;(check-expect (unique-type-names? (RecordType 'foo '() '())) #t)
;(check-expect (unique-type-names? (ArrayType 'foo '() '())) #t)
;(check-expect (unique-type-names? (NameType 'foo '() '())) #t)
;(check-expect (unique-type-names? (RecordType 'foo '() (RecordType 'bar '() (RecordType 'baz '() '())))) #t)
;(check-expect (unique-type-names? (RecordType 'foo '() (RecordType 'bar '() (RecordType 'foo '() '())))) #f)
;(check-expect (unique-type-names? (ArrayType 'foo '() (ArrayType 'bar '() (ArrayType 'baz '() '())))) #t)
;(check-expect (unique-type-names? (ArrayType 'foo '() (ArrayType 'bar '() (ArrayType 'foo '() '())))) #f)
;(check-expect (unique-type-names? (NameType 'foo '() (NameType 'bar '() (NameType 'baz '() '())))) #t)
;(check-expect (unique-type-names? (NameType 'foo '() (NameType 'bar '() (NameType 'foo '() '())))) #f)
