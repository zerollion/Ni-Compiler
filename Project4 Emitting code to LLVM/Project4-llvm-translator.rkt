#lang racket

(require "Project4-llvm-emitter.rkt"
         "names.rkt"
         (prefix-in t: "types.rkt")
         "niparser.rkt"
         "typecheck.rkt"
         "errors.rkt"
         "log.rkt")

(provide (all-defined-out))

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
               (finish-emission)))))))

  
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

(define (ast->llvm ast)
  (match ast
    ; deal with lists, like in let expressions
    ['() '()]
    [(cons first rest) (begin (ast->llvm first) (ast->llvm rest))]
    
    ; integer literals
    [(NumExpr val) (numexpr->llvm ast val)]

    [(StringExpr val) (stringexpr->llvm ast val)]

    ; variable declarations!
    [(VarDecl _ _ _) (vardecl->llvm ast)]
    
    ; function calls
    [(FuncallExpr _ _) (funcall->llvm ast)]
       
    ; variable expressions
    [(VarExpr _) (var->llvm ast)]

    ; let expressions--need these for any declarations to work!
    [(LetExpr _ _) (letexpr->llvm ast)]

    ; variable assignment expression
    [(AssignmentExpr _ _) (assignexpr->llvm ast)]

    ; if expression/branching
    [(IfExpr _ _ _) (ifexpr->llvm ast)]
    
    [_ (error "Translation node " ast " not implemented yet!")]))        

; emits a numeric literal
(define (numexpr->llvm node val)
  ; literal nums can go in registers
  (let ([result (emit-math 'add val "0")])
    (add-note node 'result result)))

;emits a string
(define (stringexpr->llvm node val)
  (let ([result (emit-literal-string val)])
    (add-note node 'result result)))

;emits variable declaration
(define (vardecl->llvm node)
  (let* ([temp (ast->llvm (VarDecl-expr node))]
         [type (get-note (VarDecl-expr node) 'type)]
         [noderes (get-note (VarDecl-expr node) 'result)]
         [result (emit-vardecl (VarDecl-id node) type noderes)]
         [varvalue (get-note node 'type)])
    (t:set-VarValue-result! varvalue result)
    (add-note node 'result result)
    ))

;emit variable expression
(define (var->llvm node)
  (let* ([varval (get-note node 'var)]
         [noderes (t:VarValue-result varval)]
         [result (emit-var varval noderes)])
    (add-note node 'result result)
    ))

;emits function call expression
(define (funcall->llvm node)
  (let* ([name (FuncallExpr-name node)]
         [listarg (FuncallExpr-args node)]
         [rettype (get-note node 'type)])
    (for-each (λ (arg) (ast->llvm arg)) listarg)
    (let* ([nodelist (map (λ (arg) (get-note arg 'result)) listarg)]
           [typelist (map (λ (arg) (get-note arg 'type)) listarg)]
           [result (emit-funcall name nodelist typelist rettype)])
      (add-note node 'result result)
    )))

;emit let expression
(define (letexpr->llvm node)
  (let ([decs (LetExpr-decs node)]
        [exprs (LetExpr-exprs node)])
    (for-each (λ (arg) (ast->llvm arg)) decs)
    (for-each (λ (arg) (ast->llvm arg)) exprs)
    (let* ([result (foldl (λ (arg default) (get-note arg 'result)) '() exprs)])
      (add-note node 'result result)
      )))

;emit assignment expression
(define (assignexpr->llvm node)
  (let* ([temp (ast->llvm (AssignmentExpr-expr node))]
         [exprnode (get-note (AssignmentExpr-expr node) 'result)]
         [varval (get-note (AssignmentExpr-name node) 'var)]
         [varnode (t:VarValue-result varval)]
         [result (emit-assignexpr varval varnode exprnode)])
    (add-note node 'result result)
    ))

;emit if expression/branching
;IfExpr (test true-branch false-branch)
(define (ifexpr->llvm)
  '()
  )