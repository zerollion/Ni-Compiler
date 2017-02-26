#lang racket

;Typr checker
;Tan Zhen

(require "NIparser.rkt"
         "environment.rkt"
         "errors.rkt"
         "log.rkt"
         (prefix-in types: "types.rkt")
         test-engine/racket-tests)

(provide (all-defined-out))

(define (typecheck ast env)
  (match ast
    ; the base case for lists, this will just be the void type
    ['() (types:make-VoidType)]
    [(NoVal) (types:make-VoidType)]

    ; base cases for types
    [(NumExpr val) (types:make-IntType)]
    [(StringExpr v) (types:make-StringType)]
     
    [(VarExpr name) (let ([t1 (apply-env env name)])
                      (if (eq? t1 #f)
                          (log-typeerror "variable not declared using 'ni' before use" ast)
                          t1))]

    ;record types -----------------------------------------P1
    [(NameType name kind next)
     (let ([t1 (apply-env env kind)])
       (if (eq? t1 #f)
           (log-typeerror "NameType: declared type does not exist." ast)
           (extend-env env name (types:NameType kind)))
        )]

    [(ArrayType name kind next)
     (let ([t1 (apply-env env kind)])
       (if (eq? t1 #f)
           (log-typeerror "ArrayType: declared type does not exist." ast)
           (extend-env env name (types:ArrayType '() t1)))
        )]

    [(TypeField name kind)
     (let ([t1 (apply-env env kind)])
       (if (eq? t1 #f)
           (log-typeerror "TypeField: declared type does not exist." ast)
           (extend-env env name (types:NameTypePair t1 name)))
        )]

    [(RecordType name field next)
     (let ([t2 (map (λ (arg) (typecheck arg env)) field)])
       (extend-env env name (types:RecordType t2 name))
       )]

    ;function decl

    ;Bool types
    [(BoolVal expr1) (types:make-BoolType)]
    [(BoolExpr expr1 op expr2)
     (let ([e1 (typecheck expr1 env)] [e2 (typecheck expr2 env)])
       (if (and (eq? e1 (types:make-IntType)) (eq? e2 (types:make-IntType)))
       (types:make-BoolType)
       (log-typeerror "Bool Exprssion contains types other than int." ast))
       )]
    
    [(LogicExpr expr1 op expr2)
     (let ([e1 (typecheck expr1 env)] [e2 (typecheck expr2 env)])
       (if (and (eq? e1 (types:make-BoolType)) (eq? e2 (types:make-BoolType)))
       (types:make-BoolType)
       (log-typeerror "Logic Exprssion contains types other than bool." ast))
       )]

    ;math expressions
    [(MathExpr expr1 op expr2)
     (let ([e1 (typecheck expr1 env)] [e2 (typecheck expr2 env)])
       (if (and (eq? e1 (types:make-IntType)) (eq? e2 (types:make-IntType)))
       (types:make-IntType)
       (log-typeerror "Math Exprssion contains types other than int." ast))
       )]
    
    [(MathExpr expr1 op expr2) (types:make-IntType)]

    ;unbounded cases
    [_ (begin
         (displayln ast)
         (error "Node not implemented yet!"))]
  ))

;utility functions
(define (tc-str str)
  (let ([env (empty-env)])
    (extend-env env 'int (types:make-IntType))
    (extend-env env 'string (types:make-StringType))
    (typecheck (first (parse-str str)) env)))

;---------------------------check expects------------------------------
;;; checking expressions

; noval
(check-expect (tc-str "()") (types:make-VoidType))

; first, simple integer expressions of various sorts
(check-expect (tc-str "5") (types:make-IntType))
(check-expect (tc-str "5+3") (types:make-IntType))
(check-expect (tc-str "5<3") (types:make-BoolType))
(check-expect (tc-str "5>3") (types:make-BoolType))
(check-expect (tc-str "5<=3") (types:make-BoolType))
(check-expect (tc-str "5>=3") (types:make-BoolType))
(check-expect (tc-str "5=3") (types:make-BoolType))
(check-expect (tc-str "5<>3") (types:make-BoolType))

; now test strings
(check-expect (tc-str "\"hello\"") (types:make-StringType))
(check-expect (tc-str "\"hi\"<\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\">\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\"<=\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\">=\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\"<>\"hello\"") (types:make-BoolType))
(check-expect (tc-str "\"hi\"=\"hello\"") (types:make-BoolType))

;(check-error (tc-str "5 = \"5\""))
;(check-error (tc-str "5 >= \"5\""))

; bool types and logic expressions
(check-expect (tc-str "true") (types:make-BoolType))
(check-expect (tc-str "false") (types:make-BoolType))
(check-expect (tc-str "true & true") (types:make-BoolType))
(check-expect (tc-str "true | true & false") (types:make-BoolType))

;(check-error (tc-str "true & 1"))
(check-expect (tc-str "true & (5+3 < 6)") (types:make-BoolType))

(test)