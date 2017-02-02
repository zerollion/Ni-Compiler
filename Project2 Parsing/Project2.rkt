#lang racket

;Tan Zhen 872692777
;Project2 Parsing

(require "Project1.rkt"
         (prefix-in lex: parser-tools/lex)
         parser-tools/cfg-parser
         parser-tools/yacc)

(provide (all-defined-out))

;--------------structs defined by Chris-----------------
; var declarations
(struct VarDecl (type id expr) #:transparent)

; type declarations--note they can be mutually recursive (using AND)
; so our struct has a link to the next one that belongs here, otherwise
; it's simply '()
(struct NameType (name kind next) #:transparent)
(struct RecordType (name fields next) #:transparent)
(struct ArrayType (name kind next) #:transparent)
(struct TypeField (name kind) #:transparent)

; defines a function in ni
; these consist of the name of the function, the arguments to it,
; the return type (which may be #f if it doesn't have one) and the body
; finally, next points to the next, related definition (for mutual recursion)
(struct FunDecl (name args rettype body next) #:transparent)

; things associated with expressions and lvalues
(struct NumExpr (val) #:transparent)
; variable expressions
(struct VarExpr (name) #:transparent)
; record expressions (name and a list of fields are required)
(struct RecordExpr (name field) #:transparent)
; array expressions (name and expression for the index)
(struct ArrayExpr (name expr) #:transparent)
; function call which is name and a list of arguments
(struct FuncallExpr (name args) #:transparent)
; a string
(struct StringExpr (str) #:transparent)
; a noval 
(struct NoVal () #:transparent)
; a list of declarations for the let and a list of expressions following it
(struct LetExpr (decs exprs) #:transparent)
; arithmetic expression
(struct MathExpr (expr1 op expr2) #:transparent)
; bool op, i.e., comparision
(struct BoolExpr (expr1 op expr2) #:transparent)
; logic op, and or or
(struct LogicExpr (expr1 op expr2) #:transparent)
; assignment in a field for creating a record
(struct FieldAssign (name expr) #:transparent)
; creating a new record
(struct NewRecordExpr (name assignments) #:transparent)
; creating a new array
(struct NewArrayExpr (name expr kind) #:transparent)
; an if expression (hint, you may temporarily use an IfElseExpr if you
; would like to make it easy to see when you're matching or not
(struct IfExpr (test true-branch false-branch) #:transparent)
; a while expression, which is a test and the body
(struct WhileExpr (test body) #:transparent)
; an assignment expression
(struct AssignmentExpr (name expr) #:transparent)
; break expression--this has no arguments
(struct BreakExpr () #:transparent)
; with expression (think: for expression)
(struct WithExpr (idname initexpr fromexpr toexpr) #:transparent)
;---------------------------------------------------------------------

(define aparser
  (cfg-parser
   (src-pos)
   (start program)
   (end EOF)
   (tokens value-tokens paren-types operators punctuation comparators boolops keywords endoffile)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
                (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                        (lex:position-line start-pos) (lex:position-col start-pos) tok-name tok-value tok-ok?))))
   (grammar

    (program
     [(expr) (list $1)]
     [(expr program) (cons $1 $2)]
     )

    (expr
     [(token) $1]
     [(MathEx) $1]
     [(vardecl) $1]
     [(typedecl) $1]
     [(rctypedecl) $1]
     )
    
    (token
     [(NUM) (NumExpr $1)]
     [(STRING)(StringExpr $1)]
     [(ID) (VarExpr $1)]
     )

    ;1.Varaible Declarations
    (vardecl
     [(NI ID IS expr) (VarDecl #f $2 $4)]
     [(NI ID ID IS expr) (VarDecl $2 $3 $5)]
     )
    ;2.Data type Declarations
    (rctypedecl
     [(typedecl) $1]
     [(typedecl AND rctypedecl)
      (match $1
        [(NameType name kind next) (NameType name kind $3)]
        [(RecordType name kind next) (RecordType name kind $3)]
        [(ArrayType name kind next) (ArrayType name kind $3)]
        )]
     )
    (typedecl
     [(DEFINE ID KIND AS ID) (NameType $2 $5 #f)]
     [(DEFINE ID KIND AS LBRACE typefiels RBRACE) (RecordType $2 $6 #f)]
     [(DEFINE ID KIND AS ARRAY OF ID) (ArrayType $2 $7 #f)]
     )
    (typefiels
     [(typeEx) (list $1)]
     [(typeEx COMMA typefiels) (cons $1 $3)]
     )
    (typeEx
     [(ID ID) (TypeField $1 $2)]
     )
    
    ;Math Expressions
    (MathEx
     [(MathEx ADD MathTerm) (MathExpr $1 '+ $3)]
     [(MathEx SUB MathTerm) (MathExpr $1 '- $3)]
     [(SUB MathEx) (MathExpr (NumExpr 0) '- $2)]
     [(MathTerm) $1]
     )
    (MathTerm
     [(MathTerm MULT MathFact) (MathExpr $1 '* $3)]
     [(MathTerm DIV MathFact) (MathExpr $1 '/ $3)]
     [(MathFact) $1]
     )
    (MathFact
     [(NUM DOT NUM) (MathExpr $1 #\. $3)]
     [(token) $1]
     [(LPAREN MathEx RPAREN) $2]
     )
    ;-----------------------------------------
    
    )
   
   ))

;(check-expect (parse-str "pt.x")(list (RecordExpr (VarExpr "pt") "x")))

;utiliy functions
(define (lex-procedure in)
  (lambda () (alexer in)))

(define (parse-str str)
  (let ([in (open-input-string str)])
    (port-count-lines! in)
    (aparser (lex-procedure in))))

(define (parse-file filename)
  (let ([in (open-input-file filename)])
    (port-count-lines! in)
    (aparser (lex-procedure in))))