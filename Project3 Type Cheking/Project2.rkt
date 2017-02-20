#lang racket

;Tan Zhen 872692777
;Project2 Parsing
;use command-line flag -r to rewrite "with-loop" as "while loop"

(require "Project1.rkt"
         (prefix-in lex: parser-tools/lex)
         parser-tools/cfg-parser
         parser-tools/yacc)
(require test-engine/racket-tests)

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
; peng expression
(struct PengExpr () #:transparent)
; with expression (think: for expression)
(struct WithExpr (idname initexpr fromexpr toexpr) #:transparent)
;---------------------------------------------------------------------
; struct for comment
;(struct LineComment (word) #:transparent)
;(struct BlockComment (word) #:transparent)

(define rewrite-with (make-parameter false))

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
     ;[(reccomm expr) (list $1 $2)]
     )

    (expr
     [(token) $1]
     [(vardecl) $1]
     [(typedecl) $1]
     [(rctypedecl) $1]
     [(fundecl) $1]
     [(lvalue) $1]
     [(funcall) $1]
     [(letexpr) $1]
     [(sqexpr) $1]
     [(MathEx) $1]
     [(LogicEx) $1]
     [(BoolEx) $1]
     [(newrec) $1]
     [(newarr) $1]
     [(AssignEx) $1]
     [(ifEx) $1]
     [(whileEx) $1]
     [(withEx) $1]
     [(breakEx) $1]
     [(pengEx) $1]
     ;[(reccomm) $1]
     ;[(reccomm expr) (if (list? $2) (cons $1 $2) (list $1 $2))]
     )
    ;sequence of expressions
    (sqexpr
     [(LPAREN Exprs RPAREN) $2]
     )
    (Exprs
     [(expr SEMI expr) (list $1 $3)]
     [(expr SEMI Exprs) (cons $1 $3)]
     )
    
    (token
     [(NUM) (NumExpr $1)] 
     [(STRING)(StringExpr $1)]
     [(ID) (VarExpr $1)]
     [(noval) $1]
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
        [(RecordType name field next) (RecordType name field $3)]
        [(ArrayType name kind next) (ArrayType name kind $3)]
        )]
     )
    (typedecl
     [(DEFINE ID KIND AS ID) (NameType $2 $5 '())]
     [(DEFINE ID KIND AS LBRACE typefiels RBRACE) (RecordType $2 $6 '())]
     [(DEFINE ID KIND AS LBRACE RBRACE) (RecordType $2 '() '())]
     [(DEFINE ID KIND AS ARRAY OF ID) (ArrayType $2 $7 '())]
     )
    (typefiels
     [(typeEx) (list $1)]
     [(typeEx COMMA typefiels) (cons $1 $3)]
     )
    (typeEx
     [(ID ID) (TypeField $2 $1)]
     )
    ;3.Function declarations
    (fundecl
     [(NEEWOM ID LPAREN typefiels RPAREN IS expr) (FunDecl $2 $4 '() $7 '())]
     [(NEEWOM ID LPAREN typefiels RPAREN AS ID IS expr) (FunDecl $2 $4 $7 $9 '())]
     [(NEEWOM ID LPAREN typefiels RPAREN IS expr AND fundecl) (FunDecl $2 $4 '() $7 $9)]
     [(NEEWOM ID LPAREN typefiels RPAREN AS ID IS expr AND fundecl) (FunDecl $2 $4 $7 $9 $11)]
     ;no arguments
     [(NEEWOM ID LPAREN RPAREN IS expr) (FunDecl $2 '() '() $6 '())]
     [(NEEWOM ID LPAREN RPAREN AS ID IS expr) (FunDecl $2 '() $6 $8 '())]
     [(NEEWOM ID LPAREN RPAREN IS expr AND fundecl) (FunDecl $2 '() '() $6 $8)]
     [(NEEWOM ID LPAREN RPAREN AS ID IS expr AND fundecl) (FunDecl $2 '() $6 $8 $10)]
     )
     #|(rcfundecl
     [(fundecl) $1]
     [(fundecl AND rcfundecl)
      (match $1
        [(FunDecl name args rettype body next) (FunDecl name args rettype body $3)]
       )]
     )|#
    
    ;4.1 Local variables
    (lvalue
     [(token) $1]
     [(lvalue DOT ID) (RecordExpr $1 $3)]
     [(lvalue LBRACKET expr RBRACKET) (ArrayExpr $1 $3)]
     )
    ;4.2 function call
    (funcall
     [(ID LPAREN RPAREN) (FuncallExpr $1 #f)]
     [(ID LPAREN args RPAREN) (FuncallExpr $1 $3)]
     )
    (args
     [(expr) (list $1)]
     [(expr COMMA args) (cons $1 $3)]
     )
    ;4.3 no value
    (noval
     [(LPAREN RPAREN) (NoVal)]
     )
    ;4.4 let expressions
    (letexpr
     [(LET declarations IN END) (LetExpr $2 '())]
     [(LET declarations IN expr END) (LetExpr $2 (list $4))]
     [(LET declarations IN Exprs END) (LetExpr $2 $4)]
     )
    (declarations
     [(decl) (list $1)]
     ;[(comm decl) (list $1 $2)]
     ;[(comm declarations) (cons $1 $2)]
     [(decl declarations) (cons $1 $2)]
     )
    (decl
     [(vardecl) $1]
     [(fundecl) $1]
     [(rctypedecl) $1]
     )
    ;4.5 Math Expressions
    (MathEx
     [(MathEx ADD MathTerm) (MathExpr $1 '+ $3)]
     [(MathEx SUB MathTerm) (MathExpr $1 '- $3)]
     [(SUB MathEx) (MathExpr (NumExpr "0") '- $2)]
     [(MathTerm) $1]
     )
    (MathTerm
     [(MathTerm MULT MathFact) (MathExpr $1 '* $3)]
     [(MathTerm DIV MathFact) (MathExpr $1 '/ $3)]
     [(MathFact) $1]
     )
    (MathFact
     ;[(NUM DOT NUM) (MathExpr $1 #\. $3)]
     [(LPAREN MathEx RPAREN) $2]
     ;[(expr) $1]
     [(token) $1]
     )
    ;4.6 Bool Expressions
    (BoolEx
     [(BoolEx EQ BoolTerm) (BoolExpr $1 'eq $3)]
     [(BoolEx NE BoolTerm) (BoolExpr $1 '<> $3)]
     [(BoolEx LT BoolTerm) (BoolExpr $1 '< $3)]
     [(BoolEx GT BoolTerm) (BoolExpr $1 '> $3)]
     [(BoolEx LE BoolTerm) (BoolExpr $1 '<= $3)]
     [(BoolEx GE BoolTerm) (BoolExpr $1 '>= $3)]
     [(BoolTerm) $1]
     )
    (BoolTerm
     [(LPAREN BoolEx RPAREN) $2]
     [(LogicEx) $1]
     )

    ;4.7 Logic Expressions
    (LogicEx
     [(LogicEx BOOLOR LogicTerm) (LogicExpr  $1 'or $3)]
     [(LogicEx BOOLAND LogicTerm) (LogicExpr  $1 'and $3)]
     [(LogicTerm) $1]
     )
    (LogicTerm
     [(LPAREN LogicEx RPAREN) $2]
     [(MathEx) $1]
     )
    ;5.1 creating a new record
    (newrec
     [(ID LBRACE RBRACE) (NewRecordExpr $1 #f)]
     [(ID LBRACE fieldassigns RBRACE) (NewRecordExpr $1 $3)]
     )
    (fieldassigns
     [(fieldassign) (list $1)]
     [(fieldassign COMMA fieldassigns) (cons $1 $3)]
     )
    (fieldassign
     [(ID IS expr) (FieldAssign $1 $3)]
     )
    ;5.2 creating a new array
    (newarr
     [(ID LBRACKET expr RBRACKET OF expr) (NewArrayExpr $1 $3 $6)]
     )
    ;6 Assignment expression
    (AssignEx
     [(NOW lvalue IS expr) (AssignmentExpr $2 $4)]
     )
    ;7 If expression
    (ifEx
     [(IF expr THEN expr END) (IfExpr $2 $4 #f)]
     [(IF expr THEN expr ELSE expr END) (IfExpr $2 $4 $6)]
     )
    ;8 While expression
    (whileEx
     [(WHILE expr DO expr END) (WhileExpr $2 $4)]
     )
    ;9 with expression
    (withEx
     [(WITH ID AS expr TO expr DO expr END)
      (if (rewrite-with) ;test
          ;then
          (let ([body (list* $8 (list (MathExpr (VarExpr $2) '+ (NumExpr "1"))))])
            (LetExpr (list (VarDecl "int" $2 $4))
                     (list (WhileExpr (BoolExpr (VarExpr $2) '<= $6) body))))
          ;else
          (WithExpr $2 $4 $6 $8))]
     )
    ;10 break expression
    (breakEx
     [(BREAK) (BreakExpr)]
     )
    ;11 peng expression
    (pengEx
     [(PENG) (PengExpr)]
     )
    ;12 comment
    #|(reccomm
     [(comm) $1]
     [(comm reccomm) (if (list? reccomm) (cons $1 $2) (cons $1 (list $2)))]
     )
    (comm
     [(BCOMMENT) (BlockComment $1)]
     [(LCOMMENT) (LineComment $1)]
     )|#
    
    )
   
   ))

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

;-------------------------- main ------------------------------------
#|(define read-file
  (command-line
   #:once-each
   [("-r") "rewrite with-loop as while"
                        (rewrite-with true)]
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))

(provide main)
(define (main filename)
  (let
      ([lst (parse-file filename)])
    (write lst)
  (printf "Pass Test ~a\n" filename)))

(main read-file)|#


;(make-dot-file (parse-str "5") "out.dot")
;Get-Childitem .\tests\ -name .\*.ni | Foreach {.\Project2.exe .\tests\$_}