#lang racket

(require parser-tools/cfg-parser
         parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "errors.rkt"
         "nilexer.rkt")

(provide (all-defined-out))


; build up a set of annotations on the nodes, this separates out
; this info so that we can more easily write check-expects on the
; correct structure of the AST--note we use a hasheq instead of a
; hash because we need to know just exactly if two objects are eq?
; not equal? (the later would be structure equality, and we
; need to map information per object)
(define annotations (make-parameter (make-hasheq)))

; we can add notes to a node with a symbol and a value
; generally, the sym should be something like 'position, or 'type
(define (add-note node sym item)
  (let ([notes (if (hash-has-key? (annotations) node)
                (hash-ref (annotations) node)
                (let ([newhash (make-hash)])
                  (hash-set! (annotations) node newhash)
                  newhash))])
    (hash-set! notes sym item)))

; this retrieves a note on a node from the annotations 
(define (get-note node sym)
  (if (hash-has-key? (annotations) node)
      (let ([node-ht (hash-ref (annotations) node)])
        (if (hash-has-key? node-ht sym)
            (hash-ref node-ht sym)
            (let ([errorstr (open-output-string)])
              (fprintf errorstr "Unable to find ~a as an annotation for ~a~n"
                       sym node)
              (error (get-output-string errorstr)))))
        (let ([errorstr (open-output-string)])
          (fprintf errorstr "Unable to find any (including ~a) annotations for ~a~n"
                   sym node)
          (error (get-output-string errorstr)))))
                                

(define (has-note? node sym)
  (if (hash-has-key? (annotations) node)
      (let ([node-ht (hash-ref (annotations) node)])
        (hash-has-key? node-ht sym))
      #f))
  

; var declarations
(struct VarDecl (type id expr) #:transparent)

; type declarations--note they can be mutually recursive (using AND)
; so our struct has a link to the next one that belongs here, otherwise
; it's simply '()
(struct NameType (name kind next) #:transparent
  #:guard (λ (name kind next tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym kind next))
            ))
(struct RecordType (name fields next) #:transparent
  #:guard (λ (name fields next tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym fields next))
            ))
(struct ArrayType (name kind next) #:transparent
  #:guard (λ (name kind next tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym kind next))
            ))
(struct TypeField (name kind) #:transparent
  #:guard (λ (name kind tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym kind))
            ))

; defines a function in ni
; these consist of the name of the function, the arguments to it,
; the return type (which may be #f if it doesn't have one) and the body
; finally, next points to the next, related definition (for mutual recursion)
(struct FunDecl (name args rettype body next) #:transparent
  #:guard (λ (name args rettype body next tyname)
            (let ([sym (if (string? name) (string->symbol name) name)]
                  [retsym (if (string? rettype) (string->symbol rettype) rettype)])
              (values sym args retsym body next))
            ))

; things associated with expressions and lvalues
(struct NumExpr (val) #:transparent)
; variable expressions
(struct VarExpr (name) #:transparent
   #:guard (λ (name tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym))
            ))
; record expressions (name and a list of fields are required)
(struct RecordExpr (name field) #:transparent
   #:guard (λ (name field tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym field))
            ))
; array expressions (name and expression for the index)
(struct ArrayExpr (name expr) #:transparent
   #:guard (λ (name expr tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym expr))
            ))
; function call which is name and a list of arguments
(struct FuncallExpr (name args) #:transparent
   #:guard (λ (name args tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym args))
            ))

; a string
(struct StringExpr (str) #:transparent)
; a bool value, like true or false--this is a literal
(struct BoolVal (val) #:transparent)
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
(struct FieldAssign (name expr) #:transparent
   #:guard (λ (name expr tyname)
             (if (or (symbol? name) (string? name))
                 (let ([sym (if (string? name) (string->symbol name) name)])
                   (values sym expr))
                 (raise-arguments-error tyname "FieldAssign requires a symbol or string for its name"
                                        "name" name
                                        "expr" expr))))
            
; creating a new record
(struct NewRecordExpr (name assignments) #:transparent
   #:guard (λ (name assignments tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym assignments))
            ))
; creating a new array
(struct NewArrayExpr (name num-elements init-values) #:transparent
   #:guard (λ (name num-elements init-value tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym num-elements init-value))
            ))
; an if expression (hint, you may temporarily use an IfElseExpr if you
; would like to make it easy to see when you're matching or not
(struct IfExpr (test true-branch false-branch) #:transparent)
; a while expression, which is a test and the body
(struct WhileExpr (test body) #:transparent)
; an assignment expression
(struct AssignmentExpr (name expr) #:transparent
   #:guard (λ (name expr tyname)
            (let ([sym (if (string? name) (string->symbol name) name)])
              (values sym expr))
            ))
; break expression--this has no arguments
(struct BreakExpr () #:transparent)
; with expression (think: for expression)
(struct WithExpr (idname fromexpr toexpr body) #:transparent
   #:guard (λ (idname fromexpr toexpr body tyname)
            (let ([sym (if (string? idname) (string->symbol idname) idname)])
              (values sym fromexpr toexpr body))
            ))

; peng expressions
(struct PengExpr () #:transparent)

(define niparser
  (cfg-parser
   (src-pos)
   (start program)
   (end EOF)
   ;(debug "parse-table.txt")
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            ; indicate a parsing error occurred
            (parse-error #t)
            (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
                (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                        (lex:position-line start-pos) (lex:position-col start-pos) tok-name tok-value tok-ok?))))
   (tokens value-tokens paren-types operators punctuation
           comparators boolops keywords endoffile)
   (grammar
    
    ; a program will be a list of declarations and expressions
    (program
     ;[(decs) (list $1)]
     ;[(decs program) (cons $1 $2)]
     [(expr) (list $1)])
     ;[(expr program) (cons $1 $2)])
    
    (decs
     [(rec-typedecl) $1]
     [(functiondecl) $1]
     [(vardecl) (begin
                  (add-note $1 'position $1-start-pos)
                  $1)])
    
    ; recursive type declarations
    (rec-typedecl
     ; add the position for the base cases 
     [(typedecl) (begin
                   (add-note $1 'position $1-start-pos)
                   $1)]
     [(typedecl AND rec-typedecl)
      (cond
        [(NameType? $1) (let ([node (struct-copy NameType $1 [next $3])])
                          ; need to add the position for these since we're changing the original
                          (add-note node 'position $1-start-pos)
                          node)]
        [(RecordType? $1) (let ([node (struct-copy RecordType $1 [next $3])])
                            (add-note node 'position $1-start-pos)
                            node)]
        [(ArrayType? $1) (let ([node (struct-copy ArrayType $1 [next $3])])
                           (add-note node 'position $1-start-pos)
                           node)]
        [else (error "Parser error, unknown kind for rec-typedecl")])])
    
    
    ; type declarations, of which there are three types
    (typedecl
     [(DEFINE typeid KIND AS recordtype) (RecordType $2 $5 '())]
     [(DEFINE typeid KIND AS arraytype) (ArrayType $2 $5 '())]
     [(DEFINE typeid KIND AS typeid) (NameType $2 $5 '())])
                                       
    
    ; defines a record type
    (recordtype
     [(LBRACE typefields RBRACE) $2])
    
    ; defines an array type
    (arraytype
     [(ARRAY OF typeid) $3])
    
    ; var declarations, which can be like "ni x is 5" or "ni int x is 5"
    (vardecl
     [(NI ID IS expr) (VarDecl #f $2 $4)]
     [(NI typeid ID IS expr) (VarDecl $2 $3 $5)])
    
    ; type ids
    (typeid
     ; note, we just return the type id here, we don't need to make a new special node
     [(ID) $1])
    
    ; typefields -- these are used by records, we want a list here of TypeField structs
    ; note, these can be empty! 
    (typefields
     [() '()]
     [(typeid ID) (let ([node (TypeField $2 $1)])
                    (add-note node 'position $1-start-pos)
                    (list node))]
     [(typeid ID COMMA typefields) (let ([node (TypeField $2 $1)])
                                     (add-note node 'position $1-start-pos)
                                     (cons node $4))])
    

    ; fun declarations!
    (functiondecl
     [(NEEWOM ID LPAREN typefields RPAREN IS expr) (let ([node (FunDecl $2 $4 #f $7 '())])
                                                     (add-note node 'position $1-start-pos)
                                                     node)]
     [(NEEWOM ID LPAREN typefields RPAREN AS typeid IS expr) (let ([node (FunDecl $2 $4 $7 $9 '())])
                                                               (add-note node 'position $1-start-pos)
                                                               node)]
     [(NEEWOM ID LPAREN typefields RPAREN IS expr AND functiondecl) (let ([node (FunDecl $2 $4 #f $7 $9)])
                                                                      (add-note node 'position $1-start-pos)
                                                                      node)]
     [(NEEWOM ID LPAREN typefields RPAREN AS typeid IS expr AND functiondecl) (let ([node (FunDecl $2 $4 $7 $9 $11)])
                                                                                (add-note node 'position $1-start-pos)
                                                                                node)])

    ; lvalues, these are things that appear on the left of an expression
    (lvalue
     [(ID) (let [(e (VarExpr $1))]
             (add-note e 'position $1-start-pos)
             e)]
     [(lvalue DOT ID) (let [(e (RecordExpr $1 $3))]
                        (add-note e 'position $1-start-pos)
                        e)]
     [(lvalue LBRACKET expr RBRACKET) (let [(e (ArrayExpr $1 $3))]
                                        (add-note e 'position $1-start-pos)
                                        e)])

    ; expressions!
    (expr
     ; expressions specificaly for order of operations around
     ; mathematical things, like bools, addition, etc.
     [(low-prec-expr) (begin
                        (add-note $1 'position $1-start-pos)
                        $1)]
     [(statements) (begin
                     (add-note $1 'position $1-start-pos)
                     $1)]
     )


    ; defines the lowest precedence expressions
    (low-prec-expr
     ; entry into all the math-like expressions
     [(logicexpr) $1]
     )
    
    ; statements, these are no value expressions, so you really can't have these mixed
    ; in with other kinds of expressions, like 1 + while ... doesn't make sense
    (statements
     ; assignment, it should be at the top of a parse tree
     [(NOW lvalue IS expr) (AssignmentExpr $2 $4)]
     ; so should loops
     [(while-expr) $1]
     [(with-expr) $1]
     [(IF expr THEN expr END) (IfExpr $2 $4 '())]
     )

    ;(second-highest-prec-expr
     
     ;[(highest-prec-expr) $1])
     
    (highest-prec-expr
     [(funcall-expr) $1]                   ; function calls
     [(LPAREN expr RPAREN) $2]             ; paren expression     
     [(lvalue) $1]                         ; lvalue expression
     [(sequence-expr) $1]                  ; sequence expression
     [(NUM) (NumExpr $1)]                  ; just a number
     [(STRING) (StringExpr $1)]            ; just a string
     [(noval) $1]                          ; the empty expression
     [(letexpr) $1]                        ; let expressions
     [(recordcreation) $1]                 ; creating a new record
     [(arraycreation) $1]                  ; creating a new array
     [(BREAK) (BreakExpr)]                 ; breaks from loops
     [(PENG) (PengExpr)]                   ; good old peng
     [(unary-minus-expr) $1]               ; for unary minus
     [(BOOL) (BoolVal $1)]                 ; for 'true' or 'false'  
     [(ifelse-expr) $1]
     )
    
    (unary-minus-expr
     [(SUB expr) (MathExpr (NumExpr "0") '- $2)])

    ; if/then/else expressions, these must return values
    (ifelse-expr
     [(IF expr THEN expr ELSE expr END) (IfExpr $2 $4 $6)])

    ; while loops, these cannot return values
    (while-expr
     [(WHILE expr DO expr END) (WhileExpr $2 $4)])

    ; with expressions, these cannot return values
    (with-expr
     [(WITH ID AS expr TO expr DO expr END) (WithExpr $2 $4 $6 $8)])


    ; logic expressions have the lowest precedence, this allows us to
    ; easily check (without parenthesis) on something like x < 5 & x > 6
    (logicexpr
     [(logicexpr BOOLAND compexpr) (LogicExpr $1 'and $3)]
     [(logicexpr BOOLOR compexpr) (LogicExpr $1 'or $3)]
     [(BOOLNOT logicexpr) (LogicExpr $2 'not #f)]
     [(compexpr) $1])
    
    ; since boolean expressions have lower precedence, we begin here
    ; before factoring out the ops that have the highest precedence,
    ; note that this will prevent the more obvious attempts at 0 < 5 < 10,
    ; but will not work for things like 0 < 5 + (3 < 10)--typechecking
    ; should do that
    (compexpr
     [(arithmeticexpr EQ arithmeticexpr) (BoolExpr $1 'eq $3)]
     [(arithmeticexpr LT arithmeticexpr) (BoolExpr $1 'lt $3)]
     [(arithmeticexpr GT arithmeticexpr) (BoolExpr $1 'gt $3)]
     [(arithmeticexpr GE arithmeticexpr) (BoolExpr $1 'ge $3)]
     [(arithmeticexpr LE arithmeticexpr) (BoolExpr $1 'le $3)]
     [(arithmeticexpr NE arithmeticexpr) (BoolExpr $1 'ne $3)]
     [(arithmeticexpr) $1])
     
    ; now we factor out expressions for additions and subtraction
    ; so they have lower precedence than MULT and DIV
    (arithmeticexpr
     [(arithmeticexpr ADD term) (MathExpr $1 '+ $3)]
     [(arithmeticexpr SUB term) (MathExpr $1 '- $3)]
     [(term) $1])

    
    ; terms are things that are added, which is why
    ; they result directly from expressions
    (term
     [(term MULT factor) (MathExpr $1 '* $3)]
     [(term DIV factor) (MathExpr $1 '/ $3)]
     [(factor) $1])

    
    ; factors are things that are multiplied, which is why
    ; they result from terms above 
    (factor
     [(highest-prec-expr) (begin
                                   (add-note $1 'position $1-start-pos)
                                   $1)])
     
    ; rules related to function calls
    (funcall-expr
     [(ID LPAREN argexpr RPAREN) (FuncallExpr $1 $3)])
    
    ; this is specifically to match the list of arguments for a function call
    (argexpr
     [() '()]
     [(expr) (list $1)]
     [(expr COMMA argexpr) (cons $1 $3)])
    
    ; sequence expression, these simply return a list of expressions
    (sequence-expr
     [(LPAREN expr-list RPAREN) $2])
    
    ; the internals of a sequence expression, which must be at least two things
    ; (otherwise, it's just a paren expression, or with none, it's a NoVal)
    (expr-list
     [(expr SEMI expr) (cons $1 (list $3))]
     [(expr SEMI expr-list) (cons $1 $3)])
    
    ; the empty expression
    (noval
     [(LPAREN RPAREN) (NoVal)])
    
    
    ; let expressions
    (letexpr
     [(LET declist IN exprseq END) (LetExpr $2 $4)])
    ; dec list, needed for let expressions
    (declist
     [(decs) (list $1)]
     [(decs declist) (cons $1 $2)])
    ; exprseq, needed for let expressions
    (exprseq
     [() '()]
     [(expr) (list $1)]
     [(expr SEMI exprseq) (cons $1 $3)])
    
    ; creation of records
    (recordcreation
     [(typeid LBRACE fieldassignments RBRACE) (NewRecordExpr $1 $3)]
     [(typeid LBRACE RBRACE) (NewRecordExpr $1 '())])
    ; field assignments are used within the braces of records
    (fieldassignments
     [(ID IS expr) (list (FieldAssign $1 $3))]
     [(ID IS expr COMMA fieldassignments) (cons (FieldAssign $1 $3) $5)])

    ; creation of arrays
    (arraycreation
     [(typeid LBRACKET expr RBRACKET OF expr) (NewArrayExpr $1 $3 $6)])

     
    )))

; input port -> ni ast   
; construct an ast from the input port
(define (build-ast in)
  (parse-error #f)
  (hash-clear! (annotations))
  (port-count-lines! in)
  (niparser (get-tokenizer in)))

; string representing ni code -> ni ast
; parses a string and turns it into an ast if possible
(define (parse-str str)
  (let ([in (open-input-string str)])
    (build-ast in)))

; string (filename) -> ni ast
; opens and parses a file and tries to turn it into an ast if possible
(define (parse-file filename)
  (let ([in (open-input-file filename)])
    (build-ast in)))




