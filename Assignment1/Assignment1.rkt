#lang racket

(provide (all-defined-out))

; a structure to hold a token, this is used by the lexer
; a token type will be 'op, 'lparen, 'rparen, or 'digit or 'eof
; repr is a character representation
(struct token (type repr) #:transparent
   ; a guard is a function that 'tests' the values you put into the structure
   ; remember: racket is dynamically typed so you kinda have to check things to
   ; save yourself a ton of grief later (trust me)
   #:guard (λ (type repr struct-name)
     (if (not (is-token-type? type))
         (error "expected a proper token-type which is-token-type? returns true from, got" type)
         (if (and (not (eq? eof repr)) (not (char? repr)))
             (error "expected a string? or eof? for token-repr, got" repr)
             (values type repr)))))

; symbol -> bool
; returns true if the token matches the symbols 'op, 'lparen, 'rparen, 'digit
(define (is-token-type? t)
  (if (or (eq? 'op t) (eq? 'lparen t) (eq? 'rparen t) (eq? 'digit t) (eq? 'eof t)) true false))  

; input-port -> token
; returns the next input token from the input port
(define (get-next-token input-port)
  (let ([t (read-char input-port)])
    (cond
      [(or (eq? #\+ t) (eq? #\* t)) (token 'op t)]
      [(eq? #\( t) (token 'lparen #\()]
      [(eq? #\) t) (token 'rparen #\))]
      [(eq? eof t) (token 'eof eof)]
      [(or (eq? #\0 t) (eq? #\1 t) (eq? #\2 t) (eq? #\3 t)
           (eq? #\4 t) (eq? #\5 t) (eq? #\6 t) (eq? #\7 t)
           (eq? #\8 t) (eq? #\9 t)) (token 'digit t)]
      [else (error "expected a token type, got" t)])))

; string -> 0 argument function that returns the next token on the string
; this function creates a function that uses get-next-token on the string that was passed in,
; notice how we pass create the input by using open-input-string
(define (lexstr str)
 (let ([input (open-input-string str)])
   (λ () (get-next-token input))))

; (() -> token) -> (ast-node | ast-expr-node)
; the parser takes a function (probably produced by lexstr) that
; lexes the contents of the input stream
(define (parser lex)
  (let ([t (lex)])
    (cond
      [(eq? (token-type t) 'eof) '()]     ;end of file, return end of file
      [(eq? (token-type t) 'rparen) (error "not expected a right parenthese")]  ;right parenthese, jump to next char
      [(eq? (token-type t) 'op) (token-repr t)]      ;operator, jump to next char
      [(eq? (token-type t) 'digit) (ast-node (string->number (string (token-repr t))))]
      [(eq? (token-type t) 'lparen)
       (let ([left-child (parser lex)]
             [operator (parser lex)]
             [right-child (parser lex)]
             [temp (lex)])
         (ast-expr-node operator left-child right-child))]
      [else (error "expect token types, got" t)])))

; value node for numbers
(struct ast-node (val) #:transparent)
; expression nodes for operators 
(struct ast-expr-node (operator left-child right-child) #:transparent)

; ast -> val
; this function takes an AST and returns the calculated value
; note that we assume the tree was built correctly!
(define (eval ast)
   (match ast
     ([ast-node v] v)
     ([ast-expr-node op left right]
       (let ([lchild (eval left)]
             [rchild (eval right)])
         (cond
           [(eq? op #\+) (+ lchild rchild)]
           [(eq? op #\*) (* lchild rchild)])))))

; str -> val
; takes a string, creates the lexer and parser and then evaluates it
(define (evalstr str)
  (let ([lexer (lexstr str)])
    (eval (parser lexer))))