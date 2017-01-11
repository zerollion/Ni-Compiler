#lang racket
         
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
      [(cond [(or #\+ #\+) (token 'op t)])]
      [(eq? (token-type t) 'lparen) (token 'lparen #\()]
      [(eq? (token-type t) 'rparen) (token 'rparen #\))]
      [(eq? (token-type t) 'eof) (token 'eof eof)]
      [(cond [(or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (token 'digit t)])])))

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
      [(eq? (token-type t) 'eof) (eof)]     ;end of file, return end of file
      [(eq? (token-type t) 'rparen) (lex)]  ;right parenthese, jump to next char
      [(eq? (token-type t) 'op) (lex)]      ;operator, jump to next char
      [(eq? (token-type t) 'digit) (ast-node (token-repr t))]
      [(eq? (token-type t) 'lparen)
       (let([left-child (parser lex)]
            [operator (parser lex)]
            [right-child (parser lex)]
            [temp (lex)])
         (ast-expr-node (operator left-child right-child)))])))

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
     (values v)))

; str -> val
; takes a string, creates the lexer and parser and then evaluates it
(define (evalstr str)
  (let ([lexer (lexstr str)])
    (eval (parser lexer))))