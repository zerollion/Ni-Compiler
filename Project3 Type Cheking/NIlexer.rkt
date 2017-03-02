#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

 

(provide (all-defined-out))


(define-tokens value-tokens (NUM ID STRING COMMENT BOOL))
(define-empty-tokens paren-types (LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE))
(define-empty-tokens operators (ADD MULT DIV SUB DOT))
(define-empty-tokens punctuation (COMMA COLON SEMI))
(define-empty-tokens comparators (EQ NE LT GT LE GE))
(define-empty-tokens boolops (BOOLOR BOOLAND))

(define-empty-tokens keywords (AND ARRAY AS BREAK DEFINE DO ELSE END FOR IF IN IS
                                   JUNIPER KIND LET NEEWOM NI NOW OF PENG THEN
                                   TO WHILE WITH))

(define-empty-tokens endoffile (EOF))

(define nilexer
  (lexer-src-pos
   ; keywords
   ["and" (token-AND)]
   ["array" (token-ARRAY)]
   ["as" (token-AS)]
   ["break" (token-BREAK)]
   ["define" (token-DEFINE)]
   ["do" (token-DO)]
   ["else" (token-ELSE)]
   ["end" (token-END)]
   ["false" (token-BOOL #f)]
   ["if" (token-IF)]
   ["in" (token-IN)]
   ["is" (token-IS)]
   ["juniper" (token-JUNIPER)]
   ["kind" (token-KIND)]
   ["let" (token-LET)]
   ["neewom" (token-NEEWOM)]
   ["ni" (token-NI)]
   ["now" (token-NOW)]
   ["of" (token-OF)]
   ["peng" (token-PENG)]
   ["then" (token-THEN)]
   ["true" (token-BOOL #t)]
   ["to" (token-TO)]
   ["while" (token-WHILE)]
   ["with" (token-WITH)]
   ; parens
   ["]" (token-RBRACKET)]
   ["[" (token-LBRACKET)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ;[(complement (:: any-string "xx")) (token-NUM lexeme)]
   
   
   ; operators
   ["+" (token-ADD)]
   ["-" (token-SUB)]
   ["/" (token-DIV)]
   ["*" (token-MULT)]
   ["." (token-DOT)]
   ; punctuation
   [";" (token-SEMI)]
   ["," (token-COMMA)]
   [":" (token-COLON)]
   ;comparators
   [">" (token-GT)]
   ["<" (token-LT)]
   ["=" (token-EQ)]
   ["<>" (token-NE)]
   [">=" (token-GE)]
   ["<=" (token-LE)]
   ; boolops
   ["&" (token-BOOLAND)]
   ["|" (token-BOOLOR)]
   ; now for the value tokens, first numbers, these are just digits
   [(:+ numeric) (token-NUM lexeme)]
   ; next IDs, these are a bit more complicated
   [(:: alphabetic (:* (:or alphabetic numeric "_" "-")) (:* #\')) (token-ID (string->symbol lexeme))]
   ; finally, strings
   ;[(:: #\" (:or (:: any-string "\\\"" any-string) (complement (:: any-string "\"" any-string)))  #\") (token-STRING lexeme)]
   [(:: #\" (:* (:or (:: #\\ any-char) (char-complement (char-set "\"\\"))))  #\") (token-STRING lexeme)]

   ; comments
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (return-without-pos (nilexer input-port))]                                                        
   ; match // followed by anything that ends in \n 
   [(:: "//" (:+ (char-complement #\newline))) (return-without-pos (nilexer input-port))]
   
   [whitespace (return-without-pos (nilexer input-port))]
   [(eof) (token-EOF)]
   [any-char (raise-lex-error start-pos lexeme)]
   ))

(define (raise-lex-error pos lexeme)
  (let* ([linenums? (not (eq? (position-line pos) #f))]
         [loc (if linenums? (position-line pos) (position-offset pos))]
         [col (position-col pos)]
         [partial-msg (string-append (if linenums? "syntax error at line "
                                         "syntax error at offset ") (number->string loc))]
         [msg (string-append partial-msg (if linenums? (string-append ", col " (number->string col)) "")
                             ": '" lexeme "'")])
         (raise-syntax-error 'nilexer msg)))
    
      

; input port -> list of tokens
; lex takes an input port and returns a list of tokens by lexing the input port
(define (lex in)
  ; this doesn't necessarily need to be internal
  (letrec ([lexfun
            (λ () (let ([tok (nilexer in)])
                     ; test to see if equal to eof and return empty list
                     (cond [(eq? (position-token-token tok) (token-EOF)) '()]
                           ; otherwise append token to eval of rest of tokens
                           [else (cons (position-token-token tok) (lexfun))])))])
    (lexfun)))


; string -> list of tokens
; lexstr takes a string and returns a list of tokens by lexing the given string
(define (lexstr str)
  (let ([in (open-input-string str)])
    (port-count-lines! in)
    (lex in)))

; filename -> list of tokens
; lexfile opens the input port on the filename and then lexes its contents
(define (lexfile filename)
  (let ([in (open-input-file filename)])
    (port-count-lines! in)
    (lex in)))

; input port -> 0-arg function that returns next token
(define (get-tokenizer in)
  (lambda () (nilexer in)))
