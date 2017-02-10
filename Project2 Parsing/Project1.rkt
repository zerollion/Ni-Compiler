#lang racket

;Tan Zhen COMP4704
;with commentator

(require racket/cmdline)
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))
(require test-engine/racket-tests)

(provide (all-defined-out))

(define-tokens value-tokens (NUM ID STRING))
(define-empty-tokens paren-types (LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE))
(define-empty-tokens operators (ADD MULT DIV SUB DOT))
(define-empty-tokens punctuation (COMMA COLON SEMI))
(define-empty-tokens comparators (EQ NE LT GT LE GE))
(define-empty-tokens boolops (BOOLOR BOOLAND))
(define-tokens comment (BCOMMENT LCOMMENT))

(define-empty-tokens keywords (AND ARRAY AS BREAK DEFINE DO ELSE END IF IN IS
 JUNIPER KIND LET NEEWOM NI NOW OF PENG THEN TO WHILE WITH))

(define-empty-tokens endoffile (EOF))

(define alexer
  (lexer-src-pos
   ;keywords
   ["and" (token-AND)]
   ["array" (token-ARRAY)]
   ["as" (token-AS)]
   ["break" (token-BREAK)]
   ["define" (token-DEFINE)]
   ["do" (token-DO)]
   ["else" (token-ELSE)]
   ["end" (token-END)]
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
   ["to" (token-TO)]
   ["while" (token-WHILE)]
   ["with" (token-WITH)]
   ;symbols
   ["," (token-COMMA)]
   [":" (token-COLON)]
   [";" (token-SEMI)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["[" (token-LBRACKET)]
   ["]" (token-RBRACKET)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["." (token-DOT)]
   ["+" (token-ADD)]
   ["-" (token-SUB)]
   ["/" (token-DIV)]
   ["*" (token-MULT)]
   ["=" (token-EQ)]
   ["<>" (token-NE)]
   ["<" (token-LT)]
   [">" (token-GT)]
   ["<=" (token-LE)]
   [">=" (token-GE)]
   ["|" (token-BOOLOR)]
   ["&" (token-BOOLAND)]
   ;Whitespace
   [whitespace (return-without-pos (alexer input-port))]
   ;End-of-line
   [(eof) (token-EOF)]
   [(:or "\r" "\n" "\t\n" "\n\t") (return-without-pos (alexer input-port))]
   ;Strings
   [(:: #\" (:* (:or (:: #\\ any-char) (char-complement (char-set "\"\\")))) #\" ) (token-STRING lexeme)]
   ;identifier
   [(:: alphabetic (:* (:or alphabetic numeric "-" "_" "'"))) (token-ID lexeme)]
   ;int
   [(:+ numeric) (token-NUM lexeme)]
   ;comments
   [(:: "/*" (complement (:: (:* any-char) "*/" (:* any-char))) "*/" ) (token-BCOMMENT lexeme)]
   ;[(:: "/*" (:* (char-complement (char-set "*/"))) "*/" ) (alexer input-port)]
   [(:: "//" (:* (char-complement #\newline)) "\n" ) (token-LCOMMENT lexeme)]
   [any-char (error "unexpected input")]

  ))

; input port -> list of tokens
; lex takes an input port and returns a list of tokens by lexing the input port
(define (lex in)
  ; this doesn't necessarily need to be internal
  (letrec ([lexfun
            (λ () (let ([tok (alexer in)])
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

#|(define read-file
  (command-line
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))

(define (main filename)
  (let
      ([lst (lexfile filename)])
    (write lst)
  (printf "Pass Test ~a\n" filename)))

(main read-file)|#