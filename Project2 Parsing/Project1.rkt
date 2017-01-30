#lang racket

;Tan Zhen COMP4704

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
   [(:: "/*" (complement (:: (:* any-char) "*/" (:* any-char))) "*/" ) (return-without-pos (alexer input-port))]
   ;[(:: "/*" (:* (char-complement (char-set "*/"))) "*/" ) (alexer input-port)]
   [(:: "//" (:* (char-complement #\newline)) "\n" ) (return-without-pos (alexer input-port))]
   [any-char (error "unexpected input")]

  ))

(define (lexstr str)
  (letrec ([in (open-input-string str)]
           [lexr
            (λ (i) (let ([tok (alexer i)])
                    (cond [(eq? tok (token-EOF)) '()]
                          [else (cons tok (lexr i))])))])
    (lexr in)))

(define (lexfile filename)
  (letrec ([in (open-input-file filename)]
           [lexr
            (λ (i) (let ([tok (alexer i)])
                    (cond [(eq? tok (token-EOF)) '()]
                          [else (cons tok (lexr i))])))])
    (lexr in)))

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