#lang racket

(require racket/cmdline)
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(require test-engine/racket-tests)

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
  (lexer
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
   [whitespace (alexer input-port)]
   ;End-of-line
   [(eof) (token-EOF)]
   [(:or "\r" "\n" "\t\n" "\n\t") (alexer input-port)]
   ;Strings
   [(:: #\" (:*(complement #\")) #\" ) (token-STRING lexeme)] 
   ;identifier
   [(:: alphabetic (:*(:or alphabetic numeric "-" "_" "'"))) (token-ID lexeme)]
   ;int
   [(:+ numeric) (token-NUM lexeme)]
   ;comments
   [(:: "/*" (:*(complement "*/")) "*/" ) (alexer input-port)]

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

;(lexfile )

;test
#|(check-expect (lexstr "and") (list (token-AND)))
(check-expect (lexstr "array") (list (token-ARRAY)))
(check-expect (lexstr "as") (list (token-AS)))
(check-expect (lexstr "break") (list (token-BREAK)))
(check-expect (lexstr "do") (list (token-DO)))
(check-expect (lexstr "else") (list (token-ELSE)))
(check-expect (lexstr "end") (list (token-END)))
(check-expect (lexstr "if") (list (token-IF)))
(check-expect (lexstr "in") (list (token-IN)))
(check-expect (lexstr "is") (list (token-IS)))
(check-expect (lexstr "juniper") (list (token-JUNIPER)))
(check-expect (lexstr "kind") (list (token-KIND)))
(check-expect (lexstr "let") (list (token-LET)))
(check-expect (lexstr "neewom") (list (token-NEEWOM)))
(check-expect (lexstr "ni") (list (token-NI)))
(check-expect (lexstr "now") (list (token-NOW)))
(check-expect (lexstr "of") (list (token-OF)))
(check-expect (lexstr "peng") (list (token-PENG)))
(check-expect (lexstr "]") (list (token-RBRACKET)))
(check-expect (lexstr "[") (list (token-LBRACKET)))
(check-expect (lexstr "}") (list (token-RBRACE)))
(check-expect (lexstr "{") (list (token-LBRACE)))
(check-expect (lexstr ")") (list (token-RPAREN)))
(check-expect (lexstr "(") (list (token-LPAREN)))
(check-expect (lexstr "+") (list (token-ADD)))
(check-expect (lexstr "-") (list (token-SUB)))
(check-expect (lexstr "/") (list (token-DIV)))
(check-expect (lexstr "*") (list (token-MULT)))
(check-expect (lexstr ";") (list (token-SEMI)))
(check-expect (lexstr ",") (list (token-COMMA)))
(check-expect (lexstr ":") (list (token-COLON)))
(check-expect (lexstr ">") (list (token-GT)))
(check-expect (lexstr "<") (list (token-LT)))
(check-expect (lexstr "=") (list (token-EQ)))
(check-expect (lexstr ">=") (list (token-GE)))
(check-expect (lexstr "<=") (list (token-LE)))
(check-expect (lexstr "&") (list (token-BOOLAND)))
(check-expect (lexstr "|") (list (token-BOOLOR)))
(check-expect (lexstr "1") (list (token-NUM "1")))
(check-expect (lexstr "ax") (list (token-ID "ax")))
(check-expect (lexstr (string-append (string #\") (string #\\) (string #\") "Hello world" (string #\\) (string #\") (string #\")))
 (list (token-STRING (string-append (string #\") (string #\\) (string #\") "Hello world" (string #\\) (string #\") (string #\") ))))
(check-expect (lexstr "") '())


(check-expect (lexstr "ni") (list (token-NI)))
(check-expect (lexstr "5") (list (token-NUM "5")))
(check-expect (lexstr "56") (list (token-NUM "56")))
(check-expect (lexstr "/* Hello
there */") '())

(test)
   

;lexfile filename open-input-file
;find . -name \*.ni -exec nic {} \;

(display "Hello World!")|#