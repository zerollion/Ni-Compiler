#lang racket

(require test-engine/racket-tests
         "nilexer.rkt")


; test the basics

(check-expect (lexstr "and") (list (token-AND)))
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
(check-expect (lexstr "ax") (list (token-ID 'ax)))
(check-expect (lexstr (string-append (string #\") (string #\\) (string #\") "Hello world"  (string #\\) (string #\") (string #\")))
              (list (token-STRING (string-append (string #\") (string #\\) (string #\") "Hello world" (string #\\) (string #\") (string #\") ))))
(check-expect (lexstr "") '())
(check-expect (lexstr "ni") (list (token-NI)))
(check-expect (lexstr "5") (list (token-NUM "5")))
(check-expect (lexstr "56") (list (token-NUM "56")))
(check-expect (lexstr "/* Hello
there */") '())
(check-expect (lexstr "/* hello */5/*there*/") (list (token-NUM "5")))
(check-expect (lexstr "// this is a comment \n5+3") (list (token-NUM "5") (token-ADD) (token-NUM "3")))
(check-expect (lexstr "\"\\\\\"a\"\"") (list (token-STRING "\"\\\\\"") (token-ID 'a) (token-STRING "\"\"")))
(check-expect (lexstr "\"you had me at \\\"hello\\\"\"") (list (token-STRING "\"you had me at \\\"hello\\\"\"")))


(check-expect (lexfile "../../ni-samples/tests/test01.ni")
              (list
               'LET
               'DEFINE
               (token-ID 'arrtype)
               'KIND
               'AS
               'ARRAY
               'OF
               (token-ID 'int)
               'NI
               (token-ID 'arrtype)
               (token-ID 'arr1)
               'IS
               (token-ID 'arrtype)
               'LBRACKET
               (token-NUM "10")
               'RBRACKET
               'OF
               (token-NUM "0")
               'IN
               (token-ID 'arr1)
               'END))
              
(test)
