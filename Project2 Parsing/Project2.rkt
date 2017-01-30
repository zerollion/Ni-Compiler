#lang racket

;Tan Zhen 872692777
;Project2 Parsing

(require "Project1.rkt")

(define aparser
  (cfg-parser
   (src-pos)
   (start tokens)
   (end EOF)
   (tokens value paren operators punctuation comparators boolops)
   (error (λ (tok-ok? tok-name start-pos end-pos
              (error "Parsing error ~a on token ~a, start at ~a end at ~a\n"))))
   (grammar
    )
   
   ))

(define (parse-str str)
  (let ([in (open-input-string str)])
    (port-count-lines! in)
    (aparser (alexer in))))