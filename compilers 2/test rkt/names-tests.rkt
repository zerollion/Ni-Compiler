#lang racket/base

(require rackunit
         rackunit/text-ui
         (prefix-in lex: parser-tools/lex)
         "names.rkt")

(define names-tests
  (test-suite
   "Tests for the names module"

   (check-equal? (get-label) (Label "L1"))
   (check-equal? (get-temp) (Temp "t1" 1))
   (check-equal? (get-label) (Label "L2"))
   (check-equal? (get-temp) (Temp "t2" 2))
   (check-equal? (begin (reset-names)
                        (get-label 20)) (Label "L20"))
   (check-equal? (begin (reset-names)
                        (get-temp 20)) (Temp "t20" 20))
   ))

(run-tests names-tests)