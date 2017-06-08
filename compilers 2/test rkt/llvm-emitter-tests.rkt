#lang racket

(require rackunit
         "names.rkt"
         "llvm-emitter.rkt")

(define llvm-emitter-tests 
  (test-suite
   "Typechecking tests for the llvm emitter"

   (test-case
    "math and bool exprs"
    (let ([r1 (Result (Temp "t1" 1))]
          [r2 (Result (Temp "t2" 2))]
          [r3 (Result (Temp "t3" 3))])
      (check-equal? (begin
                      (emit-math 'add r1 r2 r3)
                      (get-output-string (current-writer)))
                    "%t3 = add i64 %t2, %t3")))))

(require rackunit/text-ui)
(run-tests llvm-emitter-tests)
