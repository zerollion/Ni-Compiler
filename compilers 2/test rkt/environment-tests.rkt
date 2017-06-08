#lang racket

(require "environment.rkt"
         "types.rkt"
         test-engine/racket-tests)

; make sure you defined the empty environment properly
(check-expect (empty-env) `(,(hash-copy #hash())))

; simple tests for adding things to the environment with only one level of scope
(check-expect (extend-env (empty-env) 'x 5) `(,(hash-copy #hash((x . 5)))))
(check-expect (extend-env (extend-env (empty-env) 'x 5) 'y 6) `(,(hash-copy #hash((y . 6) (x . 5)))))
(check-expect (extend-env (extend-env (extend-env (empty-env) 'x 5) 'y 6) 'z 7) `(,(hash-copy #hash((z . 7) (y . 6) (x . 5)))))

; tests for checking if something is there
(check-expect (apply-env (extend-env (empty-env) 'x 5) 'x) 5)
(check-expect (apply-env (extend-env (extend-env (empty-env) 'x 5) 'y 6) 'y) 6)
(check-expect (apply-env (extend-env (extend-env (extend-env (empty-env) 'x 5) 'y 6) 'z 7) 'z) 7)

; pushing and popping tests (trival)
(check-expect (push-scope (empty-env)) `(,(make-hash) ,(make-hash)))
(check-expect (pop-scope (push-scope (empty-env))) `(,(make-hash)))

; something more complicated
(check-expect (apply-env (extend-env (push-scope (extend-env (empty-env) 'x 5)) 'y 6) 'y) 6)
(check-expect (apply-env (extend-env (push-scope (extend-env (empty-env) 'x 5)) 'y 6) 'x) 5)
(check-expect (apply-env (pop-scope (extend-env (push-scope (extend-env (empty-env) 'x 5)) 'y 6)) 'x) 5)


; check to see if type-in-top-env? works properly
(check-expect (type-in-env? (extend-env (push-scope (extend-env
                                                     (extend-env (empty-env) 'int (make-IntType))
                                                     'string (make-StringType)))
                                        'foo (make-NameType (make-IntType))) (make-IntType)) #t)

; this should be false
(check-expect (type-in-env? (pop-scope (extend-env (push-scope (extend-env (extend-env (empty-env) 'int (make-IntType)) 'string
                                                                           (make-StringType)))
                                                   'foo  (make-NameType (make-IntType)))) (make-NameType (make-IntType))) #f)




             
(test)