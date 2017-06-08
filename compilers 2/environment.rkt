#lang racket

(provide (all-defined-out))

(require test-engine/racket-tests)
(require (prefix-in t: "types.rkt"))

; create a new, empty environment
(define (empty-env) (list (make-hash)))

; extend the given environment with a symbol and value associated with it
; and then return the newly modified environment
(define (extend-env env sym val)
  (hash-set! (first env) sym val)
  env)

; apply the given environment with the symbol and return its value
(define (apply-env env sym)
  (cond
    [(eq? env '()) #f]
    [(hash-has-key? (first env) sym) (hash-ref (first env) sym)]
    [else (apply-env (rest env) sym)]))

; only look for the symbol in the topmost environment
(define (first-apply-env env sym)
  (cond
    [(eq? env '()) #f]
    [(hash-has-key? (first env) sym) (hash-ref (first env) sym)]
    [else #f]))

; see if the type is in the environment
; this is slightly costly because we must walk through a bunch of types to figure
; this out--perhaps if we stored types more efficiently to be accessed, but it's
; linear in the number of types
(define (type-in-env? env ty)
  (if (empty? env)
      #f
      (let* ([types (hash-values (first env))]
             [found? (findf (λ (type)
                              (t:type=? type ty)) types)])
        (if found? #t (type-in-env? (rest env) ty)))))

; depth is relative to the caller
(define (apply-env-at-depth env depth sym)
  (if (< depth 0)
      (error "apply-env-at-depth requires depth >= 0")
      (if (= depth 0)
          (cond
            [(eq? env '()) #f]
            [(hash-has-key? (first env) sym) (hash-ref (first env) sym)]
            [else #f]) 
          (apply-env-at-depth (rest env) (sub1 depth) sym))))

; push a new scope onto the environment list and return the new environment
(define (push-scope env)
  (cons (make-hash) env))

; pops a scope from the environment list and return the remaining environment
(define (pop-scope env)
  (rest env))

