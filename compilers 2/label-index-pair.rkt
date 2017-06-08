#lang racket

(require "names.rkt")

(provide (all-defined-out))

;struct to pair index with label
(struct LIpair (label
                [index #:mutable])
  #:transparent
  #:guard (λ (label index tyname)
            (cond
              [(not (and (Label? label)
                         (exact-positive-integer? index)))
               (raise-arguments-error tyname "argument type mismatch for LIpair"
                                      "Label? label" label
                                      "number? index" index)]
              [else (values label index)])))

;display function
(define (display-LIpair ir out)
  (let* ([label (LIpair-label ir)]
         [index (LIpair-index ir)]
         [labelstr (Label-name label)])
    (fprintf out "~a: Instrction idx.~a~n"
             labelstr index)))