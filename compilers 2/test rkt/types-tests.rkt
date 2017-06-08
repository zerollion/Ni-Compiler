#lang racket

(require rackunit
         "types.rkt")

(define type-tests
  (test-suite
   "Tests for types structs"

   (check-equal? (name-cycle? (NameType '() 'foo)) #f "test without cycle")
   (check-equal? (name-cycle? (NameType (NameType '() 'foo) 'bar)) #f "type with actual but no cycle")
   (check-equal? (name-cycle? (NameType (NameType '() 'foo) 'foo)) #t "type with cycle")
   (check-equal? (name-cycle? (NameType (NameType (NameType '() 'foo) 'bar) 'foo)) #t "type with cycle")
   (check-equal? (name-cycle? (NameType (NameType (NameType '() 'foo) 'foo) 'bar)) #t "type with cycle")
   (check-equal? (name-cycle? (NameType (NameType (NameType '() 'bar) 'foo) 'foo)) #t "type with cycle")

   (check-equal? (names-only? (IntType '())) #f "non-name type should be false")
   (check-equal? (names-only? (NameType (make-IntType) 'foo)) #f "aliases to types are ok")
   (check-equal? (names-only? (NameType (NameType '() 'foo) 'bar)) #t "names only should be true")
   (check-equal? (names-only? (NameType (NameType (NameType '() 'foo) 'bar) 'baz)) #t "multiple nested name types")
   (check-exn exn:fail? (thunk (names-only? 'foo)) "argument test on names-only")
   
   ))



(require rackunit/text-ui)
(run-tests type-tests)
