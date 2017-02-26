#lang racket

(provide (all-defined-out))

#|; types for Ni
(struct NiType (actual) #:transparent
 #:guard (λ (actual typename)
            (if (eq? typename 'NiType)
                (error "Can't instantiate NiType directly.")
                (if (or (eq? actual '()) (NiType? actual)) 
                    (values actual)
                    (error "Can only instantiate with NiTypes or '()")))))
(struct StringType NiType () #:transparent)
(struct VoidType NiType () #:transparent)
(struct IntType NiType () #:transparent)
(struct NameType NiType () #:transparent)
(struct BoolType NiType () #:transparent)
(struct PengType NiType () #:transparent)
(struct ArrayType NiType (element-type) #:transparent)
; for records, we need two structs
(struct RecordType NiType (fields) #:transparent)
; in this case, the name is the symbol name of a field, 
; and actual will refer to the actual type
(struct NameTypePair NiType (name) #:transparent)|#

; types for Ni
(struct NiType ([actual #:mutable]) #:transparent
  #:guard (λ (actual typename)
            (if (eq? typename 'NiType)
                (error "Can't instantiate NiType directly.")
                (if (or (eq? actual '())
                        (NiType? actual)) (values actual)
                                          (error "Can only instantiate with NiTypes or '()")))))
(struct StringType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:str" port)))])
(struct VoidType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:void" port)))])
(struct IntType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:int" port)))])

(struct NameType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (lambda (p port) (print p port mode))])])
                                                     (write-string "" port))))])
                                                   

(struct BoolType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:bool" port)))])
(struct PengType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (write-string "t:peng" port)))])

(struct ArrayType NiType (element-type) #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (lambda (p port) (print p port mode))])])
                                                     (display "t:array[ " port)
                                                     (theprinter (ArrayType-element-type ty) port)
                                                     (display " ]" port))))])

; for records, we need two structs
(struct RecordType NiType (fields) #:transparent
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (lambda (p port) (print p port mode))])])
                                                     (display "t:rec{ " port)
                                                     (for-each (lambda (field)
                                                                 (theprinter field port)
                                                                 (display " " port)) (RecordType-fields ty))
                                                     (display "}" port))))])
; in this case, the name is the symbol name of a field, 
; and actual will refer to the actual type
(struct NameTypePair NiType (name) #:transparent)
;value types
(struct VarValue (type) #:transparent)
; as with records, we need something for parameters,
; so this will be stored as a list of NameTypePair structs
(struct FunValue (parameters return-type) #:transparent)

;---------------------make-types functions------------------------
(define (make-StringType) (StringType '()))
(define (make-VoidType) (VoidType '()))
(define (make-IntType) (IntType '()))
;(define (make-NameType) (NameType 'NiType))
(define (make-BoolType) (BoolType '()))
(define (make-PengType) (PengType '()))
(define (make-ArrayType typename) (ArrayType '() typename))

;record type
(define (make-RecordType fields) (RecordType '() fields))
(define (make-NameTypePair name) (NameTypePair 'NiType name))

;value types
(define (make-VarValue type) (VarValue type))
(define (make-FunValue parameters return-type)
  (FunValue parameters return-type))