#lang racket

(provide (all-defined-out))

; types for Ni
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
(struct NameTypePair NiType (name) #:transparent)

;value types
(struct VarValue (type) #:transparent)
; as with records, we need something for parameters,
; so this will be stored as a list of NameTypePair structs
(struct FunValue (parameters return-type) #:transparent)