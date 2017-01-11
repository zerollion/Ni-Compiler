#lang racket
; a structure to hold a digit, this is used by the lexer
; a token type will be 'op, 'lparen, 'rparen, or 'digit or 'eof
; repr is a character representation
(struct token (type repr) #:transparent
   ; a guard is a function that 'tests' the values you put into the structure
   ; remember: racket is dynamically typed so you kinda have to check things to
   ; save yourself a ton of grief later (trust me)
   #:guard (λ (type repr struct-name)
     (if (not (is-token-type? type))
         (error "expected a proper token-type which is-token-type? returns true from, got" type)
         (if (and (not (eq? eof repr)) (not (char? repr)))
             (error "expected a string? or eof? for token-repr, got" repr)
             (values type repr)))))

; symbol -> bool
; returns true if the token matches the symbols 'op, 'lparen, 'rparen, 'digit
(define (is-token-type? t) ... )