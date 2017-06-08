#lang racket

(require "array-list.rkt"
         (prefix-in i: "iloc.rkt")
         (prefix-in t: "types.rkt")
         "niparser.rkt"
         "array-list.rkt"
         "errors.rkt"
         "typecheck.rkt"
         "names.rkt"
         "label-index-pair.rkt"
         "frame-builder.rkt"
         data/gvector)

;changed-tz
;create a label-list to store labels and corresponding index of instructions
(define labelist (make-parameter (make-array-list)))
;create a funlist to store function declearations
(define funlist (make-parameter (make-array-list)))
(define outer-let (make-parameter #t))

(define (clearlabel)
  (labelist (make-array-list))
  (funlist (make-array-list))
  (outer-let (make-parameter #t)))

(provide (all-defined-out))

;;;;; REGISTERS AND OFFSETS ;;;;;;
;;
;; These define static register names (Rarp) and offsets for various fields
;; in our activation record. We use these instead of hard numbers so we can
;; optimize (possibily?) and delay deciding on the actual frame layout until later!
(define Rarp (Result (Temp "Rarp" 0) #f #f))
(define PC (Result (Temp "PC" 0) #f #f))
; holds an offset for the static link
(define static-link-offset (make-frame-offset 'static-link))
; holds an offset for the return address
(define return-addr-offset (make-frame-offset 'return-addr))
; holds an offset for the address of where to store the return value
(define return-value-offset (make-frame-offset 'return-value))
; holds an offset for the address of the previous activation record, this is
; used because we can just store where to pop off a record instead of
; needing to figure out the math for it later
(define previous-arp (make-frame-offset 'previous-arp))

; simple translator that only outputs the stuff that you've added, not all
; the preamble and postamble materials
(define (trans str)
  (clear-errors)
  (clearlabel)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse error")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (let ([alist (make-array-list)])
                (build-frames ast)
                (ast->iloc! (first ast) alist)
                ;end of program mark 
                (array-list-add-item! alist (i:nop #f #f #f))
                ;begin function declaration
                (for ([node (array-list-vec (funlist))])
                  (fundecl->iloc node alist))
                
                ;add-note to alist(add label to instruction list)
                (for ([i (array-list-vec (labelist))]
                      [k (in-naturals)])
                  (let* ([label (LIpair-label i)]
                         [index (LIpair-index i)])
                    (add-note (array-list-item-ref alist index) 'lab (Label-name label))))
                
                ; now walk through the alist
                (printf "~ntranslation resulted in ~a ILOC instructions:~n"
                        (array-list-length alist))
                (display-array-list alist (current-output-port))
                (printf "/*---Label List:---*/~n")
                (display-array-list (labelist) (current-output-port))
                alist))))))


; actually translates the string and emits the entire 
(define (translate-str str)
  ; clear the errors first
  (clear-errors)
  (clearlabel)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse errors")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (begin
                (translate-ast ast)))))))

(define (translate-ast ast)
  ; need an array list to store the iloc in
  (let ([alist (make-array-list)])
    (ast->iloc! (first ast) alist))) ;changed-tz

(provide ast->iloc!)
; translate a ni AST into an array-list of iloc because I think it will
; be easier to handle given the algorithms in the book (I mean we could
; convert all the iterative versions into recursive versions, but that
; seems like extra work we probably don't need). Note, because of this, we
; have to pass an 'accumulator', which is our current array list of iloc.
; Oh, this also means, we need to do more 'add-note' kind of stuff with
; our iloc structs (like with 'label) and also, we mutate alist (not the ast)
(define (ast->iloc! ast alist)
  (let ([result
         (match ast
           ; deal with lists, like in let expressions
           ['() '()]
           [(NoVal) '()]
           [(list node) (ast->iloc! node alist)]
           [(cons first rest) (ast->iloc! first alist) (ast->iloc! rest alist)]
           
           ; and so we begin again...a numeric literal
           [(NumExpr val) (num->iloc val alist)]
           [(MathExpr _ _ _) (math->iloc ast alist)]
           
           ; translate boolean values, which are integer 1 and 0s
           [(BoolVal val) (boolval->iloc val alist)]
           [(BoolExpr _ _ _) (bool->iloc ast alist)]
           [(LogicExpr _ _ _) (log->iloc ast alist)]

           ; string
           ;[(StringExpr val) (string->iloc val alist)]

           ; branching expressions
           [(IfExpr _ _ '()) (if-then->iloc ast alist)]
           [(IfExpr _ _ _) (if-then-else->iloc ast alist)]

           ; variable declaration
           [(VarDecl _ _ _) (vardecl->iloc ast alist)]
           [(VarExpr _) (var->iloc ast alist)]

           ; let expression
           [(LetExpr _ _) (let->iloc ast alist)]

           ; funcall
           [(FuncallExpr _ _) (funcall->iloc ast alist)]
           
           [_ (error "Translation of " ast " not implemented yet")])])
    (add-note ast 'result result)
    result))


; to load a number into a register, since we pretty much need to do that
; with values (well, we could see if it's a math expression or something else
; but that's later with optimization
(define (num->iloc val alist)
  (let ([result (make-temp-result)])
    (array-list-add-item! alist (i:loadI (string->number val) result #f))
    result))


(define (math->iloc ast alist)
  (let ([e1 (MathExpr-expr1 ast)]
        [e2 (MathExpr-expr2 ast)]
        [op (MathExpr-op ast)])
    (let ([res1 (ast->iloc! e1 alist)]
          [res2 (ast->iloc! e2 alist)]
          [result (make-temp-result)])
      (let ([item
             (cond
               [(eq? op '+) (i:add res1 res2 result)]
               [(eq? op '-) (i:sub res1 res2 result)]
               [(eq? op '*) (i:mult res1 res2 result)]
               [(eq? op '/) (i:div res1 res2 result)])])
        (array-list-add-item! alist item)
        result))))

(define (boolval->iloc val alist)
  (let ([result (make-temp-result)]
        [boolnum (cond
                   [(eq? val #t) "1"]
                   [(eq? val #f) "0"])])
    (array-list-add-item! alist (i:loadI (string->number boolnum) result #f))
    result))

(define (bool->iloc ast alist)
  (let ([e1 (BoolExpr-expr1 ast)]
        [e2 (BoolExpr-expr2 ast)]
        [op (BoolExpr-op ast)])
    (let ([res1 (ast->iloc! e1 alist)]
          [res2 (ast->iloc! e2 alist)]
          [result (make-temp-result)])
      (let ([item
             (cond
               [(eq? op 'eq) (i:cmp_EQ res1 res2 result)]
               [(eq? op 'lt) (i:cmp_LT res1 res2 result)]
               [(eq? op 'gt) (i:cmp_GT res1 res2 result)]
               [(eq? op 'ge) (i:cmp_GE res1 res2 result)]
               [(eq? op 'le) (i:cmp_LE res1 res2 result)]
               [(eq? op 'ne) (i:cmp_NE res1 res2 result)])])
        (array-list-add-item! alist item)
        result))))

(define (log->iloc ast alist)
  (let ([e1 (LogicExpr-expr1 ast)]
        [e2 (LogicExpr-expr2 ast)]
        [op (LogicExpr-op ast)])
    (let ([res1 (ast->iloc! e1 alist)]
          [res2 (ast->iloc! e2 alist)]
          [result (make-temp-result)])
      (let ([item
             (cond
               [(eq? op 'and) (i:and res1 res2 result)]
               [(eq? op 'or) (i:or res1 res2 result)])])
        (array-list-add-item! alist item)
        result))))

; how? not-implemented
(define (string->iloc val alist) '())
  
(define (if-then->iloc node alist)
  (let ([test (IfExpr-test node)]
        [trbr (IfExpr-true-branch node)])
    (let* ([testres (ast->iloc! test alist)]
           [truelabel (make-label-result)]
           [Lt (Result-name truelabel)]
           [endlabel (make-label-result)]
           [Le (Result-name endlabel)])
      ; test
      (array-list-add-item! alist (i:cbr testres truelabel endlabel))
      ; true label
      (array-list-add-item! (labelist) (LIpair Lt (array-list-length alist)))
      (ast->iloc! trbr alist)
      
      (array-list-add-item! alist (i:jumpI endlabel #f #f))
      ; end label
      (array-list-add-item! (labelist) (LIpair Le (array-list-length alist)))
  )))

(define (if-then-else->iloc node alist)
  (let ([test (IfExpr-test node)]
        [trbr (IfExpr-true-branch node)]
        [fsbr (IfExpr-false-branch node)])
    (let* ([testres (ast->iloc! test alist)]
           [truelabel (make-label-result)]
           [Lt (Result-name truelabel)]
           [falselabel (make-label-result)]
           [Lf (Result-name falselabel)]
           [endlabel (make-label-result)]
           [Le (Result-name endlabel)])
      ; test
      (array-list-add-item! alist (i:cbr testres truelabel falselabel))
      ; true label
      (array-list-add-item! (labelist) (LIpair Lt (array-list-length alist)))
      (ast->iloc! trbr alist)
      (array-list-add-item! alist (i:jumpI endlabel #f #f))
      ; false label
      (array-list-add-item! (labelist) (LIpair Lf (array-list-length alist)))
      (ast->iloc! fsbr alist)
      (array-list-add-item! alist (i:jumpI endlabel #f #f))
      ; end label
      (array-list-add-item! (labelist) (LIpair Le (array-list-length alist)))
      ;phi
      (let ([true-res (get-note trbr 'result)]
            [false-res (get-note fsbr 'result)]
            [result (make-temp-result)])
        (array-list-add-item! alist (i:phi true-res false-res result))
        result))))

(define (vardecl->iloc node alist)
  (let* ([temp (ast->iloc! (VarDecl-expr node) alist)]
         [noderes (get-note (VarDecl-expr node) 'result)]
         [result (make-temp-result)]
         [varvalue (get-note node 'type)])
    (cond
      ; not escape
      [(eq? (t:VarValue-escape? varvalue) #f)
       (array-list-add-item! alist (i:i2i temp result #f))]
      ; escape variables
      [else
       (let ([name (VarDecl-id node)])
         (array-list-add-item! alist (i:loadI (make-frame-offset name) result #f))
         (array-list-add-item! alist (i:storeAO temp Rarp result))
         )])
    (t:set-VarValue-result! varvalue result)
    result
    ))

(define (var->iloc node alist)
  (let* ([name (VarExpr-name node)]
         [varvalue (get-note node 'var)]
         [noderes (t:VarValue-result varvalue)])
    (cond
      ; not escape
      [(eq? (t:VarValue-escape? varvalue) #t)
       (let ([memres (make-temp-result)]
             [offsetres (make-temp-result)])
         (array-list-add-item! alist (i:loadI (make-frame-offset name) offsetres #f))
         (array-list-add-item! alist (i:loadAO Rarp offsetres memres))
         memres
         )]
      ; escape variables
      [else noderes])
    ))

(define (let->iloc node alist)
  (let ([decs (LetExpr-decs node)]
        [exprs (LetExpr-exprs node)])
    (for-each (λ (arg)
                (match arg
                  [(FunDecl _ _ _ _ _) (array-list-add-item! (funlist) arg)]
                  [else (ast->iloc! arg alist)])
                ) decs)
    (for-each (λ (arg) (ast->iloc! arg alist)) exprs)
    (let ([result (foldl (λ (arg default) (get-note arg 'result)) '() exprs)])
      (if (outer-let)
          (begin (outer-let #f) (return-proc result alist))
          '())
      result
      )))

;return process
(define (return-proc result alist)
  (let ([return-off (make-temp-result)]
        [retaddr-off (make-temp-result)]
        [retaddr (make-temp-result)]
        [parp-off (make-temp-result)]
        [parp (make-temp-result)])
    (if (eq? result '())
        '()
        (begin
          (array-list-add-item! alist (i:loadI return-value-offset return-off #f))
          (array-list-add-item! alist (i:storeAO result Rarp return-off))))
    ;return process
    (array-list-add-item! alist (i:loadI return-addr-offset retaddr-off #f))
    (array-list-add-item! alist (i:loadAO Rarp retaddr-off retaddr))
    (array-list-add-item! alist (i:loadI previous-arp parp-off #f))
    (array-list-add-item! alist (i:loadAO Rarp parp-off parp))
    ;jump back
    (array-list-add-item! alist (i:i2i parp Rarp #f))
    (array-list-add-item! alist (i:jump retaddr PC #f))
    ))

;(FunDecl name args rettype body next)
(define (fundecl->iloc node alist)
  (let* ([funty (get-note node 'funvalue)]
         [varvalues (t:FunValue-varvalues funty)]
         [retype (t:FunValue-return-type funty)]
         [name (FunDecl-name node)]
         [body (FunDecl-body node)]
         [next (FunDecl-next node)]
         [funres (make-global-result)])
    ;set label
    ;(t:set-FunValue-label! funty funres)
    (array-list-add-item! (labelist)
                          (LIpair
                           (Label (string-append
                                   (Label-name (t:FunValue-label funty)) ": "
                                   (symbol->string name))) (array-list-length alist)))
    ;make temp result for each argument
    (for-each (λ (arg) (t:set-VarValue-result! arg (make-temp-result))) varvalues)
    
    ;body of function
    (ast->iloc! body alist)
    ;return
    (let* ([retres (get-note body 'result)])
      (return-proc retres alist))
    
    ;next field
    (if (equal? next '())
        '()
        (fundecl->iloc next))

    ;result, already been added to node
    funres
  ))

;function call expression
(define (funcall->iloc node alist)
  (let* ([name (FuncallExpr-name node)]
         [listarg (FuncallExpr-args node)]
         [funval (get-note node 'funval)]
         [varvalues (t:FunValue-varvalues funval)]
         [label (t:FunValue-label funval)]
         [parp-off (make-temp-result)]
         [static-off (make-temp-result)]
         [result (make-temp-result)])
    ;move arp
    (array-list-add-item! alist (i:loadI previous-arp parp-off #f))
    (array-list-add-item! alist (i:storeAO Rarp Rarp parp-off))
    ;store parameters
    (for-each (λ (arg) (ast->iloc! arg alist)) listarg)
    ;count param size of non-escape parameters
    (let* ([paramsize 0]
           [size-off (make-temp-result)]
           [new-arp (make-temp-result)])
      (for-each (λ (param)
                  (if (t:VarValue-escape? param)
                      '()
                      (set! paramsize (+ paramsize 8)))
                      ) varvalues)
      ;build up frames with parameters
      (array-list-add-item! alist (i:loadI paramsize size-off #f))
      (array-list-add-item! alist (i:add Rarp size-off new-arp))
      (array-list-add-item! alist (i:i2i new-arp Rarp #f))
      )
    ;jump to function label
    (array-list-add-item! alist (i:jumpI (Result label #t #f) #f #f))
    ;restore arp with static link
    (array-list-add-item! alist (i:loadI static-link-offset static-off #f))
    (array-list-add-item! alist (i:loadAO Rarp static-off result))
    (array-list-add-item! alist (i:i2i result Rarp #f))
    ))
  