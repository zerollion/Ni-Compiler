#lang racket

(require "ast-walk.rkt"
         "frame.rkt"
         (prefix-in t: "types.rkt")
         "names.rkt"
         "typecheck.rkt"
         "niparser.rkt")

(provide (all-defined-out))

(define (build-frames ast)
  ; mode is either 'pre or 'post
  (let* ([Lmain (Label "Lmain")]
         [framelist (list Lmain)]
         [currentfun '()]
         [walker
         (λ (node mode)
           (match node
             [(FunDecl names args rettype body next) ;cond on mode 'pre 'post
                (cond
                  [(eq? mode 'pre)
                   (let* ([funval (get-note node 'funval)]
                          [frame (make-frame-from-funvalue funval)])
                     ; counting frames
                     (set-Frame-static-link! frame (first framelist))
                     ; push framelist and funval-list
                     (set! currentfun (push-frame currentfun funval))
                     (set! framelist (push-frame framelist (Frame-name frame)))
                     (t:set-FunValue-label! funval (Frame-name frame))

                     ;parameters
                     (for-each (λ (ntPair)
                                 (let* ([varval (t:NameTypePair-type ntPair)])
                                   (t:set-VarValue-param?! varval #t) 
                                   (t:set-VarValue-level! varval frame)))
                               (t:FunValue-parameters funval))
                     )]

                  [(eq? mode 'post)
                   (begin
                     ;pop current frame and funval-list
                     (set! currentfun (pop-frame currentfun))
                     (set! framelist (pop-frame framelist))
                     )] 
                  )]

             ;variables
             [(VarDecl type id expr)
              (cond
                [(eq? mode 'pre)
                 (if (not (equal? currentfun '()))
                     (let* ([varval (get-note node 'var)]
                            [frame (t:FunValue-frame (first currentfun))])
                       (t:set-VarValue-level! varval frame)
                       (printf "Name: ~a; Decleared in ~a; level: ~a \n" id (t:FunValue-name (first currentfun)) (Label-name (Frame-name frame))))
                     (let* ([varval (get-note node 'var)]
                            [level (first framelist)])
                       (t:set-VarValue-level! varval (make-frame level))
                       (printf "Name: ~a; Decleared in ~a; level: ~a \n" id (Label-name level) (Label-name level)))
                   )
                 ])]

             [(VarExpr name)
              (cond
                [(eq? mode 'pre)
                 (let* ([varval (get-note node 'var)]
                        [level (Frame-name (t:VarValue-level varval))]
                        [frame
                         (if (empty? currentfun)
                             (make-frame Lmain)
                             (t:FunValue-frame (first currentfun)))])  ;possible bug: empty list
                   (if (equal? level (first framelist))
                       '()
                       (t:set-VarValue-escape?! varval #t)))
                 ])]
             
             [_ '()]
             ))]

         [print-fun
          (λ (node mode)
           (match node
             [(FunDecl names args rettype body next)
              (cond
                [(eq? mode 'pre)
                 (let* ([funval (get-note node 'funval)]
                        [frame (t:FunValue-frame funval)])
                   (output-frame (current-output-port) frame))])]
             [_ '()]
             ))]
         
         [print-var
          (λ (node mode)
            (match node
              [(FunDecl names args rettype body next)
              (cond
                [(eq? mode 'pre)
                 (let* ([funval (get-note node 'funval)]
                        [frame (t:FunValue-frame funval)])
                   ;print out peremeters
                   (for-each (λ (ntPair)
                               (let* ([varval (t:NameTypePair-type ntPair)]
                                      [name (t:NameTypePair-name ntPair)]
                                      [frame (t:VarValue-level varval)]
                                      [level (Frame-name frame)])
                                 (if (and (t:VarValue-escape? varval) (not (t:VarValue-alloc? varval)))
                                     (begin
                                       (t:set-VarValue-alloc?! varval #t)
                                       (t:set-VarValue-offset! varval (Frame-offset (t:VarValue-level varval)))
                                       (alloc-local! (t:VarValue-level varval) 'int)
                                       (printf "parameter: ~a escaped; level: ~a; offset ~a; \n"
                                               name (Label-name level) (t:VarValue-offset varval)))
                                     '()
                                     ) 
                                 ))
                             (t:FunValue-parameters funval))
                   )])]
              
              [(VarDecl type id expr)
               (cond
                 [(eq? mode 'pre)
                  (let* ([varval (get-note node 'var)]
                         [frame (t:VarValue-level varval)]
                         [level (Frame-name frame)])
                    (if (t:VarValue-escape? varval)
                        ;if escape? check if already allocated
                        (if (t:VarValue-alloc? varval)
                            '()
                            (begin
                              (t:set-VarValue-alloc?! varval #t)
                              (t:set-VarValue-offset! varval (Frame-offset (t:VarValue-level varval)))
                              (alloc-local! (t:VarValue-level varval) 'int)
                              ;check if it is a parameter
                              (printf "variable : ~a escaped; level: ~a; offset ~a; \n"
                                      id (Label-name level) (t:VarValue-offset varval))
                              )
                            )
                        (printf "variable : ~a in register; level: ~a;\n" id (Label-name (Frame-name frame)))))
                  ])]
              
              [_ '()]
              ))])
    (ast-walk walker ast)
    (ast-walk print-var ast)
    (ast-walk print-fun ast)))

(define (build-str str)
  (let ([ast (parse-str str)])
    (typecheck-ast ast)
    (build-frames ast)))