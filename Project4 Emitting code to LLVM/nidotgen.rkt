#lang racket
(require "niparser.rkt")
(require 2htdp/image)

(provide (all-defined-out))

(define count 0)

(define (next-node)
  (set! count (add1 count))
  (string-append "n" (number->string count)))

(define (generate-dot ast out)
  (let ([preamble (open-output-string)]
        [body (open-output-string)])
    (fprintf preamble "digraph ast {~n")
    (letrec ([match-ast
              (lambda (ast-node parent)
                (let ([nodename (next-node)])
                  ; we should generate a list of decs and such
                  (match ast-node
                    ['() (begin
                           (fprintf preamble "~n")
                           (fprintf body "~n"))]
                    [(list head rest ...)
                     ; walk through the list because it's a sequence in theory
                     (let ()
                       (if (eq? rest '())
                           (match-ast head parent)
                           (begin
                             (fprintf preamble "~a [label=\"~a\"];~n" nodename "SeqExpr")
                             (fprintf body "~a -> " nodename)
                             (match-ast head nodename)
                             (fprintf body "~a -> " nodename)
                             (match-ast rest nodename))))]
                       
                    [(VarExpr name) (let ([nodeval (next-node)])
                                      (fprintf preamble "~a [label=\"~a\"];~n" nodename "VarExpr")
                                      (fprintf preamble "~a [label=\"~a\"];~n" nodeval name)
                                      (fprintf body " ~a -> ~a;~n" nodename nodeval))]
                            
                    [(NumExpr val) (let ([nodeval (next-node)])
                                     (fprintf preamble "~a [label=\"~a\"];~n" nodename "NumExpr")
                                     (fprintf preamble "~a [label=\"~a\"];~n" nodeval val)
                                     (fprintf body " ~a -> ~a;~n" nodename nodeval))]

                    [(NoVal) (begin
                             (fprintf preamble "~a [label=\"NoValExpr\"];~n" nodename)
                             (fprintf body " ~a;~n" nodename))]

                    [(BreakExpr) (begin
                             (fprintf preamble "~a [label=\"Break\"];~n" nodename)
                             (fprintf body " ~a;~n" nodename))]

                    [(PengExpr) (begin
                             (fprintf preamble "~a [label=\"Peng\"];~n" nodename)
                             (fprintf body " ~a;~n" nodename))]
                    
                    
                    [(StringExpr val) (let ([nodeval (next-node)])
                                        (fprintf preamble "~a [label=\"~a\"];~n" nodename "StringExpr")
                                        (fprintf preamble "~a [label=~a];~n" nodeval val)
                                        (fprintf body " ~a -> ~a;~n" nodename nodeval))]
                            
                    [(MathExpr e1 op e2) (let ()
                                           (fprintf preamble "~a [label=\"~a\"];~n" nodename (symbol->string op))
                                           ; now do left child then right child
                                           (fprintf body " ~a -> " nodename)
                                           (match-ast e1 nodename)
                                           (fprintf body " ~a -> " nodename)
                                           (match-ast e2 nodename))]
                    [(IfExpr t tb fb) (let ()
                                        (fprintf preamble "~a [label=\"~a\"];~n" nodename "IfExpr")
                                        (fprintf body " ~a -> " nodename)
                                        (match-ast t nodename)
                                        (fprintf body " ~a -> " nodename)
                                        (match-ast tb nodename)
                                        (if (not (eq? fb '()))
                                            (begin (fprintf body " ~a -> " nodename)
                                                   (match-ast fb nodename)) (fprintf body "")))]
                    [(BoolExpr e1 op e2) (let ()
                                           (fprintf preamble "~a [label=\"~a\"];~n" nodename (symbol->string op))
                                           ; now do left child then right child
                                           (fprintf body " ~a -> " nodename)
                                           (match-ast e1 nodename)
                                           (fprintf body " ~a -> " nodename)
                                           (match-ast e2 nodename))]
                    [(LogicExpr e1 op e2) (let ()
                                           (fprintf preamble "~a [label=\"~a\"];~n" nodename (symbol->string op))
                                           ; now do left child then right child
                                           (fprintf body " ~a -> " nodename)
                                           (match-ast e1 nodename)
                                           (fprintf body " ~a -> " nodename)
                                           (match-ast e2 nodename))]
                    [(RecordExpr name field)
                     (let ([fieldnode (next-node)])
                       (fprintf preamble "~a [label=\"RecordExpr\"];~n" nodename)
                       (fprintf preamble "~a [label=\"~a\"];~n" fieldnode field)
                       
                       (fprintf body "~a -> " nodename)
                       (match-ast name nodename)
                       (fprintf body "~a -> ~a;~n" nodename fieldnode))]
                       
                       
                    
                    [(AssignmentExpr name expr) (let ()
                                                  (fprintf preamble "~a [label=\"AssignmentExpr\"];~n"
                                                           nodename)
                                                  (fprintf body " ~a -> " nodename)
                                                  (match-ast name nodename)
                                                  (fprintf body " ~a -> " nodename)
                                                  (match-ast expr nodename)
                                                  )]
                    [(WhileExpr test expr) (let ()
                                             (fprintf preamble "~a [label=\"While\"];~n" nodename)
                                             (fprintf body " ~a -> " nodename)
                                             (match-ast test nodename)
                                             (fprintf body "// while body~n")
                                             (fprintf body " ~a -> " nodename)
                                             (match-ast expr nodename))]
                    [(WithExpr id init end expr)
                     (let ()
                       (fprintf preamble "~a [label=\"WithExpr\"];~n" nodename)
                       (fprintf body "~a -> ~a;~n" nodename id)
                       (fprintf body "~a -> " nodename)
                       (match-ast init nodename)
                       (fprintf body "~a -> " nodename)
                       (match-ast end nodename)
                       (fprintf body "// with body~n")
                       (fprintf body "~a -> " nodename)
                       (match-ast expr nodename))]
                            
                    [(FuncallExpr name args)
                     (let ()
                       (fprintf preamble "~a [label=\"Funcall: ~a\"];~n" nodename name)
                       (if (> (length args) 0)
                           (for-each (λ (arg)
                                       (fprintf body "~a -> " nodename)
                                       (match-ast arg nodename)) args)
                           (fprintf body "~a;~n" nodename)))]
                               
                    [(ArrayExpr name expr)
                     (begin
                       (fprintf preamble "~a [label=\"ArrayExpr\"];~n" nodename)
                       (fprintf body "~a -> " nodename)
                       (match-ast name nodename)
                       (fprintf body "~a -> " nodename)
                       (match-ast expr nodename))]

                    [(NameType name kind next)
                     (let ([kindnode kind])
                       (fprintf preamble "~a [label=\"~a (kind)\"];~n" nodename name)
                       (fprintf preamble "~a [label=\"~a\"];~n" kindnode kind)
                       ; end the sequence of the parent, which may have been started by next
                       (if (not (eq? parent '()))
                           (begin
                             (fprintf body "~a; ~n" nodename)
                             (fprintf body "~a -> ~a; ~n" nodename parent))
                           '())
                       (fprintf body "~a -> ~a;~n" nodename kindnode)
                       
                       (if (not (eq? next '()))
                           (begin
                             (fprintf body "~a -> " nodename)
                             (match-ast next nodename))
                           '()))]

                    [(ArrayType name kind next)
                     (let ([kindnode (next-node)])
                       (fprintf preamble "~a [label=\"~a\"] ;~n" nodename name)
                       (fprintf preamble "~a [label=\"(array of) ~a\"];~n" kindnode kind)
                       ; end the sequence of the parent, which may have been started by next
                       (if (not (eq? parent '()))
                           (begin
                             (fprintf body "~a; ~n" nodename)
                             (fprintf body "~a -> ~a;  ~n" nodename parent))
                           '())
                       (fprintf body "~a -> ~a;~n" nodename kindnode)
                       
                       (if (not (eq? next '()))
                           (begin
                             (fprintf body "~a -> " nodename)
                             (match-ast next nodename))
                           '()))]

                    [(RecordType name fields next)
                     (let ()
                       (fprintf preamble "~a [label=\"~a (record)\"] ;~n" nodename name)
                       (for-each (λ (arg)
                                   (match-ast arg nodename)) fields)

                       ; check to see if we have a parent that called us
                       (if (not (eq? parent '()))
                           (begin
                             (fprintf body "~a; ~n" nodename)
                       +      (fprintf body "~a -> ~a;  ~n" nodename parent))
                           '())

                       ; recursively match siblings
                       (if (not (eq? next '()))
                           (begin
                             (fprintf body "~a -> " nodename)
                             (match-ast next nodename))
                           '()))]
                    
                    ; type fields in a record
                    [(TypeField name kind)
                     (let ([kindnode (next-node)])
                       (fprintf preamble "~a [label=\"~a\"] ;~n" nodename name)
                       (fprintf preamble "~a [label=\"~a\"] ;~n" kindnode kind)
                       (fprintf body "~a -> ~a -> ~a;~n" parent nodename kindnode))]

                    ; variable declarations
                    [(VarDecl kind id expr)
                     (let ([kindnode (next-node)])
                       (fprintf preamble "~a [label=\"VarDecl ~a\"] ;~n" nodename id)
                       (fprintf preamble "~a [label=\"~a\"];~n" kindnode (if (eq? kind #f) "(undef)" kind))
                       (fprintf body "~a -> ~a;~n" nodename kindnode)
                       (fprintf body "~a -> " nodename)
                       (match-ast expr nodename))]

                    ; function declarations
                    [(FunDecl name args rettype expr next)
                     (let ([count (length args)])
                       (fprintf preamble "~a [label=\"~a(" nodename name)
                       (for-each (λ (arg)
                                   (fprintf preamble "~a ~a" (TypeField-kind arg) (TypeField-name arg) )
                                   (set! count (sub1 count))
                                   (fprintf preamble "~a" (if (> count 0) ", " ""))) args)
                       (fprintf preamble ")\"];~n")
                       (fprintf body "~a -> " nodename)
                       (match-ast expr nodename)

                       (if (not (eq? parent '()))
                           (begin
                             (fprintf body "~a; ~n" nodename)
                             (fprintf body "~a -> ~a;  ~n" nodename parent))
                           '())
                       
                       ; recursively match siblings
                       (if (not (eq? next '()))
                           (begin
                             (fprintf body "~a -> " nodename)
                             (match-ast next nodename))
                           '()))]

                    ; let declarations, yay!
                    [(LetExpr decs exprs)
                     (begin
                       (fprintf preamble "~a [label=\"Let\"];~n" nodename)
                       (fprintf body "// let decs~n")
                       (for-each (λ (arg)
                                   (fprintf body "~a -> " nodename)
                                   (match-ast arg nodename)) decs)
                       (fprintf body "// let body~n")
                       (for-each (λ (arg)
                                 (fprintf body "~a -> " nodename)
                                 (match-ast arg nodename)) exprs))]

                    ; creation of new records
                    [(NewRecordExpr name assignments)
                     (begin
                       (fprintf preamble "~a [label=\"NewRecord: ~a\"];~n" nodename name)
                       (for-each (λ (arg)
                                   (fprintf body "~a -> " nodename)
                                   (match-ast arg nodename)) assignments))]
                    [(FieldAssign name expr)
                     (begin
                       (fprintf preamble "~a [label=\"Field: ~a\"];~n" nodename name)
                       (fprintf body "~a;~n//field:~n~a ->" nodename nodename)
                       (match-ast expr nodename))]
                       
                    [(NewArrayExpr name num-elements init-value)
                     (begin
                       (fprintf preamble "~a [label=\"Array of ~a\"];~n" nodename name)
                       (fprintf body "~a -> " nodename)
                       (match-ast num-elements nodename)
                       (fprintf body "~a -> " nodename)
                       (match-ast init-value nodename))]

                    
                       
                    [_ (error "node not implemented yet")]
                    )))])
(match-ast ast '()))
(fprintf body "~n}~n")
(fprintf out "~a" (get-output-string preamble))
(fprintf out "~a" (get-output-string body))))

; takes an ast and a filename and turns it into a dot file                    
(define (make-dot-file ast filename)
  (let ([out (open-output-file filename #:mode 'text #:exists 'replace)])
    (generate-dot ast out)
    (close-output-port out)
    (system (string-append "\"D:/Program Files (x86)/Graphviz2.38/bin/dot.exe\" -Tpng " filename " -o " filename ".png"))
    (bitmap/file (string-append filename ".png"))))

; takes the name of a file to process and produces a dot file using
; that filename and extended with .dot and .dot.png
; string (filename) -> .dot and .png
(define (nifile->dot filename)
  (let ([ast (parse-file filename)]
        [out (open-output-file (string-append filename ".dot") #:mode 'text #:exists 'replace)]
        [imgfilename (string-append filename ".png")])
    (generate-dot ast out)
    (close-output-port out)
    (system (string-append "export PATH=$PATH:/usr/local/bin; dot -Tpng " (string-append filename ".dot") " -o " imgfilename))
    ; this just reads it in to display it in racket (=
    (bitmap/file (string-append imgfilename))))

; takes a directory name, finds all the files that end in .ni and runs nifile->dot on them,
; thereby generating a .png for each of them
(define (makedots-fromdir dirname)
  (for-each (λ (pathname)
              (printf "Creating dot and png for: ~a~n" (path->string pathname)) 
              (nifile->dot (path->string pathname)))
            (find-files (λ (file)
                          ; make sure it's not a directory
                          (if (and (not (directory-exists? file)) (string-suffix? (path->string file) ".ni"))
                              #t #f)) dirname)))