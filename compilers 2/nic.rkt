#lang racket

; required for the lexer
(require racket/exn
         "nilexer.rkt"
         "niparser.rkt"
         "errors.rkt"
         "nidotgen.rkt"
         "typecheck.rkt"
         (prefix-in llvm: "llvm-translator.rkt")
         "llvm-emitter.rkt"
         (prefix-in iloc: "iloc-trans.rkt"))


; parameters for setting up the compiler
(define verbose-mode (make-parameter #f))
(define tokens-only (make-parameter #f))
(define parse-tree-only (make-parameter #f))
(define generate-dotfile (make-parameter #f))
(define typecheck-only (make-parameter #f))
(define llvm-ir-only (make-parameter #f))
(define iloc-ir-only (make-parameter #t))
(define gen-iloc-ir (make-parameter #t))

; process command line will take a specification for the
; command line and then parse arguments (in theory)
; name: takes the name of the program you are parsing command line arguments, it should be a string
; argv: retreives the list of command line arguments, you usually pass (current-command-line-arguments)
; table: a list of three things, flag names, description, and an expression to evaluate if the flag
;        has been seen
; remaining-handler: a function that takes a single argument, which is a list of the remaining arguments
;                    after the other ones
;(define (process-command-line name argv table remaining-handler)
  
; look to see if this is a flag
(define (is-flag? str)
  (let ([m (regexp-match #px"-[[:alnum:]]" str)])
    (cond
      [(eq? m #f) (regexp-match #px"--([[:alnum:]])+" str)]
      [else m])))

  

; read command line arguments and do stuff!
(define files-to-compile
  (command-line
   #:program "Ni Compiler"
   #:once-each
   [("-v" "--verbose") "Compile with excessive messages" (begin (to-screen? #t) (verbose-mode #t))]
   [("-t" "--tokens-only") "Only run the lexing phase and print out the tokens" (tokens-only #t)]
   [("-p" "--parse-tree-only") "Only run until the parse tree phase and print it out" (begin (tokens-only #f) (parse-tree-only #t))]
   [("-d" "--generate-dot-file") "Generate a dot file and png of image" (generate-dotfile #t)]
   [("--typecheck-only") "End after typechecking phase" (typecheck-only #t)]
   [("--gen-llvm") "Generate llvm IR" (gen-iloc-ir #f)]
   [("--gen-iloc") "Generate iloc IR" (gen-iloc-ir #t)]
   [("-l" "--llvm-ir-only") "End after emitting llvm IR" (llvm-ir-only #t)]
   [("-i" "--iloc-ir-only") "End after emitting iloc IR" (iloc-ir-only #t)]
   #:args files
  files))




; compile anything using an input port
(define (compile-in in [name ""])
  ; reset important stuff like error counts
  (clear-errors)
  ; clear the writers too!
  (clear-writers)
  (let* ([filename (if (string=? "" name) "(from string)" name)]
         [dotmatch (regexp-match #px".*[.]ni" filename)]
         [dotname (if (eq? dotmatch #f) "out"
                      (substring (first dotmatch) 0 (- (string-length filename) 3)))])
    
    (port-count-lines! in)
    ; see if we only want to print out tokens
    (if (tokens-only)
        (printf "~a~n" (lex in))
        ; next phase, parsing
        (let ([ast (build-ast in)])
          (printf "...building the ast~n")
          (cond
            [(scan-error) (printf "~nEncountered lexing/scanning error, ending compilation of ~a~n" filename)]
            [(parse-error) (printf "~nEncountered parsing error, ending compilation of ~a~n" filename)]
            [else
             (begin
               ; if the generate-dotfile flag was set, draw it
               (cond [(generate-dotfile) (make-dot-file ast dotname)]
                     [else '()])
               (if (parse-tree-only)
                   (printf "~a~n" ast)
                   (let ([ty (begin (printf "...typechecking the ast~n") (typecheck-ast ast))])
                     (cond
                       [(typecheck-only) ty]
                       [else
                        (let ([outllvm (open-output-file (string-append dotname ".ll") #:mode 'text #:exists 'replace)])
                          (emission-port outllvm)
                          (printf "...generating llvm code~n")
                          (let ([ir (translate-ast->ir ast)])
                            (close-output-port outllvm)
                            (if (or (iloc-ir-only) (llvm-ir-only)) '()
                                (compile-llvm dotname))))]))))])))))

(define (translate-ast->ir ast)
  (if (gen-iloc-ir)
      (iloc:translate-ast ast)
      (llvm:translate-ast ast)))

; compile a single file, this will not catch exceptions
(define (compile-file file)
  (printf "~nCompiling ~a...~n" file)
  ; get the filename
  (let ([in (open-input-file file)])
    (compile-in in file)))

(define (compile-str str)
  (printf "~nCompiling the string ~a...~n" str)
  ; create a string input port
  (let ([in (open-input-string str)])
    (compile-in in)))


    
; compile a whole list of file names!
(define (compile-files files)
  (for-each (λ (file)
              (with-handlers ([exn:fail? (λ (e) (printf "~a~n" (exn->string e)))])
                (compile-file file)))
            files))

(define (get-llvm filename)
  (let ([os (system-type 'os)])
    (cond
      [(eq? os 'macosx) (string-append "cc " filename ".ll NiStdLib.c -o " filename)]
      [(eq? os 'windows) (string-append "clang " filename ".ll NiStdLib.c -o " filename)]
      [else (error "Unsuported os")])))

(define (compile-llvm filename)
  (printf "...generating executable called: ~a~n" filename)
  (let ([cmd (get-llvm filename)])
    (printf "executing: ~a to compile everything together!~n" cmd)
    (system cmd)
    (printf "done!")))

(compile-files files-to-compile)