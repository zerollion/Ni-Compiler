#lang racket

;; the purpose of this module is to ease the emission of llvm code
(require "names.rkt"
         "types.rkt")

(define (register-result? res)
  (and (Result? res) (in-register? res)))

(define (frame-result? res)
  (and (Result? res) (in-frame? res)))

(define (option-result? res)
  (or (Result? res) (eq? res #f)))

(provide (all-defined-out))

; this port is used for writing to, by default it's to the screen
(define emission-port (make-parameter (current-output-port)))
; if true, output will also be sent to the screen, but note, if (emission-port) is
; a terminal port, then we'll ignore to-screen? (so we don't double print everything)
; on the other hand, if it isn't a terminal port, then we'll write to there
(define to-screen? (make-parameter #f))
; use the global writer when trying to write to the globals section of your file
(define global-writer (make-parameter (open-output-string)))
; use the fun writer when defining functions
(define fun-writer (make-parameter (open-output-string)))
; use the main writer for defining the main expression 
(define main-writer (make-parameter (open-output-string)))
; a stack of writers

(define (clear-writers)
  (global-writer (open-output-string))
  (fun-writer (open-output-string))
  (main-writer (open-output-string))
  (current-writer (main-writer)))

; the current writer is what you're currently writing to
(define current-writer (make-parameter (main-writer)))

; the collection of writers, we can push the current one, depending one what we're doing,
; and restore the last one when we're done--but note, this *won't* work for nested functions
; because the fun-writer assumes one function at a time using this style--you'd need a list
; or something of those writers so you could write them out sequentially 
(define writers (make-parameter '()))

  
; pushes the writer onto the list of writers
(define (push-writer wr)
  (writers (cons wr (writers))))

; pops and returns the last writer
(define (pop-writer)
  (let ([wr (first (writers))])
    (writers (rest (writers)))
    wr))
  
    
; these are just syntactic sugar to make it clear what we're doing
; we have two methods for starting and ending global data
(define (begin-global-defn)
  (push-writer (current-writer))
  (current-writer (global-writer)))

(define (end-global-defn)
  (current-writer (pop-writer)))

; two methods for starting and ending function data
(define (begin-fun-defn)
  (push-writer (current-writer))
  (current-writer (fun-writer)))
(define (end-fun-defn)
  (current-writer (pop-writer)))

; methods for handling main definitions
(define (begin-main-defn)
  (push-writer (current-writer))
  (current-writer (main-writer)))
(define (end-main-defn)
  (current-writer (pop-writer)))


; this should be called prior to any other emissions--it sets up the llvm file correctly
(define (emit-header)
  (begin-global-defn)
  (newline)
  (newline)
  (println ";;;;; START OF PROGRAM ;;;;;")
  (println "; target data layout for Mac, change to m:w instead for Windows" )
  (println "target datalayout = \"e-m:w-i64:64-f80:128-n8:16:32:64-S128\"" )
  (newline )
  ;(println "; target triple for Mac" )
  ;(println "target triple = \"x86_64-apple-macosx10.12.0\"" )
  (newline )

  ; output the struct defn for strings
  (println "; STRUCTS for strings and arrays")
  (println "%struct.string = type { i64, i8* }" )

  ; output struct defns for arrays
  (println "%struct.array = type { i64, i64* }" )

  ; now emit functions from the standard library 
  (emit-standard-library)

  ; note that globals are up and coming
  (println "; GLOBAL variables, defined in the program" )

  (end-global-defn))

; this emits headers for the standard library, which is written in C. We just compile
; it with cc -c NiStdLib.c to get a NiStdLib.o file, which we can then compile along
; with the generated source, so cc NiStdLib.o *.ll
(define (emit-standard-library)  
   ; now just linking with the object file, seems easier, less chance for error
  (println "; STANDARD LIB DECLARATIONS")
  (println "declare %struct.string* @makeString(i8* %str)")
  (println "declare %struct.string* @getChar()")
  (println "declare i64 @ord(%struct.string* nocapture readonly %str)")
  (println "declare %struct.string* @chr(i64 %i)")
  (println "declare i64 @size(%struct.string* nocapture readonly %str)")
  (println "declare %struct.string* @substring(%struct.string* nocapture readonly %str, i32 %first, i32 %n)")
  (println "declare %struct.string* @concat(%struct.string* nocapture readonly %s1, %struct.string* nocapture readonly %s2)")
  (println "declare void @Exit(i64 %i)")
  (println "declare void @print(%struct.string* nocapture readonly %str)")
  (println "declare void @printi(i64 %v) #0")
  (println "declare %struct.string* @intToString(i64 %val)")
  (println "declare %struct.array* @makeArray(i64 %numElements)")
  (println "declare i64* @getElementAddressAt(%struct.array* %arr, i64 %index)")
  (println "; Function Attrs: nounwind")
  (println "declare noalias i8* @malloc(i64) #1"))

; emits the header for main, which is required as the entry point into a program
; in essence, much of the code in the let body or expression will appear here
(define (emit-main-header)
  (newline)
  (println "; Function Attrs: nounwind ssp uwtable")
  (println "define i32 @main(i32 %argc, i8** %argv) #0 {")
  (println "%1 = alloca i32, align 4")
  (println "%2 = alloca i32, align 4")
  (println "%3 = alloca i8**, align 8")
  (println "store i32 0, i32* %1")
  (println "store i32 %argc, i32* %2, align 4")
  (println "store i8** %argv, i8*** %3, align 8")
  (newline)
  (println "; Ni program to follow..."))

; emits code that ends the ni program, basically a return from main, plus loads of other stuff
(define (emit-main-trailer)
  (println "; ... end Ni program")
  (println "ret i32 0")
  (println "}")

  (println "attributes #0 = { nounwind ssp uwtable ")
  (println "\"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" ")
  (println "\"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" ")
  (println "\"no-nans-fp-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" ")
  (println "\"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" } ")
  (println ";;;;; END OF PROGRAM ;;;;;")
  (newline)
  (newline))


; this is called when you're finally done to wrap things up
(define (finish-emission)
  (cond
    [(and (to-screen?) (not (eq? (current-output-port) (emission-port))))
     (begin
       (display (get-output-string (global-writer)))
  
       (displayln "; FUNCTIONS")
       (display (get-output-string (fun-writer)))
  
       (displayln "; MAIN")
       (display (get-output-string (main-writer))))])
  
  (display (get-output-string (global-writer)) (emission-port))
  
  (displayln "; FUNCTIONS" (emission-port))
  (display (get-output-string (fun-writer)) (emission-port))
  
  (displayln "; MAIN" (emission-port))
  (display (get-output-string (main-writer)) (emission-port)))
 
; try to simplify output, don't double print though
(define (println str . rest)
  (displayln (string-append* str rest) (current-writer)))

(define (newline)
  (displayln "" (current-writer)))

; try to simplify output, don't double print though
(define (print str . rest)
  (display (string-append* str rest) (current-writer)))


;;; these are helper functions to emit code for what they do
(define (emit-comment str)
  (newline)
  (println "; " str))


; Usage: to emit proper llvm math expressions 
; emit-math: symbol? (or Result? string?) (or Result? String?) Result? -> void
; this function will emit a math llvm ir sequence if you pass it one
; of the following as the first arg: 'add 'sub 'mul 'div, generally
; you use this by passing two result arguments to it and it will return
; the third one back to you (LLVM requires new names, we assume these go into
; register temps and to save some work we create it for you)
(define (mathsym? sym)
  (and (symbol? sym) (or (eq? sym 'add) (eq? sym 'sub) (eq? sym 'mul) (eq? sym 'div))))

(define (emit-math mathsym v1 v2 [result (make-temp-result)])
  (let ([v1str (if (Result? v1) (result->string v1) v1)]
        [v2str (if (Result? v2) (result->string v2) v2)])
    (let ([resstr (result->string result)]
          [tyname "i64 "])
      (cond
        [(not (and (mathsym? mathsym) (or (string? v1) (Result? v1)) (or (string? v2) (Result? v2))))
         (raise-arguments-error 'emit-math "mathsym should be a symbol and v1 and v2 should be Result (or string) types"
                                "mathsym" mathsym
                                "v1" v1
                                "v2" v2)]
        [(eq? mathsym 'add) (println resstr " = add " tyname v1str ", " v2str)]
        [(eq? mathsym 'sub) (println resstr " = sub " tyname v1str ", " v2str)]
        [(eq? mathsym 'mul) (println resstr " = mul " tyname v1str ", " v2str)]
        [(eq? mathsym 'div) (println resstr " = sdiv " tyname v1str ", " v2str)]
        [else (raise-arguments-error 'emit-math "mathsym must be 'add, 'sub, 'mul, or 'div"
                                     "mathsym" mathsym)])
      ; return the result that was created
      result)))

(define (boolsym? sym)
  (and (symbol? sym) (or (eq? sym 'eq) (eq? sym 'lt) (eq? sym 'gt) (eq? sym 'ge) (eq? sym 'le) (eq? sym 'ne))))

(define (emit-bool boolsym v1 v2 type [result (make-temp-result)])
  (cond
    [(IntType? type)
     (let ([v1str (if (Result? v1) (result->string v1) v1)]
           [v2str (if (Result? v2) (result->string v2) v2)])
       (let ([resstr (result->string result)]
             [tyname "i64 "])
         (cond
           [(not (and (boolsym? boolsym) (or (string? v1) (Result? v1)) (or (string? v2) (Result? v2))))
            (raise-arguments-error 'emit-bool "boolsym should be a symbol and v1 and v2 should be Result (or string) types"
                                   "boolsym" boolsym
                                   "v1" v1
                                   "v2" v2)]
           [(eq? boolsym 'eq) (println resstr " = icmp eq " tyname v1str ", " v2str)]
           [(eq? boolsym 'lt) (println resstr " = icmp slt " tyname v1str ", " v2str)]
           [(eq? boolsym 'gt) (println resstr " = icmp sgt " tyname v1str ", " v2str)]
           [(eq? boolsym 'ge) (println resstr " = icmp sge " tyname v1str ", " v2str)]
           [(eq? boolsym 'le) (println resstr " = icmp sle " tyname v1str ", " v2str)]
           [(eq? boolsym 'ne) (println resstr " = icmp ne " tyname v1str ", " v2str)]
           [else (raise-arguments-error 'emit-bool "boolsym must be 'eq, 'lt, 'gt, 'ge, 'le, or 'ne"
                                        "boolsym" boolsym)])
         ; return the result that was created
         result))]
    
    [(StringType? type)
     (let ([result (make-temp-result)]
           [resstr (result->string result)])
       (println ";string comparism, calling stringCompare()")
       (println resstr " = call i64 @stringCompare( "
                (get-type-name type) " " (result->string v1) ", "
                (get-type-name type) " " (result->string v2) " )")
       result)]
    ))

(define (logicsym? sym)
  (and (symbol? sym) (or (eq? sym 'and) (eq? sym 'or))))

(define (emit-logic logicsym v1 v2 [result (make-temp-result)])
  (let ([v1str (if (Result? v1) (result->string v1) v1)]
        [v2str (if (Result? v2) (result->string v2) v2)])
    (let ([resstr (result->string result)]
          [tyname "i1 "])
      (cond
        [(not (and (logicsym? logicsym) (or (string? v1) (Result? v1)) (or (string? v2) (Result? v2))))
         (raise-arguments-error 'emit-logic "logicsym should be a symbol and v1 and v2 should be Result (or string) types"
                                "logicsym" logicsym
                                "v1" v1
                                "v2" v2)]
        [(eq? logicsym 'and) (println resstr " = and " tyname v1str ", " v2str)]
        [(eq? logicsym 'or)  (println resstr " = or " tyname v1str ", " v2str)]
        [else (raise-arguments-error 'emit-logic "logicsym must be 'and/'or"
                                     "logicsym" logicsym)])
      ; return the result that was created
      result)))

(define (emit-boolval val)
  (let* ([result (make-temp-result)]
         [resstr (result->string result)]
         [tyname "i1 "])
    (if (eq? val #t)
        (println resstr " = add " tyname "1, 0")
        (println resstr " = add " tyname "0, 0"))
    result
    ))

(define (emit-literal-string val)
  (let*([str (substring val 1 (sub1 (string-length val)))]
        [length (sub1 (string-length val))]
        [strval (make-global-result)]
        [struct (make-global-result)]
        ; replace every "\n" with "\0A" for newline in LLVM
        [llvmstr (string-replace (substring val 1 (sub1 (string-length val))) "\\n" "\\0A")]
        ; we multiply the diff of the new string length minus the old one
        ; (after subbing 2 because of quotes on the old one) because
        ; "\n" has a length of 2, according to racket, while "\0A" has a
        ; length of 3, thus we need to remove 2 characters for every
        ; "\n" we replaced to get the right string length according to llvm
        [lenval (add1 (- (string-length llvmstr)
                        (* 2 (- (string-length llvmstr) ( - (string-length val) 2)))))])
    (begin-global-defn)
    (let ([L1 (result->string strval)]
          [L2 (result->string struct)]
          [strres (string-append " c\"" llvmstr "\\00\"")]
          [type1 (string-append "[" (number->string lenval) " x i8]")]
          [type2 "%struct.string"])
      (println L1 " = global " type1 strres ", align 1")
      (println L2 " = global " type2 "{ i64 " (number->string (sub1 lenval))
               ", i8* getelementptr inbounds (" type1 ", " type1 "* " L1 ", i32 0, i32 0) }, align 8")
      (end-global-defn)
      struct
      )
    ))


(define (emit-funcall name nodelist typelist rettype funty)
  (begin
    (println ";Function Call on:" (symbol->string name))
    (let* ([namestr (symbol->string name)]
           [retstr (get-type-name rettype)]
           [temp (make-temp-result)]
           [tempstr (result->string temp)]
           [register (FunValue-result funty)])
           ;[namestring (if (equal? register '()) namestr (result->string register))])
      (if (equal?  register '())
          (if (equal? retstr "void")
              (print "call void @" namestr "( ")
              (print tempstr " = call " retstr " @" namestr "( "))
          (if (equal? retstr "void")
              (print "call void " (result->string register) "( ")
              (print tempstr " = call " retstr " " (result->string register) "( ")))
      
      (if (empty? nodelist)
          (print "")
          (begin
            ;first
            (let ([nodestr (result->string (first nodelist))]
                  [typestr (get-type-name (first typelist))])
              (print typestr " " nodestr)
              )
            ;rest
            (for-each (λ (node type)
                        (let ([nodestr (result->string node)]
                              [typestr (get-type-name type)])
                          (print ", " typestr " " nodestr)
                          )) (rest nodelist) (rest typelist))
          ))
      (println " )")
      (if (equal? retstr "void")
          #f
          temp)
   )))

(define (emit-vardecl name type node)
  (let* ([nodestr (result->string node)]
         [typestr (get-type-name type)]
         [local (make-label-result)]
         [localstr (result->string local)])
    (println "; variable declaration of " (symbol->string name))
    (println localstr " = alloca " typestr ", align 8")
    (println "store " typestr " " nodestr ", " typestr "* " localstr)
    local
  ))

(define (emit-var varval node)
  (let* ([nodestr (result->string node)]
         [typestr (get-type-name (VarValue-type varval))]
         [temp (make-temp-result)]
         [tempstr (result->string temp)])
    (if (load? node (VarValue-type varval))
        (begin
          (println tempstr " = load " typestr ", " typestr "* " nodestr)
          temp)
        node)
    ))

(define (load? result type)
  (or (global? result) (in-frame? result) (StringType? type) (ArrayType? type)))

(define (emit-assignexpr varval varnode exprnode)
  (let* ([nodestr (result->string varnode)]
         [typestr (get-type-name (VarValue-type varval))]
         [exprnodestr (result->string exprnode)]
         [temp (make-temp-result)]
         [tempstr (result->string temp)])
    (println ";Assignment")
    (println "store " typestr " " exprnodestr ", " typestr "* " nodestr)
    temp
   ))

;if else then-----------------------------
(define (emit-test testresult L1 L2)
  (let ([resultstr (result->string testresult)]
        [L1str (result->string L1)]
        [L2str (result->string L2)])
    (println "br i1 " resultstr ", label " L1str ", label " L2str)
  ))

(define (emit-label label)
  (let* ([labelstr (result->string label)]
         [lstr (string-replace labelstr "%" "")])
    (println " ")
    (println lstr ":")
  ))

(define (emit-branch label)
  (println "br label " (result->string label))
  )

(define (emit-ifphi type true-res L1 false-res L2)
  (let* ([result (make-temp-result)]
         [resstr (result->string result)]
         [typestr (get-type-name type)]
         [tstr (result->string true-res)]
         [fstr (result->string false-res)]
         [L1str (result->string L1)]
         [L2str (result->string L2)])
    (println resstr " = phi " typestr " [ " tstr ", " L1str " ], [ " fstr ", " L2str " ]")
    result
  ))
;end if else then-------------------------------

(define (emit-fundecl name funty  nodelist typelist)
  (let* ([func (make-global-result)]
         [funstr (result->string func)]
         [type (get-type-name (FunValue-return-type funty))])
    ;add the result to FunValue
    (set-FunValue-result! funty func)
    (begin-fun-defn)
    (println " ")
    (println ";Function declaration on :" (symbol->string name))
    (print "define " type " " funstr "(")
    ;arguments
    (if (empty? nodelist)
        (print "")
        (begin
          ;first
          (let ([nodestr (result->string (first nodelist))]
                [typestr (get-type-name (first typelist))])
            (print typestr " " nodestr)
            )
          ;rest
          (for-each (λ (node type)
                      (let ([nodestr (result->string node)]
                            [typestr (get-type-name type)])
                        (print ", " typestr " " nodestr)
                        )) (rest nodelist) (rest typelist))
          ))
    (println " ) {")
    func
  ))

(define (emit-fundecl-body retres rettype)
  (begin
    (if (or (eq? retres '()) (eq? retres #f))
        (println "ret void")
        (let ([ret (result->string retres)]
              [type (get-type-name rettype)])
          (println "ret " type " " ret)
      ))
    (println "}")
    (println " ")
    (end-fun-defn)
    ))

(define (get-type-name nitype)
  (let ([ty (actual-type nitype)])
    (match ty
      [(IntType _) "i64"]
      [(BoolType _) "i1"]
      [(StringType _) "%struct.string *"]
      [(VoidType _) "void"]
      [(ArrayType _ _ _) "%struct.array *"]
      ; you'll want to add records here...
      [_ (error "get-type-name not worknig for your type!")])))
                     
;clang



