#lang racket

;Typr checker
;Tan Zhen

(require "niparser.rkt"
         "environment.rkt"
         "errors.rkt"
         "log.rkt"
         rackunit
         (prefix-in types: "types.rkt")
         test-engine/racket-tests)

(provide (all-defined-out))

(define (typecheck ast env loop)
  (match ast
    ; the base case for lists, this will just be the void type
    ['() (types:make-VoidType)]
    [(NoVal) (types:make-VoidType)]

    ; base cases for types
    [(NumExpr val) (types:make-IntType)]
    [(StringExpr v) (types:make-StringType)]
    [(PengExpr) (types:make-PengType)]
     
    [(VarExpr name) (let ([t1 (apply-env env name)])
                      (if (eq? t1 #f)
                          (log-typeerror "variable not declared using 'ni' before use" ast)
                          (types:VarValue-type t1)))] ;(types:VarValue-type t1)

    ;record types
    [(NameType name kind next)
     (begin
       (extend-env env name (types:make-NameType))
       (if (empty? next) '() (typecheck next env loop))
       (let ([t1 (apply-env env kind)] [t3 (apply-env env name)])
         (if (or (eq? t1 #f) (eq? name 'int) (eq? name 'string) (eq? name 'boolean))
             (log-typeerror "NameType: declared type does not exist." ast)
             ;(extend-env env name (types:NameType t1))
             (types:set-NiType-actual! t3 (types:NameType t1))
             ))
       ;(with-handlers ([exn:fail? (λ (exn) (log-typeerror "NameType: must go through array." ast))])
         ;(types:actual-type (apply-env env kind)))
        )]

    [(ArrayType name kind next)
     (let ([t1 (apply-env env kind)])
       (if (empty? next) '() (typecheck next env loop))
       (if (or (eq? t1 #f) (eq? name 'int) (eq? name 'string) (eq? name 'boolean))
           (log-typeerror "ArrayType: declared type does not exist." ast)
           (extend-env env name (types:ArrayType '() t1)));(types:actual-type t1)
        )]

    [(TypeField name kind)
     (let ([t1 (apply-env env kind)])
       (if (or (eq? t1 #f) (eq? name 'int) (eq? name 'string) (eq? name 'boolean))
           (log-typeerror "TypeField: declared type does not exist." ast)
           (types:NameTypePair t1 name))
        )]

    [(RecordType name field next)
     (begin
       ; extend the type to be name type
       (extend-env env name (types:make-NameType))
       (if (empty? next) '() (typecheck next env loop))
       (let ([t2 (map (λ (arg) (typecheck arg env loop)) field)] [t3 (apply-env env name)])
         (types:set-NiType-actual! t3 (types:RecordType '() t2))
         ;(extend-env env name (types:RecordType '() t2))
       ))]

    ;new record expr
    [(NewRecordExpr name assignments)
     (let ([t1 (apply-env env name)])
       (if (eq? t1 #f)
           (log-typeerror "new record: record type does not exist." ast)
           (begin
             (for-each (λ (arg1 arg2)
                       (if (equal? (FieldAssign-name arg1) (types:NameTypePair-name arg2))
                           (let ([p1 (types:actual-type (typecheck (FieldAssign-expr arg1) env loop))]
                                 [p2 (types:actual-type (types:NiType-actual arg2))])
                           (if (or (equal? p1 p2)
                                   (and (equal? p1 (types:make-PengType))(types:RecordType? p2)))
                               '() ;check pass. do nothing
                               (log-typeerror "FieldAssign: field does not exist." ast)))
                           (log-typeerror "FieldAssign: field does not exist." ast))
               )assignments (types:RecordType-fields (types:actual-type t1)))
             (types:actual-type t1))
           )
        )]

    ;new array expression
    [(NewArrayExpr name expr kind)
     (let ([t1 (apply-env env name)] [t2 (typecheck kind env loop)])
       (if (eq? t1 #f)
           (log-typeerror "new array: array type does not exist." ast)
           (if (or (equal? (types:actual-type (types:ArrayType-element-type (types:actual-type t1))) t2)
                   (equal? t2 (types:make-PengType)))
               (types:actual-type t1)
               (log-typeerror "new array: array type does not match with assignment expression." ast)
            ))
      )]

    ;record field access
    [(RecordExpr name field)
     (let ([t1 (typecheck name env loop)])
       (let ([t2 (if (eq? t1 #f)
                     (log-typeerror "record access: record was not declared." ast)
                     (types:fieldcheck field (types:RecordType-fields (types:actual-type t1))) ;field check functions
                     )])
         (if (eq? t2 #f)
             (log-typeerror "record access: field accessed does not exist." ast)
             (types:actual-type t2)))
      )]

    ;array/subscript access
    [(ArrayExpr name expr)
     (let ([t1 (typecheck name env loop)] [t2 (typecheck expr env loop)])
       (if (eq? t1 #f)
           (log-typeerror "array/subscript access: array varible does not exist/not array type." ast)
           (if (equal? t2 (types:make-IntType))
               (if (types:ArrayType? (types:actual-type t1))
                   (types:ArrayType-element-type (types:actual-type t1))
                   (log-typeerror "array/subscript access: varible not array type." ast))
               (log-typeerror "array/subscript access: index is not int." ast)))
      )]
    
    ;Bool types
    [(BoolVal expr1) (types:make-BoolType)]
    [(BoolExpr expr1 op expr2)
     (let ([e1 (typecheck expr1 env loop)] [e2 (typecheck expr2 env loop)])
       (if (or (and (equal? (types:actual-type e1) (types:make-IntType))
                    (equal? (types:actual-type e2) (types:make-IntType)))
               (and (equal? (types:actual-type e1) (types:make-StringType))
                    (equal? (types:actual-type e2) (types:make-StringType)))
               (and (equal? (types:actual-type e1) (types:make-PengType))
                    (types:RecordType? (types:actual-type e2)))
               (and (equal? (types:actual-type e2) (types:make-PengType))
                    (types:RecordType? (types:actual-type e1))))
       (types:make-BoolType)
       (log-typeerror "Bool Exprssion contains types other than int." ast))
       )]
    
    [(LogicExpr expr1 op expr2)
     (let ([e1 (typecheck expr1 env loop)] [e2 (typecheck expr2 env loop)])
       (if (and (equal? (types:actual-type e1) (types:make-BoolType))
                (equal? (types:actual-type e2) (types:make-BoolType)))
       (types:make-BoolType)
       (log-typeerror "Logic Exprssion contains types other than bool." ast))
       )]

    ;math expressions
    [(MathExpr expr1 op expr2)
     (let ([e1 (typecheck expr1 env loop)] [e2 (typecheck expr2 env loop)])
       (if (and (equal? (types:actual-type e1) (types:make-IntType))
                (equal? (types:actual-type e2) (types:make-IntType)))
       (types:make-IntType)
       (log-typeerror "Math Exprssion contains types other than int." ast))
       )]

    ;var declaration
    [(VarDecl type id expr)
     (let ([t1 (typecheck expr env loop)] [t2 (apply-env env type)])       
       ;check recordtype name
       (if (and (NewRecordExpr? expr) (not(eq? type #f)))
           (if (equal? type (NewRecordExpr-name expr))
               '() (log-typeerror "VarDecl: declared type does match." ast))
           '())
       ;check array type name
       (if (and (NewArrayExpr? expr) (not(eq? type #f)))
           (if (or (equal? type (NewArrayExpr-name expr))
                   (types:NameType? (apply-env env (NewArrayExpr-name expr))))
               '() (log-typeerror "VarDecl: declared type does match." ast))
           '())
       ;name matches
       (if (or (eq? type #f) (equal? (types:actual-type t1) (types:actual-type t2))
               (and (types:ArrayType? (types:actual-type t2)) (types:PengType? t1))
               (and (types:RecordType? (types:actual-type t2)) (types:PengType? t1)))
           (if (eq? t2 #f)
               (extend-env env id (types:VarValue t1 #f))
               (extend-env env id (types:VarValue (types:actual-type t2) #f)))
           (log-typeerror "VarDecl: declared type does not exist." ast)
           )
      )]

    ;let expressions
    [(LetExpr decs exprs)
     (let ([new-env (push-scope env)])
       (let ([d1 (map (λ (arg) (typecheck arg new-env loop)) decs)])
         (let ([b1 (foldl (λ (arg2 last) (typecheck arg2 new-env loop)) (types:make-VoidType) exprs)])
           b1))
      )]

    ;if expression
    [(IfExpr test true-branch false-branch)
     (let ([t1 (typecheck test env loop)] [b1 (typecheck true-branch env loop)] [b2 (typecheck false-branch env loop)])
       (if (equal? t1 (types:make-BoolType))
           (if (equal? '() false-branch)
               (if (equal? b1 (types:make-VoidType))
                   (types:make-VoidType)
                   (log-typeerror "if expression: not having a false-branch and ture-branch not void." ast))
               (if (equal? b1 b2)
                   (types:actual-type b1)
                   (log-typeerror "if expression: ture-branch and false-branch do not match." ast)))
           (log-typeerror "if expression: test is not a bool type" ast))
       )]

    ;assignment expression
    [(AssignmentExpr name expr)
     (if (and (eq? loop #t) ;check whether in a loop
              (eq? (types:VarValue-iter (apply-env env (VarExpr-name name))) #t)) ;check whether is a iterator
         (log-typeerror "assignment expression: cannot do assignmentto the iterator." ast)
         (let ([t1 (typecheck name env loop)] [t2 (typecheck expr env loop)])
           (if (eq? t1 #f)
               (log-typeerror "assignment expression: variable does not exist." ast)
               (cond 
                 [(equal? t1 t2) (types:make-VoidType)]
                 [(and (types:RecordType? t1) (types:PengType? t2)) (types:make-VoidType)]
                 [(and (types:RecordType? t2) (types:PengType? t1)) (types:make-VoidType)]
                 [(and (types:ArrayType? t1) (types:PengType? t2)) (types:make-VoidType)]
                 [(and (types:ArrayType? t2) (types:PengType? t1)) (types:make-VoidType)]
                 [else (log-typeerror "assignment expression: variable does not match exprssion type." ast)])
               ))
      )]

    ;with loop
    [(WithExpr idname initexpr toexpr body)
     (let ([new-env (push-scope env)])
       (extend-env new-env idname (types:VarValue (types:make-IntType) #t))
       (let ([t1 (typecheck initexpr new-env loop)] [t2 (typecheck toexpr new-env loop)] [b1 (typecheck body new-env #t)])
         (if (and (equal? t1 (types:make-IntType)) (equal? t2 (types:make-IntType)))
             (if (equal? b1 (types:make-VoidType))
                 (types:make-VoidType)
                 (log-typeerror "with loop: With cannot return or assign value." ast))
             (log-typeerror "with loop: Iterator must be int type." ast)))
       )]

    ;while loop
    [(WhileExpr test body)
     (let ([t1 (typecheck test env loop)] [t2 (typecheck body env #t)])
       (if (equal? t1 (types:make-BoolType))
           (if (equal? t2 (types:make-VoidType))
               (types:make-VoidType)
               (log-typeerror "with expression: body must be void type." ast))
           (log-typeerror "with expression: test is not a bool type." ast))
      )]

    ;break expression
    [(BreakExpr)
     (if (eq? loop #t)
         (types:make-VoidType)
         (log-typeerror "break expression: break can only be inside a loop." ast)
      )]

    ;function decl
    [(FunDecl name args rettype body next) ;push-scope before the body
     (let ([a1 (map (λ (arg) (typecheck arg env loop)) args)] [new-env (push-scope env)])
       ;extend scope with arguments
       (for-each (λ (nt)
                   (extend-env new-env
                               (types:NameTypePair-name nt)
                               (types:VarValue(types:NiType-actual nt) #f))) a1)
       
       ;typecheck
       (let ([r1 (if (eq? rettype #f)
                     (types:make-VoidType)
                     (apply-env new-env rettype))])
         (extend-env env name (types:FunValue a1 r1))
         (if (not (empty? next))
             (typecheck next env loop) '()) ;typecheck the next field
         (let ([r2 (typecheck body new-env #f)]) ;entered the body mark false(cannot break a loop)
           (if (equal? r1 r2)
               (extend-env env name (types:FunValue a1 r2))
               (log-typeerror "function decl: body and return type do not match." ast)))
             ) 
       )]

    ;function call
    [(FuncallExpr name args)
     (let ([t1 (apply-env env name)])
       (if (eq? t1 #f)
           (log-typeerror "function call: function name does not exist." ast)
           (begin
             (if (equal? (length args) (length (types:FunValue-parameters (types:actual-type t1))))
                 (for-each (λ (a1 a2)
                             (if (or (equal? (typecheck a1 env loop) (types:actual-type (types:NiType-actual a2)))
                                     (and (equal? (typecheck a1 env loop) (types:make-PengType))
                                          (types:RecordType? (types:actual-type (types:NiType-actual a2)))))
                                 '() ;check pass, do nothing
                                 (log-typeerror "function call: argument types do not match." ast))
                             )args (types:FunValue-parameters (types:actual-type t1)))
                 (log-typeerror "function call: argument number does not match." ast))
             (types:FunValue-return-type (types:actual-type t1)))
           )
      )]

    ;list of exprssions
    [(list expr ___)
     (foldl (λ (arg last) (typecheck arg env loop)) (types:make-VoidType) expr)]

    ;unbounded cases
    [_ (begin
         (displayln ast)
         (error "Node not implemented yet!"))]
  ))

;utility functions
(define (tc-str str)
  (let ([env (empty-env)])
    (clear-errors)
    (extend-env env 'int (types:make-IntType))
    (extend-env env 'string (types:make-StringType))
    (extend-env env 'boolean (types:make-BoolType))
    (extend-env env 'bool (types:make-BoolType))
    (extend-env env 'print (types:FunValue (list (types:NameTypePair (types:make-StringType) 'sss)) (types:make-VoidType)))
    (typecheck (first (parse-str str)) env #f)))

(define (tc-file filename)
  (let ([env (empty-env)])
    (clear-errors)
    (extend-env env 'int (types:make-IntType))
    (extend-env env 'string (types:make-StringType))
    (extend-env env 'boolean (types:make-BoolType))
    (extend-env env 'bool (types:make-BoolType))
    (extend-env env 'print (types:FunValue (list (types:NameTypePair (types:make-StringType) 'sss)) (types:make-VoidType)))
    (typecheck (first (parse-file filename)) env #f)))

;---------------------------check expects------------------------------
;;; checking expressions
   #|(check-exn exn:fail? (thunk (tc-str "
let
  define e kind as { int x }
  ni e x is e { x is 7 }
in
  x
end")) "x is kind e, but no longer in scope")
   (check-exn exn:fail? (thunk (tc-str "
let
  define color kind as { int r, int g, int b }
  define point kind as { int x, int y, int z }
  define dot kind as { point p, color c }
  ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
in d.p end")) "d.p is kind point, but that's not in scope when let exits")

   (check-exn exn:fail? (thunk (tc-str "
let
  define color kind as { int r, int g, int b }
in
  let
    define point kind as { int x, int y, int z }
  in
    let
       define dot kind as { point p, color c }
       ni dot d is dot { p is point { x is 1, y is 2, z is 3 }, c is color { r is 1, g is 1, b is 1 } }
    in
      d.c
    end
  end
end")) "nested lets where color escapes body of outer let")

; this one might get errors saying print hasn't been defined, this is
   ; because these functions are part of the standard library and should
   ; have function headers in the value environment 
   (check-equal? (tc-str "
let
  ni N is 9

  define intArray kind as array of int

  ni row is intArray [ N ] of 0
  ni col is intArray [ N ] of 0
  ni diag1 is intArray [ N + N - 1] of 0
  ni diag2 is intArray [ N + N - 1] of 0

  neewom printboard () is 
    (with i as 0 to N - 1 do
      (with j as 0 to N - 1 do
        print(if col[i] = j then \" 0\" else \" .\" end)
       end;
       print(\"\n\"))
     end;
     print(\"\n\"))

  neewom try (int c) is
    if c = N - 1
    then printboard()
    else with r as 0 to N - 1 do
              if row[r] = 0 & diag1[r + c] = 0 & diag2[r + 7 - c] = 0 
              then (now row[r] is 1;
                    now diag1[r + c] is 1;
                    now diag2[r + 7 - c] is 1;
                    now col[c] is r;
                    try(c + 1);
                    now row[r] is 0;
                    now diag1[r + c] is 0;
                    now diag2[r + 7 - c] is 0)
              end
         end
    end
  in
    try(0)
end
") (types:make-VoidType) "the beautiful 8 queens example")
(check-equal? (tc-str "
// mutually recursive types successfully going through array
let
    define c kind as b and
    define b kind as d and
    define d kind as array of c
in
end") (types:make-VoidType)
      "test55.ni: mutually recursive types successfully going through array, this means you can assign peng to arrays")

   ; test 56.ni
   (check-equal? (tc-str "
// succesful mutually recursive types that go through record
let
    define c kind as b and
    define b kind as { d x, c y} and
    define d kind as c
in
end") (types:make-VoidType)
      "test56.ni: succesful mutually recursive types that go through record")

   ; test57.ni
   (check-equal? (tc-str "
// a more complicated test that checks for nested record field access, should pass typechecking
let
    define vec4 kind as { int x, int y, int z, int w }
    define mat4 kind as { vec4 row1, vec4 row2, vec4 row3, vec4 row4 }

    neewom vec4add (vec4 v, vec4 v') as vec4 is
        vec4 { x is v.x + v'.x,
               y is v.y + v'.y,
               z is v.z + v'.z,
               w is v.w + v'.w }

    neewom mat4add (mat4 m1, mat4 m2) as mat4 is
        mat4 { row1 is vec4add(m1.row1, m2.row1),
               row2 is vec4add(m1.row2, m2.row2),
               row3 is vec4add(m1.row3, m2.row3),
               row4 is vec4add(m1.row4, m2.row4) }


    ni identity is mat4 { row1 is vec4 { x is 1, y is 0, z is 0, w is 0 },
                          row2 is vec4 { x is 0, y is 1, z is 0, w is 0 },
                          row3 is vec4 { x is 0, y is 0, z is 1, w is 0 },
                          row4 is vec4 { x is 0, y is 0, z is 0, w is 1 } }


in
    mat4add(identity, identity);

    // if you're resolving fields properly, this will work, if not, you'll get a type error
    identity.row1.x + identity.row2.x
end") (types:make-IntType)
      "test57.ni: a more complicated test that checks for nested record field access")|#

(test)