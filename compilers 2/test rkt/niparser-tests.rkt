#lang racket/base

(require rackunit
         rackunit/text-ui
         (prefix-in lex: parser-tools/lex)
         "niparser.rkt")

(define parser-tests
  (test-suite
   "Tests for the Ni Parser"

   ; strings
   (check-equal? (parse-str "\"Hello World\"") (list (StringExpr "\"Hello World\"")) "string literal expression")
   ; noval
   (check-equal? (parse-str "()") (list (NoVal)) "no val expression")
   (check-equal? (parse-str "5") (list (NumExpr "5")) "int expression")
   (check-equal? (parse-str "true") (list (BoolVal #t)) "bool literal expression")
   (check-equal? (parse-str "false") (list (BoolVal #f)) "bool literal expression")
   
   ; parens
   (check-equal? (parse-str "(5)") (list (NumExpr "5")) "testing parenthesis")

   ; math ops
   (check-equal? (parse-str "2-5*3")
                 (list (MathExpr (NumExpr "2") '- (MathExpr (NumExpr "5") '* (NumExpr "3")))) "math precedence")
   (check-equal? (parse-str "2*5-3")
                 (list (MathExpr (MathExpr (NumExpr "2") '* (NumExpr "5")) '- (NumExpr "3"))) "math precedence")
   (check-equal? (parse-str "2-5*3-6")
                 (list (MathExpr (MathExpr (NumExpr "2") '-
                                           (MathExpr (NumExpr "5") '* (NumExpr "3")))
                                 '- (NumExpr "6"))) "math precedence")
   (check-equal? (parse-str "1-2-3")
                 (list (MathExpr (MathExpr (NumExpr "1") '- (NumExpr "2")) '- (NumExpr "3"))) "left associativity")
   ; math ops using negated numbers
   (check-equal? (parse-str "-5") (list (MathExpr (NumExpr "0") '- (NumExpr "5"))) "negated numbers")
   (check-equal? (parse-str "-5-6") (list (MathExpr (MathExpr (NumExpr "0") '- (NumExpr "5")) '- (NumExpr "6"))) "negated numbers")
   (check-equal? (parse-str "-5*6") (list (MathExpr (MathExpr (NumExpr "0") '- (NumExpr "5")) '* (NumExpr "6"))) "negated numbers")

   
   ; bool expressions
   (check-equal? (parse-str "5=6") (list (BoolExpr (NumExpr "5") 'eq (NumExpr "6"))) "bool expressions")
   (check-equal? (parse-str "5<6") (list (BoolExpr (NumExpr "5") 'lt (NumExpr "6"))) "bool expressions")
   (check-equal? (parse-str "5>6") (list (BoolExpr (NumExpr "5") 'gt (NumExpr "6"))) "bool expressions")
   (check-equal? (parse-str "5<=6") (list (BoolExpr (NumExpr "5") 'le (NumExpr "6"))) "bool expressions")
   (check-equal? (parse-str "5>=6") (list (BoolExpr (NumExpr "5") 'ge (NumExpr "6"))) "bool expressions")
   (check-equal? (parse-str "5<>6") (list (BoolExpr (NumExpr "5") 'ne (NumExpr "6"))) "bool expressions")

   ; little more complicated demonstrating order of ops
   (check-equal? (parse-str "3+2*6>-4+3")
                 (list (BoolExpr
                        (MathExpr (NumExpr "3") '+ (MathExpr (NumExpr "2") '* (NumExpr "6")))
                        'gt (MathExpr (MathExpr (NumExpr "0") '- (NumExpr "4")) '+ (NumExpr "3"))))
                 "bool expressions have lower precedence than maht ops")


   ;(list (MathExpr (MathExpr (NumExpr "0") '- (NumExpr "5")) '* (NumExpr "6"))))
   (check-equal? (parse-str "-add(2)") (list (MathExpr (NumExpr "0") '- (FuncallExpr 'add (list (NumExpr "2")))))
                 "negation with function call")

    ; let expressions, doesn't have a body
   (check-equal? (parse-str "let ni int x is 5 in end")
                 (list (LetExpr (list (VarDecl 'int 'x (NumExpr "5"))) '()))
                 "simple var declaration in a let (which doesn't have a body)")
   (check-equal? (parse-str "let ni int x is 5 in 5 end")
                 (list (LetExpr (list (VarDecl 'int 'x (NumExpr "5"))) (list (NumExpr "5"))))
                 "simple var declaration in a let (which doesn't have a body)")
   (check-equal? (parse-str "let ni x is 5 in 5; 6 end")
                 (list (LetExpr (list (VarDecl #f 'x (NumExpr "5")))
                                (list (NumExpr "5") (NumExpr "6"))))
                 "let expression with multiple body parts")
  
   ; var declarations
   (check-equal? (parse-str "let ni x is 5 in end") (list (LetExpr (list (VarDecl #f 'x (NumExpr "5"))) '()))
                 "var declaration without type")
   (check-equal? (parse-str "let ni int x is 5 in end") (list (LetExpr (list (VarDecl 'int 'x (NumExpr "5"))) '()))
                 "var declaration with type")

   ; type declarations
   (check-equal? (parse-str "let define int2 kind as int in end") (list (LetExpr (list (NameType 'int2 'int '())) '()))
                 "simple name type")
   (check-equal? (parse-str "let define int2 kind as int and define int3 kind as int2 in end")
                 (list (LetExpr (list (NameType 'int2 'int (NameType 'int3 'int2 '()))) '()))
                 "mutually recursive name types")
   (check-equal? (parse-str "let define intarr kind as array of int in end")
                 (list (LetExpr (list (ArrayType 'intarr 'int '())) '()))
                 "array declaration")
   (check-equal? (parse-str "let define rec kind as { } in end")
                 (list (LetExpr (list (RecordType 'rec '() '())) '()))
                 "record without fields")
   (check-equal? (parse-str "let define intrec kind as { int x } in end")
                 (list (LetExpr (list (RecordType 'intrec (list (TypeField 'x 'int)) '())) '()))
                 "record declaration with a single field")
   (check-equal? (parse-str "let define point kind as { int x, int y } in end")
                 (list (LetExpr (list (RecordType 'point (list (TypeField 'x 'int) (TypeField 'y 'int)) '())) '()))
                 "record with two fields")
   
   ; function declarations
   (check-equal? (parse-str "let neewom no() is () in end")
                 (list (LetExpr (list (FunDecl 'no '() #f (NoVal) '())) '()))
                 "function decl with no args and no return type (void)")
   (check-equal? (parse-str "let neewom getX() as int is 5 in end")
                 (list (LetExpr (list (FunDecl 'getX '() 'int (NumExpr "5") '())) '()))
                 "function decl without parameters")
   (check-equal? (parse-str "let neewom add2(int x) as int is 2 in end")
                 (list (LetExpr (list (FunDecl 'add2 (list (TypeField 'x 'int)) 'int (NumExpr "2") '())) '()))
                 "function decl with 1 parameter")
   (check-equal? (parse-str "let neewom add2(int x, int y) as int is 0 in end")
                 (list (LetExpr (list (FunDecl 'add2 (list (TypeField 'x 'int) (TypeField 'y 'int)) 'int (NumExpr "0") '())) '()))
                 "function decl with 2 parameters")
   (check-equal? (parse-str "let neewom getX() as int is 5 and neewom add2(int x) as int is 2 in end")
                 (list (LetExpr (list (FunDecl 'getX '() 'int (NumExpr "5")
                                               (FunDecl 'add2 (list (TypeField 'x 'int)) 'int (NumExpr "2") '()))) '()))
                 "mutually recursive function decls")
   
   ; function calls of various sorts
   (check-equal? (parse-str "getX()") (list (FuncallExpr 'getX '())) "funcall, no args")
   (check-equal? (parse-str "add2(5)") (list (FuncallExpr 'add2 (list (NumExpr "5")))) "funcall, 1 arg")
   (check-equal? (parse-str "add2(5, 7)") (list (FuncallExpr 'add2 (list (NumExpr "5") (NumExpr "7"))))
                 "funcall, 2 args")
   (check-equal? (parse-str "printi(5 < 6)") (list (FuncallExpr 'printi (list (BoolExpr (NumExpr "5") 'lt (NumExpr "6")))))
                 "funcall with logic expression")
   
   ; various sequences
   (check-equal? (parse-str "(6; 5)") (list (list (NumExpr "6") (NumExpr "5"))) "sequence of 2 things")
   (check-equal? (parse-str "(getX(); 5)") (list (list (FuncallExpr 'getX '()) (NumExpr "5"))) "sequence with funcall")
   (check-equal? (parse-str "(5; 6; 7)") (list (list (NumExpr "5") (NumExpr "6") (NumExpr "7"))) "sequence of 3 things")
   (check-equal? (parse-str "(getX(); 5; 6)") (list (list (FuncallExpr 'getX '()) (NumExpr "5") (NumExpr "6")))
                 "funcall in seq of 3 things")
  
   
   ; new array creation
   (check-equal? (parse-str "intarr[10] of 6")
                 (list (NewArrayExpr 'intarr (NumExpr "10") (NumExpr "6")))
                 "creating an array with simple expression as count")
   (check-equal? (parse-str "intarr[x+6] of 6")
                 (list (NewArrayExpr 'intarr (MathExpr (VarExpr 'x) '+ (NumExpr "6"))
                                     (NumExpr "6")))
                 "creating an array with more complex expression as count")

   ; new record expression
   (check-equal? (parse-str "point {}")
                 (list (NewRecordExpr 'point '()))
                 "creating a record with no fields")
   (check-equal? (parse-str "point { x is 6, y is 42 }")
                 (list (NewRecordExpr 'point (list (FieldAssign 'x (NumExpr "6"))
                                                   (FieldAssign 'y (NumExpr "42")))))
                 "creating a record with two fields")
   

   ; record field access
   (check-equal? (parse-str "pt.x")
                 (list (RecordExpr (VarExpr 'pt) 'x))
                 "record dot notation")
   (check-equal? (parse-str "x[3].y")
                 (list (RecordExpr (ArrayExpr (VarExpr 'x) (NumExpr "3")) 'y))
                 "record dot notation")
   

   ; if expressions
   ;(check-equal? (parse-str "if a then b") (list (IfExpr (VarExpr "a") (VarExpr "b") '())))
   ;(check-equal? (parse-str "if a then b else c") (list (IfExpr (VarExpr "a") (VarExpr "b") (VarExpr "c"))))
   ; dangling else--an else should match the most recent if
   ;(check-equal? (parse-str "if a then if b then s1 else s2")
   ;              (list (IfExpr (VarExpr "a") (IfExpr (VarExpr "b") (VarExpr "s1") (VarExpr "s2")) '())))
   ; more complicated
   ;(check-equal? (parse-str "if a then b + 1")
   ;              (list (IfExpr (VarExpr "a") (MathExpr (VarExpr "b") '+ (NumExpr "1")) '())))
   ;(check-equal? (parse-str "if a then b + 1 else c + 1")
   ;              (list (IfExpr (VarExpr "a") (MathExpr (VarExpr "b") '+ (NumExpr "1"))
   ;                            (MathExpr (VarExpr "c") '+ (NumExpr "1")))))
   ;(check-equal? (parse-str "if a then if b then c else d + 1")
   ;              (list (IfExpr (VarExpr "a")
   ;                            (IfExpr (VarExpr "b")
   ;                                    (VarExpr "c")
   ;                                    (MathExpr (VarExpr "d") '+ (NumExpr "1")))
   ;                            '())))
   ; if with end 
   (check-equal? (parse-str "if a then b end") (list (IfExpr (VarExpr 'a) (VarExpr 'b) '()))
                 "if then")
   (check-equal? (parse-str "if a then b else c end") (list (IfExpr (VarExpr 'a) (VarExpr 'b) (VarExpr 'c)))
                 "if then else")
   ; dangling else--an else should match the most recent if
   (check-equal? (parse-str "if a then if b then s1 else s2 end end")
                 (list (IfExpr (VarExpr 'a) (IfExpr (VarExpr 'b) (VarExpr 's1) (VarExpr 's2)) '()))
                 "nest if then else")
   ; more complicated
   (check-equal? (parse-str "if a then b + 1 end")
                 (list (IfExpr (VarExpr 'a) (MathExpr (VarExpr 'b) '+ (NumExpr "1")) '()))
                 "precedence test with math and if")
   (check-equal? (parse-str "if a then b + 1 else c + 1 end")
                 (list (IfExpr (VarExpr 'a) (MathExpr (VarExpr 'b) '+ (NumExpr "1"))
                               (MathExpr (VarExpr 'c) '+ (NumExpr "1"))))
                 "precedence test with math and if")
   (check-equal? (parse-str "if a then if b then c else d + 1 end end")
                 (list (IfExpr (VarExpr 'a)
                               (IfExpr (VarExpr 'b)
                                       (VarExpr 'c)
                                       (MathExpr (VarExpr 'd) '+ (NumExpr "1")))
                               '()))
                 "precedence test with math and if")

   (check-equal? (parse-str "1 + if 1 then 1 else 2 end")
                 (list (MathExpr (NumExpr "1") '+
                                 (IfExpr (NumExpr "1") (NumExpr "1") (NumExpr "2"))))
                 "precedence test with math and if")

   (check-equal? (parse-str "if 1 then 1 else 2 end + 1")
                 (list (MathExpr (IfExpr (NumExpr "1") (NumExpr "1") (NumExpr "2")) '+
                                 (NumExpr "1")))
                 "precedence test with math and if")
              

   ; assignment
   (check-equal? (parse-str "now x is 5") (list (AssignmentExpr (VarExpr 'x) (NumExpr "5")))
                 "simple assignment")
   (check-equal? (parse-str "now x is x + 1")
                 (list (AssignmentExpr (VarExpr 'x) (MathExpr (VarExpr 'x) '+ (NumExpr "1"))))
                 "assignment has lower precedence than math")
   ; more complicated, a common error would be to have MathExpr at the top level
   (check-equal? (parse-str "now x is if x = 0 then 0 else 1 end + 1")
                 (list (AssignmentExpr (VarExpr 'x)
                                       (MathExpr
                                        (IfExpr
                                         (BoolExpr (VarExpr 'x) 'eq (NumExpr "0"))
                                         (NumExpr "0")
                                         (NumExpr "1")) '+ (NumExpr "1"))))
                 "more complicated assignment, don't want math expression at top level")

   ; while expressions (without end)
   ;(check-equal? (parse-str "while t do x") (list (WhileExpr (VarExpr 't) (VarExpr 'x))))
   ;(check-equal? (parse-str "while x < 5 do now x is x + 1")
   ;              (list (WhileExpr (BoolExpr (VarExpr 'x) 'lt (NumExpr "5"))
   ;                               (AssignmentExpr (VarExpr 'x) (MathExpr (VarExpr 'x) '+ (NumExpr "1"))))))
   ;(check-equal? (parse-str "while x < 5 do x + 1")
   ;             (list (WhileExpr (BoolExpr (VarExpr 'x) 'lt (NumExpr "5"))
   ;                              (MathExpr (VarExpr 'x) '+ (NumExpr "1")))))

   ; while with end
   (check-equal? (parse-str "while 1 do () end") (list (WhileExpr (NumExpr "1") (NoVal)))
                 "simple while expression")
   (check-equal? (parse-str "while x < 5 do now x is x + 1 end")
                 (list (WhileExpr (BoolExpr (VarExpr 'x) 'lt (NumExpr "5"))
                                  (AssignmentExpr (VarExpr 'x) (MathExpr (VarExpr 'x) '+ (NumExpr "1")))))
                 "more complicated while")
   (check-equal? (parse-str "while x < 5 do x + 1 end")
                 (list (WhileExpr (BoolExpr (VarExpr 'x) 'lt (NumExpr "5"))
                                  (MathExpr (VarExpr 'x) '+ (NumExpr "1"))))
                 "more complex while")

   ; with expressions (without end)
   ;(check-equal? (parse-str "with x as 1 to 5 do print(x)")
   ;              (list (WithExpr 'x (NumExpr "1") (NumExpr "5") (FuncallExpr 'print (list (VarExpr 'x))))));

   ;(check-equal? (parse-str "with i as 25 * x to (42 * 20) - 1 do print(x)")
   ;              (list (WithExpr "i" (MathExpr (NumExpr "25") '* (VarExpr 'x))
   ;  (MathExpr (MathExpr (NumExpr "42") '* (NumExpr "20")) '- (NumExpr "1"))
   ;  (FuncallExpr "print" (list (VarExpr 'x))))))

   ; with expressions with end
   (check-equal? (parse-str "with i as 1 to 5 do () end")
                 (list (WithExpr 'i (NumExpr "1") (NumExpr "5") (NoVal)))
                 "simple with expression")
   
   (check-equal? (parse-str "with x as 1 to 5 do print(x) end")
                 (list (WithExpr 'x (NumExpr "1") (NumExpr "5") (FuncallExpr 'print (list (VarExpr 'x)))))
                 "with expression has a body")

   (check-equal? (parse-str "with i as 25 * x to (42 * 20) - 1 do print(x) end")
                 (list (WithExpr 'i (MathExpr (NumExpr "25") '* (VarExpr 'x))
                                 (MathExpr (MathExpr (NumExpr "42") '* (NumExpr "20")) '- (NumExpr "1"))
                                 (FuncallExpr 'print (list (VarExpr 'x)))))
                 "with expressions with expressions for tests")

   ; break expressions
   (check-equal? (parse-str "break") (list (BreakExpr)) "simple break expression")
   (check-equal? (parse-str "while x < 5 do (if x = 3 then break end; now x is x + 1) end")
                 (list (WhileExpr (BoolExpr (VarExpr 'x) 'lt (NumExpr "5"))
                                  (list (IfExpr (BoolExpr (VarExpr 'x) 'eq (NumExpr "3")) (BreakExpr) '())
                                        (AssignmentExpr (VarExpr 'x) (MathExpr (VarExpr 'x) '+ (NumExpr "1"))))))
                 "more complicated with expression")

   ; peng expressions
   (check-equal? (parse-str "peng") (list (PengExpr)) "simple peng expression")
   (check-equal? (parse-str "let ni pengrec r is peng in end")
                 (list (LetExpr (list (VarDecl 'pengrec 'r (PengExpr))) '()))
                       "testing peng with records")

   ; not expressions
   (check-equal? (parse-str "not true") (list (LogicExpr (BoolVal #t) 'not #f)))
   (check-equal? (parse-str "not 5 < 6") (list (LogicExpr (BoolExpr (NumExpr "5")
                                                                    'lt (NumExpr "6")) 'not #f)))
   
   ))

(run-tests parser-tests)

   