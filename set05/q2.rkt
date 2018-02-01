;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide lit
         literal-value
         var
         variable-name
         op
         operation-name
         call
         call-operator
         call-operands
         block
         block-var
         block-rhs
         block-body
         literal?
         variable?
         operation?
         call?
         block?
         variables-defined-by
         variables-used-by
         constant-expression?
         constant-expression-value)


(check-location "05" "q2.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Literal is represented as a struct:
;; (make-literal value)

;; INTERP:
;; value : Real    the value that the literal represents.

;; IMPLEMENTATION:
(define-struct literal (value))

;; CONSTRUCTOR TEMPLATE :
;; (make-literal Real)

;; OBSERVER TEMPLATE:
;; literal-fn : Literal -> ??
;;(define (literal-fn l)
;;  (...
;;   (literal-value l)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;A Variable is represented as a struct:
;;(make-variable name)

;;INTERP:
;; name : String    represents the name of the variable which begins with a
;;                  letter and contains nothing but letters and digits.

;;IMPLEMENTATION :
(define-struct variable (name))

;;CONSTRUCTOR TEMPLATE:
;;(make-variable String)

;;OBSERVER TEMPLATE:
;; variable-fn : Variable -> ??
;;(define (variable-fn v)
;;  (...
;;   (variable-name v)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An OperationName is represented as one of the following strings:
;;     -- "+"      (indicating addition)
;;     -- "-"      (indicating subtraction)
;;     -- "*"      (indicating multiplication)
;;     -- "/"      (indicating division)
;;
;; OBSERVER TEMPLATE:
;; operation-name-fn : OperationName -> ??
;;(define (operation-name-fn op)
;;  (cond ((string=? op "+") ...)
;;        ((string=? op "-") ...)
;;        ((string=? op "*") ...)
;;        ((string=? op "/") ...)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An Operation is represented as a struct:
;;(make-operation name)

;; INTERP:
;; name : OperationName   the name of the operation. Like addition or division

;;IMPLEMENTATION :
(define-struct operation (name))

;; CONSTRUCTOR TEMPLATE:
;; (make-operator OperationName)

;;OBSERVER TEMPLATE:
;; operation-fn : Operation -> ??
;;(define (operation-fn op)
;;  (...
;;   (operation-name op)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Call is represented as a struct:
;;(make-call-data operator operands)

;;INTERP:
;; operator : Operation                  represents the operation that has to
;;                                       be applied to the operands of the call.
;; operands : ArithmenticExpressionList  represents the operands in the call on
;;                                       which the operation is applied.

;;IMPLEMENTATION:
(define-struct call-data (operator operands))

;;CONSTRUCTOR TEMPLATE:
;;for Call:
;;(make-call Operation LiteralList)
;;
;;for ArithmeticExpressionList:
;; -- empty
;; -- (cons ArithmeticExpression ArithmeticExpressionList)

;;OBSERVER TEMPLATE:
;;for call :
;; call-fn : Call -> ??
;;(define (call-fn c)
;;  (...
;;   (call-data-operator c)
;;   (call-data-operands c)))

;;for ArithmeticExpressionList :
;; ael-fn : ArithmeticExpressionList -> ??
;;(define (ael-fn lst)
;;  (cond
;;    [(empty? lst) ...]
;;    [else (... (ArithmeticExpression-fn (first lst))
;;               (ael-fn (rest lst)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Block is represented as a struct:
;;(make-block-data var rhs body)

;;INTERP:
;; var  : Variable                  represents the variable of the block.
;; rhs  : ArithmeticExpression      represents the expression whose value will
;;                                  become the value of the variable defined by
;;                                  the block.
;; body : ArithmeticExpression      represents the expression of the block.

;;IMPLEMENTATION:
(define-struct block-data (var rhs body))

;;CONSTRUCTOR TEMPLATE:
;;(make-block Variable ArithmeticExpression ArithmeticExpression)

;;OBSERVER TEMPLATE:
;; block-fn : Block -> ??
;;(define (block-fn b)
;;  (...
;;   (block-data-var b)
;;   (block-data-rhs b)
;;   (block-data-body b)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;; An ArithmeticExpression is one of
;;     -- a Literal
;;     -- a Variable
;;     -- an Operation
;;     -- a Call
;;     -- a Block
;;
;; OBSERVER TEMPLATE:
;; arithmetic-expression-fn : ArithmeticExpression -> ??
;;(define (arithmetic-expression-fn exp)
;;  (cond ((literal? exp) ...)
;;        ((variable? exp) ...)
;;        ((operation? exp) ...)
;;        ((call? exp) ...)
;;        ((block? exp) ...)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A StringList is represented as a list of strings

;; CONSTRUCTOR TEMPLATE:
;;  empty
;;  (cons String StringList)

;; OBSERVER TEMPLATE:
;; string-list-fn : StringList -> ??
;;(define (string-list-fn sl)
;;  (cond
;;    [(empty? sl) ...]
;;    [else (...
;;           (first sl)
;;           (string-list-fn (rest sl)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A LiteralList is represented as a list of Literals

;; CONSTRUCTOR TEMPLATE:
;;  empty
;;  (cons String LiteralList)

;; OBSERVER TEMPLATE:
;; literal-list-fn : LiteralList -> ??
;;(define (literal-list-fn ll)
;;  (cond
;;    [(empty? ll) ...]
;;    [else (...
;;           (literal-fn (first ll))
;;           (literal-list-fn (rest ll)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lit : Real -> Literal
;; GIVEN: a real number
;; RETURNS: a literal that represents that number
;; EXAMPLE:
;; (lit 17.4) => (make-literal 17.4)
;; DESIGN STRATEGY : use the constructor template for Literal.

(define (lit num)
  (make-literal num))

(begin-for-test
  (check-equal? (lit 17.4)
                (make-literal 17.4)
                "should return a literal of value 17.4"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; var : String -> Variable
;; GIVEN: a string
;; WHERE: the string begins with a letter and contains
;;     nothing but letters and digits
;; RETURNS: a variable whose name is the given string
;; EXAMPLE:
;; (var "x15") => (make-variable "x15")
;; DESIGN STRATEGY : use the constructor template for Variable.
(define (var str)
  (make-variable str))

(begin-for-test
  (check-equal? (var "x15")
                (make-variable "x15")
                "should return a variable with name x15"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; op : OperationName -> Operation
;; GIVEN: the name of an operation
;; RETURNS: the operation with that name
;; EXAMPLES:
;;(op "+") => (make-operation "+")
;; DESIGN STRATEGY: use the constructor template for Operation.
(define (op name)
  (make-operation name))


(begin-for-test
  (check-equal? (op "+")
                (make-operation "+")
                "should return an operation with name +"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call : ArithmeticExpression ArithmeticExpressionList -> Call
;; GIVEN: an operator expression and a list of operand expressions
;; RETURNS: a call expression whose operator and operands are as
;;     given
;; EXAMPLES:
;; (call (op "-") (list (lit 7) (lit 2.5))) =>
;; (make-call-data  (op "-") (list (lit 7) (lit 2.5)))
;; DESIGN STRATEGY : use the constructor template for Call.
(define (call exp exlst)
  (make-call-data exp exlst))

(begin-for-test
  (check-equal? (call (op "-") (list (lit 7) (lit 2.5)))
                (make-call-data  (op "-") (list (lit 7) (lit 2.5)))
                "should return call data. with operation - and a
literal list (7 2.5)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call-operator : Call -> ArithmeticExpression
;; GIVEN: a call
;; RETURNS: the operator expression of that call
;; EXAMPLE:
;;     (call-operator (call (op "-")
;;                          (list (lit 7) (lit 2.5))))
;;         => (op "-")
;; DESIGN STRATEGY : use the observer template for Call.
(define (call-operator c)
  (call-data-operator c))

(begin-for-test
  (check-equal? (call-operator (call (op "-")(list (lit 7) (lit 2.5))))
                (op "-")
                "must return the operator of call."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call-operands : Call -> ArithmeticExpressionList
;; GIVEN: a call
;; RETURNS: the operand expressions of that call
;; EXAMPLE:
;;     (call-operands (call (op "-")
;;                          (list (lit 7) (lit 2.5))))
;;         => (list (lit 7) (lit 2.5))
;; DESIGN STRATEGY : use the observer template for Call.
(define (call-operands c)
  (call-data-operands c))

(begin-for-test
  (check-equal? (call-operands (call (op "-")  (list (lit 7) (lit 2.5))))
                (list (lit 7) (lit 2.5))
                "must return the operands of call."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; block : Variable ArithmeticExpression ArithmeticExpression
;;             -> ArithmeticExpression
;; GIVEN: a variable, an expression e0, and an expression e1
;; RETURNS: a block that defines the variable's value as the
;;     value of e0; the block's value will be the value of e1
;; EXAMPLES:
;;(block (var "x5")
;;       (lit 5)
;;       (call (op "*") (list (var "x6") (var "x7")))) =>
;;                                                     
;;(make-block-data (var "x5")
;;                 (lit 5)
;;                 (call (op "*") (list (var "x6") (var "x7"))))                                                     
;; DESIGN STRATEGY : use the constructor template for Block.
(define (block var exp1 exp2)
  (make-block-data var exp1 exp2))

(begin-for-test
  (check-equal? (block (var "x5")
       (lit 5)
       (call (op "*") (list (var "x6") (var "x7"))))
                                                     
(make-block-data (var "x5")
                 (lit 5)
                 (call (op "*") (list (var "x6") (var "x7"))))
"must return a block with var: x5 rhs: lit 5 and body expression :
call"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block-var : Block -> Variable
;; GIVEN: a block
;; RETURNS: the variable defined by that block
;; EXAMPLE:
;;     (block-var (block (var "x5")
;;                       (lit 5)
;;                       (call (op "*")
;;                             (list (var "x6") (var "x7")))))
;;         => (var "x5")
;; DESIGN STRATEGY: use the observer template for Block.
(define (block-var b)
  (block-data-var b))

(begin-for-test
  (check-equal? (block-var (block (var "x5")
                                  (lit 5)
                                  (call (op "*")
                                        (list (var "x6") (var "x7")))))
                (var "x5"))
  "must return the variable of block.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block-rhs : Block -> ArithmeticExpression
;; GIVEN: a block
;; RETURNS: the expression whose value will become the value of
;;     the variable defined by that block
;; EXAMPLE:
;;     (block-rhs (block (var "x5")
;;                       (lit 5)
;;                       (call (op "*")
;;                             (list (var "x6") (var "x7")))))
;;         => (lit 5)
;; DESIGN STRATEGY : use the observer template for block.

(define (block-rhs b)
  (block-data-rhs b))


(begin-for-test
  (check-equal? (block-rhs (block (var "x5")
                                  (lit 5)
                                  (call (op "*")
                                        (list (var "x6") (var "x7")))))
                (lit 5)
                "must return the rhs of block."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block-body : Block -> ArithmeticExpression
;; GIVEN: a block
;; RETURNS: the expression whose value will become the value of
;;     the block expression
;; EXAMPLE:
;;     (block-body (block (var "x5")
;;                        (lit 5)
;;                        (call (op "*")
;;                              (list (var "x6") (var "x7")))))
;;         => (call (op "*") (list (var "x6") (var "x7")))
;; DESIGN STRATEGY : use the observer template for block.

(define (block-body b)
  (block-data-body b))

(begin-for-test
  (check-equal?  (block-body (block (var "x5")
                                    (lit 5)
                                    (call (op "*")
                                          (list (var "x6") (var "x7")))))
                 (call (op "*") (list (var "x6") (var "x7")))
                 "must return the body of block."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block? : ArithmeticExpression -> Boolean
;; GIVEN: an arithmetic expression
;; RETURNS: true if and only the expression is a block
;; EXAMPLES:
;;     (block? (block (var "y") (lit 3) (var "z"))))
;;         => true
;;     (block? (var "y"))
;;         => false
;; DESIGN STRATEGY : check if input is of type Block.

(define (block? exp)
  (block-data? exp))

(begin-for-test
  (check-equal? (block? (block (var "y") (lit 3) (var "z"))) true
                "should return true")
  (check-equal? (block? (var "y")) false
                "should return false."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call? : ArithmeticExpression -> Boolean
;; GIVEN :  an arithmetic expression
;; RETURNS:  true if and only the expression is a call
;; EXAMPLE :
;;(call? (call (op "-")
;;             (list (lit 7) (lit 2.5))))
;;=> true
;;
;;(call? (var "b"))
;;=> false
;; DESIGN STRATEGY : check if input is of type Call.

(define (call? exp)
  (call-data? exp))

(begin-for-test
  (check-equal? (call? (call (op "-")
                             (list (lit 7) (lit 2.5))))
                true
                "should return true.")
  
  (check-equal? (call? (var "b"))
                false
                "should return false."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block-variable-name : Block -> StringList
;; GIVEN : a block
;; RETURNS : a list of names of variables in that block.
;; EXAMPLE:
;;(block-variable-name (block (var "x")
;;                            (lit 5)
;;                            (var "c")))
;;=> (list "x")
;; DESIGN STRATEGY : use the observer template for Block.
(define (block-variable-name b)
  (list (variable-name (block-var b))))

(begin-for-test
  (check-equal? (block-variable-name (block (var "x")
                                            (lit 5)
                                            (var "c")))
                (list "x")
                "should return a list of strings (\"x\")"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; not-in-list? : String StringList -> Boolean
;; GIVEN : a String  and a StringList
;; RETURNS : false if the String is in StringList
;; EXAMPLE:
;; (not-in-list? "c" list("a" "b" "c")) => false
;; (not-in-list? "x" (list "a" "b" "c")) => true
;; DESIGN STRATEGY : use the observer template for StringList
(define (not-in-list? ele l)
  (cond
    [(empty? l) true]
    [(string=? (first l) ele) false]
    [else (not-in-list? ele (rest l))]))

(begin-for-test
  (check-equal? (not-in-list? "c" (list "a" "b" "c")) false
                "should return false.")
  (check-equal? (not-in-list? "x" (list "a" "b" "c")) true
                "should return true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dedup-list : StringList StringList -> StringList
;; GIVEN : a list of Strings l and a list of strings seen which is always empty.
;; RETURNS : a list of Strings like l but with no repeating elements.
;; EXAMPLE:
;; (dedup-list (list "a" "b" "c" "a") empty) => (list "a" "b" "c")
;; DESIGN STRATEGY : use observer template for StringList.

(define (dedup-list l seen)
   (cond
     [(empty? l) empty]
     [(not-in-list? (first l) seen)
      (cons (first l) (dedup-list (rest l) (cons (first l) seen)))]
     [else (dedup-list (rest l) seen)]))

(begin-for-test
  (check-equal? (dedup-list (list "a" "b" "c" "a") empty)
                (list "a" "b" "c")
                "the output should not contain repeating items."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables-defined-by : ArithmeticExpression -> StringList
;; GIVEN: an arithmetic expression
;; RETURNS: a list of the names of all variables defined by
;;     all blocks that occur within the expression, without
;;     repetitions, in any order

;; variables-defined-by-list : ArithmeticExpressionList -> StringList
;; GIVEN : an arithmetic expression list , explst.
;; RETURNS : the list of all names of variables that appear in explst.

;; EXAMPLE:
;;     (variables-defined-by
;;      (block (var "x")
;;             (var "y")
;;             (call (block (var "z")
;;                          (var "x")
;;                          (op "+"))
;;                   (list (block (var "x")
;;                                (lit 5)
;;                                (var "x"))
;;                         (var "x")))))
;;  => (list "x" "z") or (list "z" "x")

;; DESIGN STRATEGY : use observer template for
;;                   ArithmeticExpression/ArithmeticExpressionList.

(define (variables-defined-by-list explst)
  (cond
    [(empty? explst) empty]
    [else (append
           (variables-defined-by (first explst))
           (variables-defined-by-list (rest explst)))]))

(define (variables-defined-by exp)
  (cond
    [(block? exp) (dedup-list
                   (append (block-variable-name exp)
                           (variables-defined-by (block-body exp))
                           (variables-defined-by (block-rhs exp)))
                   empty)]
    
    [(call? exp) (dedup-list
                  (append (variables-defined-by (call-operator exp))
                          (variables-defined-by-list (call-operands exp)))
                  empty)]
    
    [else empty]))


(begin-for-test
  (check-equal?
        (variables-defined-by
         (block (var "x")
                (var "y")
                (call (block (var "z")
                             (var "x")
                             (op "+"))
                      (list (block (var "x")
                                   (lit 5)
                                   (var "x"))
                            (var "x")))))
        (list "x" "z")
        "should return (list \"x\" \"z\")"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables-used-by : ArithmeticExpression -> StringList
;; GIVEN: an arithmetic expression
;; RETURNS: a list of the names of all variables used in
;;     the expression, including variables used in a block
;;     on the right hand side of its definition or in its body,
;;     but not including variables defined by a block unless
;;     they are also used

;; variables-used-by-list : ArithmeticExpressionList -> StringList
;; GIVEN : an arithmetic expression list.
;; RETURNS : the list of all variables used by the ArithmeticExpressions in the
;;           list.


;; EXAMPLE:
;;     (variables-used-by
;;      (block (var "x")
;;             (var "y")
;;             (call (block (var "z")
;;                          (var "x")
;;                          (op "+"))
;;                   (list (block (var "x")
;;                                (lit 5)
;;                                (var "x"))
;;                         (var "x")))))
;;  => (list "x" "y") or (list "y" "x")
;; DESIGN STRATEGY : use the observer template for
;;                   ArithmeticExpression/ArithmeticExpressionList

(define (variables-used-by-list explst)
  (cond
    [(empty? explst) empty]
    [else (append (variables-used-by (first explst))
                  (variables-used-by-list (rest explst)))]))



(define (variables-used-by exp)
  (cond
    [(variable? exp) (list (variable-name exp))]
    
    [(block? exp) (dedup-list
                   (append (variables-used-by (block-body exp))
                           (variables-used-by (block-rhs exp)))
                   empty)]
    
    [(call? exp) (dedup-list
                  (append (variables-used-by (call-operator exp))
                          (variables-used-by-list (call-operands exp)))
                  empty)]
    
    [else empty]))


(begin-for-test
  (check-equal? (variables-used-by
      (block (var "x")
             (var "y")
             (call (block (var "z")
                          (var "x")
                          (op "+"))
                   (list (block (var "x")
                                (lit 5)
                                (var "x"))
                         (var "x")))))
                (list "x" "y")
                "should return (list \"x\" \"z\")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operation-expression? : ArithmeticExpression -> Boolean
;; GIVEN : An arithmetic expression.
;; RETURNS : true if the given arithmetic expression is an operation-expression
;; EXAMPLE:
;; (operation-expression? (op "+")) => true
;; (operation-expresison? (var "x")) => false
;; (operation-expression? (block (var "x")
;;                                (lit 5)
;;                                (op "+"))) => true
;;(operation-expression? (block (var "x")
;;                                (lit 5)
;;                                (var "x"))) => false
;; DESIGN STRATEGY : use the observer template for ArithmeticExpression on exp.
(define (operation-expression? exp)
  (cond
    [(operation? exp) true]
    [(block? exp) (operation-expression? (block-body exp))]
    [else false]))



(begin-for-test
  (check-equal? (operation-expression? (op "+")) true
                "should return true.")
  (check-equal? (operation-expression? (var "x")) false
                "should return false.")
  (check-equal? (operation-expression? (block (var "x")
                                              (lit 5)
                                              (op "+")))
                                       true
                                       "should return true.")
  (check-equal? (operation-expression? (block (var "x")
                                              (lit 5)
                                              (var "x")))
                                       false
                                       "should return false."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-call : Call -> Boolean.
;; GIVEN : a Call Expression.
;; RETURNS : true if the Call expression is a constant expression.

;; check-call-operands? : ArtithmeticExpressionList -> Boolean.
;; GIVEN : an artithmetic expression list.
;; RETURNS : true if each arithmetic expression is an operation expression.


;; EXAMPLES:
;; (check-call (call (op "+") (list (var "a") (var "b")))) => false.
;; (check-call (call (op "+") (list (lit 5) (lit 6)))) => true
;; DESIGN STRATEGY : use the observer template for
;;                   Call/ArithmeticExpressionList.

(define (check-call-operands explst)
  (cond
    [(empty? explst) true]
    [else (and (constant-expression? (first explst))
               (check-call-operands (rest explst)))]))



(define (check-call c)
  (and (operation-expression? (call-operator c))
       (check-call-operands (call-operands c))))

(begin-for-test
  (check-equal? (check-call (call (op "+") (list (var "a") (var "b")))) false
                "should return false")
  (check-equal? (check-call (call (op "+") (list (lit 5) (lit 6)))) true
                "should return true"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constant-expression? : ArithmeticExpression -> Boolean
;; GIVEN: an arithmetic expression
;; RETURNS: true if and only if the expression is a constant
;;     expression
;; EXAMPLES:
;;     (constant-expression?
;;      (call (var "f") (list (lit -3) (lit 44))))
;;         => false
;;     (constant-expression?
;;      (call (op "+") (list (var "x") (lit 44))))
;;         => false
;;     (constant-expression?
;;      (block (var "x")
;;             (var "y")
;;             (call (block (var "z")
;;                          (call (op "*")
;;                                (list (var "x") (var "y")))
;;                          (op "+"))
;;                   (list (lit 3)
;;                         (call (op "*")
;;                               (list (lit 4) (lit 5)))))))
;;         => true
;; DESIGN STRATEGY : combine simpler functions.

(define (constant-expression? exp)
  (cond
    [(literal? exp) true]
    [(call? exp) (check-call exp)]
    [(block? exp) (constant-expression? (block-body exp))]
    [else false]))



(begin-for-test
  (check-equal? (constant-expression?
                (call (var "f") (list (lit -3) (lit 44))))
                false
                "should return false.")
  
  (check-equal? (constant-expression?
                 (call (op "+") (list (var "x") (lit 44))))
                false
                "should return false.")
  (check-equal? (constant-expression?
                 (block (var "x")
                        (var "y")
                        (call (block (var "z")
                                     (call (op "*")
                                           (list (var "x") (var "y")))
                                     (op "+"))
                              (list (lit 3)
                                    (call (op "*")
                                          (list (lit 4) (lit 5)))))))
                true
                "should return true."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplify-add : LiteralList -> Real
;; GIVEN   : a List of Literals
;; RETURNS :  the sum of all the literals in the list.
;; EXAMPLE :
;; (simplify-add (list (lit 5) (lit 5) (lit 5))) => 15
;; DESIGN STRATEGY : combining simpler functions.
(define (simplify-add lst)
  (cond
    [(empty? lst) 0]
    [else (+ (literal-value (first lst))
             (simplify-add (rest lst)))]))



(begin-for-test
  (check-equal? (simplify-add (list (lit 5) (lit 5) (lit 5))) 15
                "should return 15"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplify-multiply : LiteralList -> Real
;; GIVEN   : a List of Literals
;; RETURNS :  the product of all the literals in the list.
;; EXAMPLE :
;; (simplify-multiply (list (lit 5) (lit 5) (lit 5))) => 125
;; DESIGN STRATEGY : combining simpler functions.
(define (simplify-multiply lst)
  (cond
    [(empty? lst) 1]
    [else (* (literal-value (first lst))
             (simplify-multiply (rest lst)))]))



(begin-for-test
  (check-equal? (simplify-multiply (list (lit 5) (lit 5) (lit 5))) 125
                "should return 125"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplify-divide : LiteralList -> Real
;; GIVEN   : a List of Literals
;; RETURNS : if only one element returns quotient of 1 divided by the element.
;;           the quotient of dividing the first literal by the product of
;;           the rest of the literals in the list.
;; EXAMPLE :
;; (simplify-divide (list (lit 40) (lit 2) (lit 2))) => 10
;; (simplify-divide (list (lit 3)) => 1/3
;; DESIGN STRATEGY : combining simpler functions.

(define (simplify-divide lst)
  (cond
    [(empty? (rest lst)) (/ 1 (literal-value (first lst)))]
    [else (/ (literal-value (first lst))
             (simplify-multiply (rest lst)))]))



(begin-for-test
  (check-equal? (simplify-divide (list (lit 40) (lit 2) (lit 2))) 10
                "should return 10")
  (check-equal? (simplify-divide (list (lit 3))) 1/3
                "should return 1/3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplify-subtract : LiteralList -> Real
;; GIVEN   : a List of Literals
;; RETURNS : if only one element in the list returns a negative value whose
;;           absolute numerical value is the same as the given literal.
;;           else retuns the value of difference between the first literal with
;;           the sum of the rest of the literals
;; EXAMPLE :
;; (simplify-divide (list (lit 10) (lit 2) (lit 3))) => 5
;; (simplify-divide (list (lit 3)) => -3
;; DESIGN STRATEGY : combining simpler functions.

(define (simplify-subtract lst)
  (cond
    [(empty? (rest lst)) (- (literal-value (first lst)))]
    [else (- (literal-value (first lst))
             (simplify-add (rest lst)))]))


(begin-for-test
  (check-equal? (simplify-subtract (list (lit 10) (lit 2) (lit 3))) 5
                "should return 5")
  (check-equal? (simplify-subtract (list (lit 3))) -3
                "should return -3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplify-call : Call -> Literal
;; GIVEN : a call
;; RETURNS : the simplified value of that call.
;; EXAMPLE :
;; (simplify-call (call (op "+") (list (lit 5) (lit 4))) => (lit 9)
;; (simplify-call (call (op "-") (list (lit 5) (lit 4))) => (lit 1)
;; (simplify-call (call (op "*") (list (lit 5) (lit 4))) => (lit 20)
;; (simplify-call (call (op "/") (list (lit 10) (lit 2))) => (lit 5)
;; DESIGN STRATEGY : combining simpler functions.

(define (simplify-call c)
  (cond
    [(string=? (operation-name (call-operator c)) "+")
     (lit (simplify-add (call-operands c)))]
    
    [(string=? (operation-name (call-operator c)) "-")
     (lit (simplify-subtract (call-operands c)))]
    
    [(string=? (operation-name (call-operator c)) "*")
     (lit (simplify-multiply (call-operands c)))]
    
    [(string=? (operation-name (call-operator c)) "/")
     (lit (simplify-divide (call-operands c)))]))



(begin-for-test
  (check-equal?
   (simplify-call (call (op "+") (list (lit 5) (lit 4)))) (lit 9)
                  "should return (lit 9)")
  (check-equal?
   (simplify-call (call (op "-") (list (lit 5) (lit 4)))) (lit 1)
                  "should return (lit 1)")
  (check-equal?
   (simplify-call (call (op "*") (list (lit 5) (lit 4)))) (lit 20)
                  "should return (lit 20)")
  (check-equal?
   (simplify-call (call (op "/") (list (lit 10) (lit 2)))) (lit 5)
                  "should return (lit 5)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval-call : ArithmeticExpression -> Literal
;; GIVEN : an ArithmeticExpression
;; WHERE  : the expression is a constant expression. 
;; RETURNS : the value of the Arithmetic expression as a literal.

;; eval-call : ArithmeticExpressionList -> LiteralList
;; GIVEN : a list of ArithmeticExpressions
;; WHERE  : each  expression in the list is a constant expression. 
;; RETURNS : the value of the Arithmetic expressions as a list of Literals.

;; EXAMPLES:
;; (eval-call (call (op "+") (list (lit 4) (lit 5)))) => (lit 9)
;; DESIGN STRATEGY : use the observer template for
;;                   ArithmeticExpression/ArithmeticExpressionList

(define (eval-call-list explst)
  (cond
    [(empty? explst) empty]
    [else (cons (eval-call (first explst))
                (eval-call-list (rest explst)))]))

(define (eval-call exp)
  (cond
    [(call? exp)
     (simplify-call (call (call-operator exp)
                          (eval-call-list (call-operands exp))))]
    [else exp]))

(begin-for-test
  (check-equal? (eval-call (call (op "+") (list (lit 4) (lit 5)))) (lit 9)
                "should return a literal with value 9"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval-block : ArithmeticExpression -> ArithmeticExpression
;; GIVEN : an ArithmeticExpression
;; WHERE : the expression is a constant expression.
;; RETURNS : an ArithmeticExpression where all the blocks have been simplified
;;           by keeping only the block bodies.


;; eval-block-list : ArithmeticExpressionList -> ArithmeticExpressionList
;; GIVEN : an ArithmeticExpressionList 
;; WHERE : each  expression is a constant expression.
;; RETURNS : an ArithmeticExpressionList where all the blocks have been
;;           simplified by keeping only the block bodies.

;; EXAMPLE :
;;(eval-block  (block (var "x")
;;            (var "y")
;;             (call (block (var "z")
;;                          (call (op "*")
;;                                (list (var "x") (var "y")))
;;                          (op "+"))
;;                   (list (lit 3)
;;                         (call (op "*")
;;                               (list (lit 4) (lit 5)))))))
;;
;;=> (call (op "+") (list (lit 3) (call (op "*") (list (lit 4) (lit 5)))))
;; DESIGN STRATEGY : use the observer template for
;;                   ArithmeticExpression/ArithmeticExpressionList

(define (eval-block-list explst)
  (cond
    [(empty? explst) empty]
    [else (cons (eval-block (first explst))
                (eval-block-list (rest explst)))]))

(define (eval-block exp)
  (cond
    [(block? exp) (eval-block (block-body exp))]
    [(call? exp) (call (eval-block (call-operator exp))
                       (eval-block-list (call-operands exp)))]
    [else exp]))


(begin-for-test
  (eval-block  (block (var "x")
            (var "y")
             (call (block (var "z")
                          (call (op "*")
                                (list (var "x") (var "y")))
                          (op "+"))
                   (list (lit 3)
                         (call (op "*")
                               (list (lit 4) (lit 5)))))))
  (call (op "+") (list (lit 3) (call (op "*") (list (lit 4) (lit 5)))))
  "the output should not contain blocks")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constant-expression-value : ArithmeticExpression -> Real
;; GIVEN: an arithmetic expression
;; WHERE: the expression is a constant expression
;; RETURNS: the numerical value of the expression
;; EXAMPLES:
;;     (constant-expression-value
;;      (call (op "/") (list (lit 15) (lit 3))))
;;         => 5
;;     (constant-expression-value
;;      (block (var "x")
;;             (var "y")
;;             (call (block (var "z")
;;                          (call (op "*")
;;                                (list (var "x") (var "y")))
;;                          (op "+"))
;;                   (list (lit 3)
;;                         (call (op "*")
;;                               (list (lit 4) (lit 5)))))))
;;         => 23
;; DESIGN STRATEGY : combining simpler functions. 

(define (constant-expression-value exp)
  (literal-value (eval-call (eval-block  exp))))


(begin-for-test
  (check-equal?
   (constant-expression-value
      (call (op "/") (list (lit 15) (lit 3))))
   5
   "should return 5")
  (check-equal? (constant-expression-value
      (block (var "x")
             (var "y")
             (call (block (var "z")
                          (call (op "*")
                                (list (var "x") (var "y")))
                          (op "+"))
                   (list (lit 3)
                         (call (op "*")
                               (list (lit 4) (lit 5)))))))
                23
                "should return 23."))


