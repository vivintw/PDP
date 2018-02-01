;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
         undefined-variables)


(check-location "07" "q1.rkt")
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Call is represented as a struct:
;;(make-call-data operator operands)

;;INTERP:
;; operator : ArithmeticExpression       represents the operation that has to
;;                                       be applied to the operands of the call.
;; operands : ArithmeticExpressionList   represents the operands in the call on
;;                                       which the operation is applied.

;;IMPLEMENTATION:
(define-struct call-data (operator operands))

;;CONSTRUCTOR TEMPLATE:
;;for Call:
;;(make-call ArithmeticExpression ArithmeticExpressionList)
;;
;;for ArithmeticExpressionList:
;; -- empty
;; -- (cons ArithmeticExpression ArithmeticExpressionList)

;;OBSERVER TEMPLATE:
;;for call :
;; call-fn : Call -> ??
;;(define (call-fn c)
;;  (...
;;   (arithmetic-expression-fn (call-data-operator c))
;;   (ael-fn (call-data-operands c))))

;;for ArithmeticExpressionList :
;; ael-fn : ArithmeticExpressionList -> ??
;;(define (ael-fn lst)
;;  (cond
;;    [(empty? lst) ...]
;;    [else (... (arithmetic-expression-fn (first lst))
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
;;   (arithmetic-expression-fn (block-data-rhs b))
;;   (arithmetic-expression-fn (block-data-body b))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call : ArithmeticExpression ArithmeticExpressionList -> Call
;; GIVEN: an operator expression and a list of operand expressions
;; RETURNS: a call expression whose operator and operands are as
;;          given.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block? : ArithmeticExpression -> Boolean
;; GIVEN: an arithmetic expression
;; RETURNS: true if and only the expression is a block
;; EXAMPLES:
;;(block? (block (var "y") (lit 3) (var "z"))) => true
;;(block? (var "y")) => false
;; DESIGN STRATEGY : check if input is of type Block.

(define (block? exp)
  (block-data? exp))

(begin-for-test
  (check-equal? (block? (block (var "y") (lit 3) (var "z"))) true
                "should return true")
  (check-equal? (block? (var "y")) false
                "should return false."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dedup-list : StringList StringList -> StringList
;; GIVEN : a list of Strings l, and a list of strings seen.
;; WHERE : the value passed to the function for seen initially is always empty
;; RETURNS : a list of Strings like l but with no repeating elements.
;; EXAMPLE:
;; (dedup-list (list "a" "b" "c" "a") empty) => (list "a" "b" "c")
;; DESIGN STRATEGY : use HOF ormap to determine if the given string is in seen.

(define (dedup-list l seen)
   (cond
     [(empty? l) empty]
     [(not (ormap
            ;; String -> Boolean
            ;; GIVEN : a String (any string will do).
            ;; RETURNS : true if the string equals first element of l.
            (lambda (x) (string=? (first l) x)) seen))
      (cons (first l) (dedup-list (rest l) (cons (first l) seen)))]
     [else (dedup-list (rest l) seen)]))

(begin-for-test
  (check-equal? (dedup-list (list "a" "b" "c" "a") empty)
                (list "a" "b" "c")
                "the output should not contain repeating items."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; not-in-defined-vars? : Variable ArithmeticExpressionList -> Boolean
;; GIVEN : a Variable var, and an ArithmeticExpresisonList explist
;; WHERE : each element in explst is a Variable.
;; RETURNS : true if var doesnot exist in explst.
;; EXAMPLES: 
;; (not-in-defined-vars? (var "x") (list (var "z"))) => true
;; (not-in-defined-vars? (var "x") (list (var "x"))) => false
;; (not-in-defined-vars? (var "x") empty) => true
;; DESIGN STRATEGY : use HOF ormap.

(define (not-in-defined-vars? var defined-vars)
  (not (ormap
        ;; Variable -> Boolean.
        ;; GIVEN : Variable x.
        ;; RETURNS : true if x is the same as var.
        (lambda (x) (equal? x var))
         defined-vars)))


(begin-for-test
  (check-equal? (not-in-defined-vars? (var "x") (list (var "z"))) true)
  (check-equal? (not-in-defined-vars? (var "x") (list (var "x"))) false)
  (check-equal? (not-in-defined-vars? (var "x") empty) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables-not-defined : ArithmeticExpression ArithmeticExpressionList ->
;; StringList
;; GIVEN :  an arbitrary arithmetic expression exp, and
;;          an ArithmeticExpressionList defined-vars
;; WHERE : the ArithmeticExpressionList defined-vars contains only Variables.
;; AND : defined-vars contains all variables defined until now.
;; RETURNS : a list of the names of all undefined variables
;;           for the expression, in any order

;; variables-list-not-defined :
;; ArithmeticExpressionList ArithmeticExpressionList -> StringList
;; GIVEN : a list of arbitrary arithmetic expressions explst, and an
;; ArithmeticExpressionList defined-vars
;; WHERE : the ArithmeticExpressionList defined-vars,  contains only Variables.
;; AND : defined-vars contains all variables defined until now.
;; RETURNS : a list of the names of all undefined variables
;;     for the expression, in any order

;; EXAMPLES:
;;     (variables-not-defined
;;      (call (var "f")
;;            (list (block (var "x")
;;                         (var "x")
;;                         (var "x"))
;;                  (block (var "y")
;;                         (lit 7)
;;                         (var "y"))
;;                  (var "z"))) empty)
;;  => some permutation of (list "f" "x" "z")
;;    (variables-not-defined (op "+") empty)
;;  => empty

;; DESIGN STRATEGY : use observer template for
;;                   ArithmeticExpression/ArithmeticExpressionList.


(define (variables-list-not-defined explst defined-vars)
  (cond
    [(empty? explst) empty]
    [else (append (variables-not-defined (first explst) defined-vars)
                  (variables-list-not-defined (rest explst) defined-vars))]))


(define (variables-not-defined exp defined-vars)
  (cond [(or (literal? exp)
             (operation? exp)) empty]
        
        [(variable? exp) (if (not-in-defined-vars? exp defined-vars)
                             (list (variable-name exp))
                             empty)]
        
        [(call? exp) (append
                      (variables-not-defined
                       (call-operator exp)
                       defined-vars)
                      (variables-list-not-defined
                       (call-operands exp)
                       defined-vars))]
        
        [(block? exp) (append
                       (variables-not-defined (block-rhs exp) defined-vars)
                       (variables-not-defined
                        (block-body exp)
                        (cons (block-var exp) defined-vars)))]))



(begin-for-test
  (check-equal? (variables-not-defined
                 (call (var "f")
                       (list (block (var "x")
                                    (var "x")
                                    (var "x"))
                             (block (var "y")
                                    (lit 7)
                                    (var "y"))
                             (var "z"))) empty)
                (list "f" "x" "z")
                "must return (list \"f\" \"x\" \"z\")")
  
  (check-equal? (variables-not-defined (op "+") empty) empty)
  "must return '()")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undefined-variables : ArithmeticExpression -> StringList
;; GIVEN: an arbitrary arithmetic expression
;; RETURNS: a list of the names of all undefined variables
;;     for the expression, without repetitions, in any order
;; EXAMPLE:
;;     (undefined-variables
;;      (call (var "f")
;;            (list (block (var "x")
;;                         (var "x")
;;                         (var "x"))
;;                  (block (var "y")
;;                         (lit 7)
;;                         (var "y"))
;;                  (var "z"))))
;;  => some permutation of (list "f" "x" "z")
;; DESIGN STRATEGY : call a simpler function.

(define (undefined-variables exp)
  (dedup-list (variables-not-defined exp empty) empty))



(begin-for-test
  (check-equal? (undefined-variables
                 (call (var "f")
                       (list (block (var "x")
                                    (var "x")
                                    (var "x"))
                             (block (var "y")
                                    (lit 7)
                                    (var "y"))
                             (var "z"))))
                (list "f" "x" "z")
                "must return (list \"f\" \"x\" \"z\")"))
