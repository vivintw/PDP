;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)
(require "q1.rkt")

(check-location "07" "q2.rkt")

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
         undefined-variables
         well-typed?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define OP0-LIST (list "+" "*"))
(define OP1-LIST (list "-" "/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; op-in-list : Operation ArithmeticExpressionList -> Boolean
;; GIVEN : an Operation p and an ArithmeticExpressionList lst
;; WHERE : all the elements in lst are Operations
;; RETURNS : true, if p exists in lst else false
;; EXAMPLES:
;; (op-in-list (op "+") OP0-LIST) => true
;; (op-in-list (op "-") OP0-LIST) => false
;; DESIGN STRATEGY : use HOF ormap.

(define (op-in-list p lst)
  (ormap
   ;; Operation -> Boolean
   ;; GIVEN : an Operation x
   ;; RETURNS : true, if p is the same as x 
   (lambda (x) (string=? (operation-name p) x)) lst))


(begin-for-test
  (check-equal? (op-in-list (op "+") OP0-LIST) true
                "should return true, as + in OP0-LIST")
  
  (check-equal? (op-in-list (op "-") OP0-LIST) false
                "should return false, as - not in OP0-LIST"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; op0? : Operation -> Boolean
;; GIVEN : an Operation p 
;; RETURNS : true, if p is of type op0
;; EXAMPLES:
;; (op0? (op "+")) => true
;; (op0? (op "-")) => false
;; DESIGN STRATEGY : call a more general function.

(define (op0? p)
  (op-in-list p OP0-LIST))



(begin-for-test
  (check-equal? (op0? (op "+")) true
                "should return true, as + is of type op0")
  (check-equal? (op0? (op "-")) false
                "should return false, as - is not of type op0"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; op1? : Operation -> Boolean
;; GIVEN : an Operation p 
;; RETURNS : true, if p is of type op1
;; EXAMPLES:
;; (op1? (op "+")) => false
;; (op1? (op "-")) => true
;; DESIGN STRATEGY : call a more general function.

(define (op1? p)
  (op-in-list p OP1-LIST))



(begin-for-test
  (check-equal? (op1? (op "+")) false
                "should return false, as + is not of type op1")
  (check-equal? (op1? (op "-")) true
                "should return true, as - is of type op1"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; well-typed-call-list-op? Boolean ArithmeticExpressionList -> Boolean
;; GIVEN : Boolean base-val, and ArithmeticExpressionList explst
;; RETURNS : base-val if the explst is empty,  else true, if all elements of
;;           explst are well-typed.
;; EXAMPLES:
;; (well-typed-call-list-op? true empty) => true
;; (well-typed-call-list-op? false empty) => false
;; (well-typed-call-list-op? true (list (var "x"))) => false
;; (well-typed-call-list-op? false (list (var "x"))) => false
;; (well-typed-call-list-op? true (list (lit 5))) => true
;; (well-typed-call-list-op? false (list (lit 5))) => true
;; DESIGN STRATEGY : use HOF andmap.

(define (well-typed-call-list-op? base-val explst)
  (cond
    [(empty? explst) base-val]
    [else (andmap
           ;; ArithmeticExpression -> Boolean
           ;; GIVEN : an ArithmeticExpression x
           ;; RETURNS : true, if x is well-typed.
           (lambda (x) (well-typed-call-exp? x)) explst)]))


(begin-for-test
  (check-equal? (well-typed-call-list-op? true empty) true
                "should return base-val for empty list")
  (check-equal? (well-typed-call-list-op? false empty) false
                 "should return base-val for empty list")
  (check-equal? (well-typed-call-list-op? true (list (var "x"))) false
                "variable out of scope, hence false")
  (check-equal? (well-typed-call-list-op? false (list (var "x"))) false
                "variable out of scope, hence false")
  (check-equal? (well-typed-call-list-op? true (list (lit 5))) true
                "literal is of type Int")
  (check-equal? (well-typed-call-list-op? false (list (lit 5))) true
                "literal is of type Int"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; well-typed-call-list-op0? : ArithmeticExpressionList -> Boolean
;; GIVEN : ArithmeticExpressionList explst
;; WHERE : explst is the Operands of a call whose operator is of type  op0
;; RETURNS : true, if if all Operands in explst are well typed.
;; EXAMPLES:
;; (well-typed-call-list-op0? empty) => true
;; (well-typed-call-list-op0? (list (var "x"))) => false
;; (well-typed-call-list-op0? (list (lit 5))) => true
;; DESIGN STRATEGY : call a more general function.

(define (well-typed-call-list-op0? explst)
 (well-typed-call-list-op? true explst))



(begin-for-test
  (check-equal? (well-typed-call-list-op0? empty) true
               "should return true for empty list")
  (check-equal? (well-typed-call-list-op0? (list (var "x"))) false
               "variable out of scope, hence false")
  (check-equal? (well-typed-call-list-op0? (list (lit 5))) true
               "literal is of type Int"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; well-typed-call-list-op1? : ArithmeticExpressionList -> Boolean
;; GIVEN : ArithmeticExpressionList explst
;; WHERE : explst is the Operands of a call whose operator is of type  op1
;; RETURNS : true, if if all Operands in explst are well typed and contains
;;           atleast one operand.
;; EXAMPLES:
;; (well-typed-call-list-op1? empty) => false
;; (well-typed-call-list-op1? (list (var "x"))) => false
;; (well-typed-call-list-op1? (list (lit 5)))  => true
;; DESIGN STRATEGY : call a more general function.

(define (well-typed-call-list-op1? explst)
  (well-typed-call-list-op? false explst))



(begin-for-test
  (check-equal? (well-typed-call-list-op1? empty) false
                "should return false for empty list")
  (check-equal? (well-typed-call-list-op1? (list (var "x"))) false
                "variable out of scope, hence false")
  (check-equal? (well-typed-call-list-op1? (list (lit 5))) true
                "literal is of type Int"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-inner-value exp)
  (cond
    [(block? exp) (get-inner-value (block-body exp))]
    [else exp]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; well-typed-call? ArithmeticExpression -> Boolean
;; GIVEN : an ArithmenticExpression exp
;; WHERE : exp is of type Call only.
;; AND : all the variables defined in its ancestor blocks have been replaced
;;       with the variable's value.
;; RETURNS : true, if the operator of a call has type Op0, and all of its
;;           operands have type Int or If the operator of a call has type Op1,
;;           and all of its operands have type Int, and there is at least one
;;           operand
;; EXAMPLE:
;; (well-typed-call? (call (op "+") (list (lit 5) (lit 6)))) => true
;; (well-typed-call? (call (op "+") (list (lit 5) (var "x")))) => false
;; (well-typed-call? (call (op "+") empty)) => true
;; (well-typed-call? (call (op "-") empty)) => false
;; (well-typed-call? (call (var "x") (list (lit 5) (lit 6)))) => false
;; DESIGN STRATEGY : use simpler functions.

(define (well-typed-call? exp)
  (cond
    [(not (operation? (get-inner-value (call-operator exp)))) false]
    [(op0? (get-inner-value (call-operator exp)))
     (well-typed-call-list-op0? (call-operands exp))]
    [(op1? (get-inner-value (call-operator exp)))
     (well-typed-call-list-op1? (call-operands exp))]))



(begin-for-test
  (check-equal? (well-typed-call? (call (op "+") (list (lit 5) (lit 6)))) true
               "well typed call must return true.")
  (check-equal? (well-typed-call? (call (op "+") (list (lit 5) (var "x"))))
               false "undefined variable in operands of the call")
  (check-equal? (well-typed-call? (call (op "+") empty)) true
               "well-typed-call? must return true")
  (check-equal? (well-typed-call? (call (op "-") empty)) false
               "well-typed-call? must return false")
  (check-equal? (well-typed-call? (call (var "x") (list (lit 5) (lit 6))))
                false "operator variable not defined, hence false."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; well-typed-exp? : Boolean ArithmeticExpression -> Boolean
;; GIVEN : a Boolean op-val and any ArithmeticExpression exp
;; WHERE : op-val is a Boolean value that must be returned if the type of
;;         exp is an Operation.(true if its exp is of Block type,
;;         else false)
;; AND : all the variables defined in its ancestor blocks have been replaced
;;       with the variable's value.
;; RETURNS : op-val if exp is an Operation, else true, if the exp is well typed
;; EXAMPLES:
;; (well-typed-exp? true (op "+")) => true
;; (well-typed-exp? true (block (var "x") (var "y") (var "x"))) => false
;; (well-typed-exp? true (block (var "x") (lit 5) (lit 5))) => true
;; (well-typed-exp? false (call (op "+") (list (lit 5) (lit 6)))) => true
;; (well-typed-exp? false (call (var "x") empty)) => false
;; DESIGN STRATEGY : use the obsever template for ArithmeticExpression.

(define (well-typed-exp? op-val exp)
  (cond
    [(literal? exp) true]
    [(variable? exp) false]
    [(operation? exp) op-val]
    [(block? exp) (if (well-typed-block-exp? (block-rhs exp))
                      (well-typed-block-exp? (block-body exp))
                      false)]
    [(call? exp) (well-typed-call? exp)]))



(begin-for-test
  (check-equal? (well-typed-exp? true (op "+")) true
                "must return the value of op-val")
  (check-equal? (well-typed-exp? true (block (var "x") (var "y") (var "x")))
                false "has undefined variables")
  (check-equal? (well-typed-exp? true (block (var "x") (lit 5) (lit 5)))
                true "well typed block must return true.")
  (check-equal? (well-typed-exp? false (call (op "+") (list (lit 5) (lit 6))))
                true "well typed call must return true")
  (check-equal? (well-typed-exp? false (call (var "x") empty))
                false "undefined variable x in call"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; well-typed-block-exp? : ArithmeticExpression -> Boolean
;; GIVEN : An ArithmeticExpression exp
;; WHERE : exp is of type Block
;; AND : all the variables defined in its ancestor blocks have been replaced
;;       with the variable's value.
;; RETURNS : true, if the block is well typed.
;; EXAMPLES:
;; (well-typed-block-exp? (block (var "x") (var "y") (var "x"))) => false
;; (well-typed-block-exp? (block (var "x") (lit 5) (lit 5))) => true
;; DESIGN STRATEGY : use a more general function.

(define (well-typed-block-exp? exp)
  (well-typed-exp? true exp))



(begin-for-test
  (check-equal? (well-typed-block-exp? (block (var "x") (var "y") (var "x")))
                false "has undefined variables")
  (check-equal? (well-typed-block-exp? (block (var "x") (lit 5) (lit 5)))
                true "well typed block must return true."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; well-typed-call-exp? : ArithmeticExpression -> Boolean
;; GIVEN : An ArithmeticExpression exp
;; WHERE : exp is of type Call
;; AND : all the variables defined in its ancestor blocks have been replaced
;;       with the variable's value.
;; RETURNS : true, if the block is well typed.
;; EXAMPLES:
;; (well-typed-call-exp? (call (op "+") (list (lit 5) (lit 6)))) => true
;; (well-typed-call-exp? (call (var "x") empty)) => false
;; DESIGN STRATEGY : use a more general function.

(define (well-typed-call-exp? exp)
  (well-typed-exp? false exp))



(begin-for-test
  (check-equal? (well-typed-call-exp? (call (op "+") (list (lit 5) (lit 6))))
                true "well typed call must return true")
   (check-equal? (well-typed-call-exp? (call (var "x") empty))
                false "undefined variable x in call"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; var-replace : ArithmeticExpression ArithmeticExpression
;; ArithmeticExpression -> ArithmeticExpression
;; GIVEN :  an ArtihmeticExpression v, ArtihmeticExpression l and an
;; ArtihmeticExpression exp
;; WHERE : v is of type Variable.
;; RETURNS : an ArtihmeticExpression like exp but where all occurances of
;;           v is replaced by l.


;; var-replace-list :ArithmeticExpression ArithmeticExpression
;; ArithmeticExpressionList -> ArithmeticExpressionList
;; GIVEN : an ArtihmeticExpression v, ArtihmeticExpression l and an
;; ArtihmeticExpressionList explst
;; WHERE : v is of type Variable. 
;; RETURNS : an ArtihmeticExpressionList like explst but where all occurances of
;;           v is replaced by l.


;; EXAMPLES:
;;(var-replace-list (var "x") (lit 1) (list (var "x") (var "y")))
;;  => (list (lit 1) (var "y"))
;; (var-replace-list (var "x") (lit 1) empty) => empty


;; DESIGN STRATEGY : use the observer template for
;; ArithmeticExpression/ArithmeticExpressionList

(define (var-replace v l exp)
  (cond
    [(variable? exp) (if (string=? (variable-name exp)
                                   (variable-name v)) l exp)]
    
    [(block? exp) (block (block-var exp)
                         (var-replace v l (block-rhs exp))
                         (var-replace v l (block-body exp)))]
    
    [(call? exp) (call (var-replace v l (call-operator exp))
                       (var-replace-list v l (call-operands exp)))]
    [else exp]))



(define (var-replace-list v l explst)
  (cond
    [(empty? explst) empty]
    [else (cons
           (var-replace v l (first explst))
           (var-replace-list v l (rest explst)))]))



(begin-for-test
  (check-equal?
   (var-replace (var "x") (lit 1) (call (var "z") (list (var "x") (var "y"))))
   (call (var "z") (list (lit 1) (var "y")))
   "must replace var x with lit 1")
  
  (check-equal?
   (var-replace (var "x") (lit 1)  (block (var "x") (lit 1) (var "x")))
   (block (var "x") (lit 1) (lit 1))
   "must replace var x with lit 1"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all-var-replace : ArithmeticExpression -> ArithmeticExpression
;; GIVEN : an ArithmeticExpression exp
;; RETURNS : an ArithmeticExpression like exp but with all variables defined by
;;           blocks in exp by the rhs of the same block within the same block's
;;           body.

;; all-var-replace-list : ArithmeticExpressionList -> ArithmeticExpressionList
;; GIVEN : ArithmeticExpressionList explst
;; RETURNS : an ArithmeticExpressionList like exp but with all variables defined
;;           by elements which are blocks in explst by the rhs of the same block
;;           within the same block's body.


;; EXAMPLES :
;;(all-var-replace
;; (block (var "f")
;;        (op "+")
;;        (block (var "x")
;;               (call (var "f") (list))
;;               (call (op "*")
;;                     (list (var "x"))))))
;;=> (block (var "f")
;;          (op "+")
;;          (block (var "x")
;;                 (call (op "+") (list))
;;                 (call (op "*")
;;                       (list (call (op "+") (list))))))
;; DESIGN STRATEGY : use the observer template for
;;  ArithmeticExpression/ArithmeticExpressionList


(define (all-var-replace-list explst)
  (cond
    [(empty? explst) empty]
    [else (cons (all-var-replace (first explst))
                (all-var-replace-list (rest explst)))]))

(define (all-var-replace exp)
  (cond
    
;    [(block? exp) (block (block-var exp)
;                         (all-var-replace (block-rhs exp))
;                         (var-replace (block-var exp)
;                                      (all-var-replace (block-rhs exp))
;                                      (all-var-replace (block-body exp))))]
    
     [(block? exp) (block (block-var exp)
                          (block-rhs exp)
                         (var-replace (block-var exp)
                                      (block-rhs exp)
                                      (all-var-replace (block-body exp))))]

    [(call? exp) (call (all-var-replace (call-operator exp))
                       (all-var-replace-list (call-operands exp)))]
    [else exp]))

(begin-for-test
  (check-equal? (all-var-replace
                 (block (var "f")
                        (op "+")
                        (block (var "x")
                               (call (var "f") (list))
                               (call (op "*")
                                     (list (var "x"))))))
                
                (block (var "f")
                       (op "+")
                       (block (var "x")
                              (call (op "+") (list))
                              (call (op "*")
                                    (list (call (op "+") (list))))))
                "must return a block with rhs of the block replacing the block
variable in the blocks body."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; well-typed? : ArithmeticExpression -> Boolean
;; GIVEN: an arbitrary arithmetic expression
;; RETURNS: true if and only if the expression is well-typed
;; EXAMPLES:
;;     (well-typed? (lit 17))  =>  true
;;     (well-typed? (var "x"))  =>  false
;;
;;     (well-typed?
;;      (block (var "f")
;;             (op "+")
;;             (block (var "x")
;;                    (call (var "f") (list))
;;                    (call (op "*")
;;                          (list (var "x")))))) => true
;;
;;     (well-typed?
;;      (block (var "f")
;;             (op "+")
;;             (block (var "f")
;;                    (call (var "f") (list))
;;                    (call (op "*")
;;                          (list (var "f")))))) => true
;;
;;     (well-typed?
;;      (block (var "f")
;;             (op "+")
;;             (block (var "x")
;;                    (call (var "f") (list))
;;                    (call (op "*")
;;                          (list (var "f")))))) => false
;; DESIGN STRATEGY : use the observer template for ArithmeticExpression.

(define (well-typed? exp)
  (cond
    [(block? exp) (well-typed-block-exp? (all-var-replace exp))]
    [(call? exp) (well-typed-call-exp? (all-var-replace exp))]
    [(variable? exp) false]
    [else true]))
  

(begin-for-test
  
  (check-equal? (well-typed? (lit 17)) true
                "a literal is of type Int, hence true.")
  
  (check-equal? (well-typed? (var "x")) false
                "an undefined variable has no type, hence false.")
  
  (check-equal?  (well-typed?
                  (block (var "f")
                         (op "+")
                         (block (var "x")
                                (call (var "f") (list))
                                (call (op "*")
                                      (list (var "x"))))))
                 true
                 "well typed block must return true")
  
  (check-equal? (well-typed?
                 (block (var "f")
                        (op "+")
                        (block (var "f")
                               (call (var "f") (list))
                               (call (op "*")
                                     (list (var "f"))))))
                true
                "well typed block must return true")
  
  (check-equal? (well-typed?
                 (block (var "f")
                        (op "+")
                        (block (var "x")
                               (call (var "f") (list))
                               (call (op "*")
                                     (list (var "f"))))))
                false
                "block is not well typed, hence must return false.")
  
  (check-equal? (well-typed?
                 (call (op "+") (list (block (var "x")
                                             (lit 1)
                                             (call (op "*")
                                                   (list (var "x") (lit 2))))
                                      (lit 5))))
                true
                "well typed call expression must return true")
  
  (check-equal? (well-typed?
                 (call (op "+") (list (block (var "x")
                                             (lit 1)
                                             (call (op "*")
                                                   (list (var "x") (lit 2))))
                                      (var "x"))))
                false
                "call is not well typed , hence false."))



;(block (var "r")
;       (block (var "a")
;              (op "+")
;              (block (var "b")
;                     (call (var "a")
;                           (list (call (var "a") (list))
;                                 (call (var "a") (list (lit 7)))))
;                     (block (var "c")
;                            (block (var "c")
;                                   (call (var "a") (list (var "b") (lit 5)))
;                                   (var "a"))
;                            (var "c"))))
;       (block (var "f")
;              (var "r")
;              (block (var "x")
;                     (call (var "f") (list (lit 9)))
;                     (call (var "r") (list (var "x"))))))

;(block
; (var "r")
; (block
;  (var "a")
;  (op "+")
;  (block
;   (var "b")
;   (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7)))))
;   (block
;    (var "c")
;    (block
;     (var "c")
;     (call
;      (op "+")
;      (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;     (op "+"))
;    (block
;     (var "c")
;     (call
;      (op "+")
;      (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;     (op "+")))))
; (block
;  (var "f")
;  (block
;   (var "a")
;   (op "+")
;   (block
;    (var "b")
;    (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7)))))
;    (block
;     (var "c")
;     (block
;      (var "c")
;      (call
;       (op "+")
;       (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;      (op "+"))
;     (block
;      (var "c")
;      (call
;       (op "+")
;       (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;      (op "+")))))
;  (block
;   (var "x")
;   (call
;    (block
;     (var "a")
;     (op "+")
;     (block
;      (var "b")
;      (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7)))))
;      (block
;       (var "c")
;       (block
;        (var "c")
;        (call
;         (op "+")
;         (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;        (op "+"))
;       (block
;        (var "c")
;        (call
;         (op "+")
;         (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;        (op "+")))))
;    (list (lit 9)))
;   (call
;    (block
;     (var "a")
;     (op "+")
;     (block
;      (var "b")
;      (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7)))))
;      (block
;       (var "c")
;       (block
;        (var "c")
;        (call
;         (op "+")
;         (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;        (op "+"))
;       (block
;        (var "c")
;        (call
;         (op "+")
;         (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;        (op "+")))))
;    (list
;     (call
;      (block
;       (var "a")
;       (op "+")
;       (block
;        (var "b")
;        (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7)))))
;        (block
;         (var "c")
;         (block
;          (var "c")
;          (call
;           (op "+")
;           (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;          (op "+"))
;         (block
;          (var "c")
;          (call
;           (op "+")
;           (list (call (op "+") (list (call (op "+") '()) (call (op "+") (list (lit 7))))) (lit 5)))
;          (op "+")))))
;      (list (lit 9))))))))


