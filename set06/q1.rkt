;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide inner-product
         permutation-of?
         shortlex-less-than?
         permutations)

(check-location "06" "q1.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ZERO 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inner-product : RealList RealList -> Real
;; GIVEN: two lists of real numbers
;; WHERE: the two lists have the same length
;; RETURNS: the inner product of those lists
;; EXAMPLES:
;;     (inner-product (list 2.5) (list 3.0))  =>  7.5
;;     (inner-product (list 1 2 3 4) (list 5 6 7 8))  =>  70
;;     (inner-product (list) (list))  =>  0
;; DESIGN STRATEGY: use HOF foldr on the output of HOF map.


(define (inner-product rl1 rl2)
  (foldr
    ;; Real Real Real -> Real
    ;; GIVEN : three Real numbers
    ;; RETURNS : the product  of the first two numbers summed up with the third.
   (lambda (x y z) (+ (* x y) z))
         ZERO rl1 rl2))


(begin-for-test
  (check-equal? (inner-product (list 2.5) (list 3.0)) 7.5
                "inner product of the lists should be  7.5")
  (check-equal? (inner-product (list 1 2 3 4) (list 5 6 7 8)) 70
                "inner product of the lists should be 70")
  (check-equal? (inner-product (list) (list)) 0
                "inner produt of 2 empty lists is 0"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; permutation-of? : IntList IntList -> Boolean
;; GIVEN: two lists of integers
;; WHERE: neither list contains duplicate elements
;; RETURNS: true if and only if one of the lists
;;     is a permutation of the other
;; EXAMPLES:
;;     (permutation-of? (list 1 2 3) (list 1 2 3)) => true
;;     (permutation-of? (list 3 1 2) (list 1 2 3)) => true
;;     (permutation-of? (list 3 1 2) (list 1 2 4)) => false
;;     (permutation-of? (list 1 2 3) (list 1 2)) => false
;;     (permutation-of? (list) (list)) => true
;; DESIGN STRATEGY : combine simpler functions.

(define (permutation-of? ilst1 ilst2)
  (equal? (sort ilst1 <) (sort ilst2 <)))

(begin-for-test
  (check-equal? (permutation-of? (list 1 2 3) (list 1 2 3)) true
                "list (list 1 2 3) is a permutation of itself.")
  (check-equal? (permutation-of? (list 3 1 2) (list 1 2 3)) true
                "(list 3 1 2) is a permutation of (list 1 2 3)")
  (check-equal? (permutation-of? (list 3 1 2) (list 1 2 4)) false
                "(list 3 1 2) is not a permutation of (list 1 2 4)")
  (check-equal? (permutation-of? (list 1 2 3) (list 1 2)) false
                "(list 1 2 3) is not a permutation of (list 1 2)")
  (check-equal? (permutation-of? (list) (list)) true
                "empty list is a permutation of itself."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; length-less : IntList IntList -> Boolean
;; GIVEN: two lists of integers
;; RETURNS: true only if the length of the first list is lesser than the second.
;; EXAMPLES:
;; (length-less (list 3) (list 1 2)) => true
;; (length-less (list 1 2) (list 3)) => false
;; DESIGN STRATEGY :  use predefined functions by ISL.

(define (length-less ilst1 ilst2)
  (< (length ilst1) (length ilst2)))

(begin-for-test
  (check-equal? (length-less (list 3) (list 1 2)) true
                "length of (list 3) is less than (list 1 2).")
  (check-equal? (length-less (list 1 2) (list 3)) false
                "length of (list 1 2) is greater than (list 3)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; length-equal : IntList IntList -> Boolean
;; GIVEN: two lists of integers
;; RETURNS: true only if the length of both lists are equal.
;; EXAMPLES:
;; (length-equal (list 3) (list 3)) => true
;; (length-equal (list) (list)) => true
;; (length-less (list 1 2) (list 3)) => false
;; DESIGN STRATEGY :  use predefined functions by ISL.

(define (length-equal ilst1 ilst2)
  (equal? (length ilst1) (length ilst2)))

(begin-for-test
  (check-equal? (length-equal (list 3) (list 3)) true
                "length of (list 3) is equal to (list 3.)")
  (check-equal? (length-equal (list) (list)) true
                "length of (list) is equal to (list).")
  (check-equal? (length-equal (list 1 2) (list 3)) false
                "length of (list 1 2) is not equal to (list 3)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shortlex-less-than? : IntList IntList -> Boolean
;; GIVEN: two lists of integers
;; RETURNS: true if and only either
;;     the first list is shorter than the second
;;  or both are non-empty, have the same length, and either
;;         the first element of the first list is less than
;;             the first element of the second list
;;      or the first elements are equal, and the rest of
;;             the first list is less than the rest of the
;;             second list according to shortlex-less-than?
;; EXAMPLES:
;;     (shortlex-less-than? (list) (list)) => false
;;     (shortlex-less-than? (list) (list 3)) => true
;;     (shortlex-less-than? (list 3) (list)) => false
;;     (shortlex-less-than? (list 3) (list 3)) => false
;;     (shortlex-less-than? (list 3) (list 1 2)) => true
;;     (shortlex-less-than? (list 3 0) (list 1 2)) => false
;;     (shortlex-less-than? (list 0 3) (list 1 2)) => true
;; DESIGN STRATEGY : combine simpler functions.

(define (shortlex-less-than? ilst1 ilst2)
  (cond
    [(length-less ilst1 ilst2)  true]
    [(or (empty? ilst1) (empty? ilst2)) false]
    [(and (length-equal ilst1 ilst2)
          (< (first ilst1) (first ilst2))) true]
    [(= (first ilst1) (first ilst2))
     (shortlex-less-than? (rest ilst1) (rest ilst2))]
    [else false]))
      

(begin-for-test
  (check-equal? (shortlex-less-than? (list) (list)) false
                "empty lists as input should result in false.")
  (check-equal? (shortlex-less-than? (list) (list 3)) true
                "length of (list) is less (list 3), should be true.")
  (check-equal? (shortlex-less-than? (list 3) (list)) false
                "length of (list 3) is greater than (list), should be false")
  (check-equal? (shortlex-less-than? (list 3) (list 3)) false
                "length of (list 3) equals (list 3), hence false.")
  (check-equal? (shortlex-less-than? (list 3) (list 1 2)) true
                "length of (list 3) is less than (list 1 2), hence true.")
  (check-equal? (shortlex-less-than? (list 3 0) (list 1 2)) false
                "3 > 1, hence false.")
  (check-equal? (shortlex-less-than? (list 0 3) (list 1 2)) true
                "0 < 1, hence true."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; single-spot : IntList Int Int Int -> IntList
;; GIVEN : a list of Integers lst, the Integer to be inserted i, spot s at which
;; the Integer is to be inserted and an Integer ctr whose value to be provided
;; is always 0
;; RETURNS : a list of Integers like lst but with i inserted at s.
;; EXAMPLES :
;; (single-spot (list 1 3) 2 1 0) => (list 1 2 3)
;; (single-spot (list 2 3) 1 0 0) => (list 1 2 3)
;; (single-spot (list 1 2) 3 2 0) => (list 1 2 3)
;; DESIGN STRATEGY : use HOF foldr. 

(define (single-spot lst num spot)
  ;; insert Int -> IntList
  ;; GIVEN : an Int i
  ;; RETURNS : an IntList with num in front of the list if
  ;;           x is equal to the element is at spot in the list.
  (local ((define (insert-at x)
         (if (= x (list-ref lst spot))
             (list num x)
             (list x))))
  (cond
    [(= spot (length lst)) (append lst (list num))]
    [else (foldr (lambda (x xl) (append  (insert-at x) xl)) empty lst)])))
  


(begin-for-test
  (check-equal? (single-spot (list 1 3) 2 1 ) (list 1 2 3)
                "inserting 2 at pos 1. => (list 1 2 3)")
  (check-equal? (single-spot (list 2 3) 1 0 ) (list 1 2 3)
                "inserting 1 at pos 0. => (list 1 2 3)")
  (check-equal? (single-spot (list 1 2) 3 2 ) (list 1 2 3)
                "inserting 3 at pos 2. => (list 1 2 3)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; every-spot-list : IntList Int Int -> IntListList
;; GIVEN : a list of Integers lst, the Integer to be inserted i
;; and an Integer ctr whose value to be provided is always 0
;; RETURNS : a list of list of Integers, where each list is like lst
;; but with i at a single location which is different on every list.
;; EXAMPLES :
;; (every-spot-list (list 1 2 3) 4 0) =>
;;(list (list 4 1 2 3)
;;      (list 1 4 2 3)
;;      (list 1 2 4 3)
;;      (list 1 2 3 4))
;; DESIGN STRATEGY : use HOF map

(define (every-spot-list lst num)
  (map
   ;; Int -> IntList
   ;; GIVEN : a position to insert num in IntLst lst
   ;; RETURNS : IntList like lst but with num inserted at the given position.
   (lambda (x) (single-spot lst num x))
       (build-list (+ (length lst) 1) (lambda (x) x))))

(begin-for-test
  (check-equal? (every-spot-list (list 1 2 3) 4) (list (list 4 1 2 3)
                                                         (list 1 4 2 3)
                                                         (list 1 2 4 3)
                                                         (list 1 2 3 4))
                "insert 4 at every single position on the list."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; every-combo : IntListList num -> IntListList
;; GIVEN : an Integer i, and list of list of Integers ll 
;; RETURNS: list of list of Integers like ll but with i inserted at every
;; location on every list in ll.
;; EXAMPLE :
;; (every-combo (list (list 1 2) (list 2 1)) 3) =>
;; (list (list 3 1 2)
;;         (list 1 3 2)
;;         (list 1 2 3)
;;         (list 3 2 1)
;;         (list 2 3 1)
;;         (list 2 1 3))
;; DESIGN STRATEGY : use HOF foldr

(define (every-combo lst-of-lst num)
  (foldr
   ;; IntLst IntListList -> IntListList
   ;; GIVEN : an IntList l and IntListList ll
   ;; RETURNS : puts num in every position in l and appends it to ll.
   (lambda (lst lstp) (append (every-spot-list lst num) lstp))
   empty
   lst-of-lst))

(begin-for-test
  (check-equal? (every-combo (list (list 1 2) (list 2 1)) 3)
                (list (list 3 1 2)
                      (list 1 3 2)
                      (list 1 2 3)
                      (list 3 2 1)
                      (list 2 3 1)
                      (list 2 1 3)))
  "insert given number at every single position in evry single given list.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; permutations : IntList -> IntListList
;; GIVEN: a list of integers
;; WHERE: the list contains no duplicates
;; RETURNS: a list of all permutations of that list,
;;     in shortlex order
;; EXAMPLES:
;;     (permutations (list))  =>  (list (list))
;;     (permutations (list 9))  =>  (list (list 9))
;;     (permutations (list 3 1 2))
;;         =>  (list (list 1 2 3)
;;                   (list 1 3 2)
;;                   (list 2 1 3)
;;                   (list 2 3 1)
;;                   (list 3 1 2)
;;                   (list 3 2 1))
;; DESIGN STRATEGY : combine simpler functions.

(define (permutations lst)
  (cond
    [(empty? lst) (list empty)]
    [(empty? (rest lst)) (list lst)]
    [else (sort
           (every-combo (permutations (rest lst)) (first lst))
           shortlex-less-than?)]))

(begin-for-test
  (check-equal? (permutations (list))
                (list (list))
                "all permutation of empty list is empty.")
  (check-equal? (permutations (list 9))
                (list (list 9))
                "all permutations of a list with one element is same list.")
  (check-equal?  (permutations (list 3 1 2))
                 (list (list 1 2 3)
                       (list 1 3 2)
                       (list 2 1 3)
                       (list 2 3 1)
                       (list 3 1 2)
                       (list 3 2 1))
                 "3 element list has 3! permutations."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
