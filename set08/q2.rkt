;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "q1.rkt")

(provide
 tie
 defeated
 defeated?
 outranks
 outranked-by
 power-ranking)

(check-location "08" "q2.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comp-in-tie? : Competitor Tie -> Boolean
;; GIVEN : a Competitor c1 and a Tie oc
;; RETURNS : true, if c1 is present as one of the Competitors in oc else false
;; EXAMPLES :
;;(comp-in-tie? "A"  (tie "B" "C"))  => false
;;(comp-in-tie? "A"  (tie "A" "B"))  => true
;;(comp-in-tie? "A"  (tie "B" "A"))  => true
;; DESIGN STRATEGY : use the observer template for Tie

(define (comp-in-tie? c1 oc)
  (or
   (string=? (tie-outcome-c1 oc) c1)
   (string=? (tie-outcome-c2 oc) c1)))




(begin-for-test
  (check-equal? (comp-in-tie? "A"  (tie "B" "C")) false
                "A is not in Tie")
  (check-equal? (comp-in-tie? "A"  (tie "A" "B")) true
                "A Ties B")
  (check-equal? (comp-in-tie? "A"  (tie "B" "A")) true
                "B ties A"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comp-defeated-defeat? : Competitor Defeat -> Boolean
;; GIVEN : a Competitor c1 and an Defeat oc
;; RETURNS : true, if the outcome states that c1 defeated another Competitor.
;; EXAMPLES:
;;(comp-defeated-defeat? "A" (defeated "A" "B")) =>  true
;;(comp-defeated-defeat? "B" (defeated "A" "B")) =>  false
;;(comp-defeated-defeat? "C" (defeated "A" "B")) =>  false
;; DESIGN STRATEGY : make sure the defeat-outcome-c1 is c1


(define (comp-defeated-defeat? c1 oc)
   (string=? (defeat-outcome-c1 oc) c1))




(begin-for-test
  (check-equal? (comp-defeated-defeat? "A" (defeated "A" "B")) true
                "A defeats B")
  (check-equal? (comp-defeated-defeat? "B" (defeated "A" "B")) false
                "A defeats B")
  (check-equal? (comp-defeated-defeat? "C" (defeated "A" "B")) false
                "C is not in Defeat"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comp-defeated? : Competitor Outcome -> Boolean
;; GIVEN : a Competitor c1 and an Outcome oc
;; RETURNS : true, if the outcome states that c1 defeated or tied with another
;;           Competitor.
;; EXAMPLES:
;; (comp-defeated? "A" (defeated "A" "B")) =>  true
;; (comp-defeated? "A" (defeated "B" "A")) => false
;; (comp-defeated? "A" (tie "A" "B")) => true
;; (comp-defeated? "A" (tie "B" "A")) => true
;; DESIGN STRATEGY : use the observer template for Outcome.

(define (comp-defeated? c1 oc)
  (cond
    [(defeat-outcome? oc) (comp-defeated-defeat? c1 oc)]
    [else (comp-in-tie? c1 oc)]))



(begin-for-test
  (check-equal? (comp-defeated? "A" (defeated "A" "B")) true
                "A defeats B")
  (check-equal? (comp-defeated? "A" (defeated "B" "A")) false
                "A defeats B")
  (check-equal? (comp-defeated? "A" (tie "A" "B")) true
                "A ties B")
  (check-equal? (comp-defeated? "A" (tie "B" "A")) true
                "B ties A"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-losing-numerator : Competitor OutcomeList -> Integer
;; GIVEN : a Competitor c1, an OutcomeList oclst
;; RETURNS : an Integer which represents the number of Outcomes in oclst which
;;           states that c1 defeated another Competitor.
;; EXAMPLES :
;;(non-losing-numerator
;; "A"
;; (list (defeated "A" "B") (defeated "B" "A") (tie "B" "C")))  => 1
;;(non-losing-numerator
;; "B"
;; (list (defeated "A" "B") (defeated "B" "A") (tie "B" "C"))) => 2
;;(non-losing-numerator
;; "B"
;; (list (defeated "A" "B") (defeated "B" "A") (defeated "B" "C"))) => 0
;;(non-losing-numerator
;;                 "X"
;;                 (list (defeated "X" "Y")
;;                       (defeated "X" "Y")
;;                       (defeated "A" "B"))) => 2
;; DESIGN STRATEGY : use HOF foldr


(define (non-losing-numerator c1 oclst)
  (foldr
   (lambda (oc r) (+ (if (comp-defeated? c1 oc) 1 0) r))
   0
   oclst))



(begin-for-test
  (check-equal? (non-losing-numerator
                 "A"
                 (list (defeated "A" "B") (defeated "B" "A") (tie "B" "C")))
                1
                "should be 1")
  
  (check-equal? (non-losing-numerator
                 "B"
                 (list (defeated "A" "B") (defeated "B" "A") (tie "B" "C")))
                2
                "should be 2")
  
  (check-equal? (non-losing-numerator
                 "C"
                 (list (defeated "A" "B")
                       (defeated "B" "A")
                       (defeated "B" "C")))
                0
                "should be 0")
  (check-equal? (non-losing-numerator
                 "X"
                 (list (defeated "X" "Y")
                       (defeated "X" "Y")
                       (defeated "A" "B")))
                2
                "should be 2"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comp-in-defeat? : Competitor Defeat -> Boolean
;; GIVEN : a Competitor c1 and a Defeat oc
;; RETURNS : true, if c1 is present as one of the Competitors in oc else false
;; EXAMPLES:
;;(comp-in-defeat? "A" (defeated "A" "B")) => true
;;(comp-in-defeat? "A" (defeated "B" "A")) => true
;;(comp-in-defeat? "A" (defeated "C" "D")) => false
;; DESIGN STRATEGY : use the observer template for Defeat

(define (comp-in-defeat? c1 oc)
  (or
   (string=? (defeat-outcome-c1 oc) c1)
   (string=? (defeat-outcome-c2 oc) c1)))


(begin-for-test
  (check-equal? (comp-in-defeat? "A" (defeated "A" "B")) true)
  (check-equal? (comp-in-defeat? "A" (defeated "B" "A")) true)
  (check-equal? (comp-in-defeat? "A" (defeated "C" "D")) false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comp-in? : Competitor Outcome -> Boolean
;; GIVEN : a Competitor c1 and an Outcome oc
;; RETURNS : true, if c1 is present as one of the Competitors in oc else false
;; EXAMPLES :
;;(comp-in? "A" (defeated "A" "B")) => true
;;(comp-in? "A" (defeated "B" "A")) => true
;;(comp-in? "A" (defeated "C" "D")) => false
;;(comp-in? "A"  (tie "B" "C"))  => false
;;(comp-in? "A"  (tie "A" "B"))  => true
;;(comp-in? "A"  (tie "B" "A"))  => true
;; DESIGN STRATEGY : call simpler functions.

(define (comp-in? c1 oc)
  (cond
    [(tie-outcome? oc) (comp-in-tie? c1 oc)]
    [else (comp-in-defeat? c1 oc)]))



(begin-for-test
  (check-equal? (comp-in? "A" (defeated "A" "B")) true)
  (check-equal? (comp-in? "A" (defeated "B" "A")) true)
  (check-equal? (comp-in? "A" (defeated "C" "D")) false)
  (check-equal? (comp-in? "A"  (tie "B" "C")) false)
  (check-equal? (comp-in? "A"  (tie "A" "B")) true)
  (check-equal? (comp-in? "A"  (tie "B" "A")) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-losing-denominator : Competitor OutcomeList -> Integer
;; GIVEN : a Competitor c1 and an OutcomeList oclst
;; RETURNS :  number of outcomes that mention c1 in oclst.
;; EXAMPLES :
;;(non-losing-denominator
;;                 "A"
;;                 (list (defeated "A" "B") (tie "B" "C"))) => 1
;;(non-losing-denominator
;;                 "A"
;;                 (list (tie "A" "B") (tie "B" "C") (defeated "C" "A"))) => 2
;; (non-losing-denominator
;;                 "A"
;;                 (list (defeated "A" "B")
;;                       (tie "C" "A")
;;                       (defeated "A" "B"))) => 3
;; DESIGN STRATEGY : use HOF foldr.
(define (non-losing-denominator c1 oclst)
  (foldr
   (lambda (oc r) (+ (if (comp-in? c1 oc) 1 0) r))
   0
   oclst))



(begin-for-test
  (check-equal? (non-losing-denominator
                 "A"
                 (list (defeated "A" "B") (tie "B" "C")))
                1
                "should be 1")
  
  (check-equal? (non-losing-denominator
                 "A"
                 (list (tie "A" "B") (tie "B" "C") (defeated "C" "A")))
                2
                "should be 2"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-losing-percent : Competitor OutcomeList -> Real
;; GIVEN : a Competitor c1 and an OutcomeList oclst
;; WHERE : oclst must contain atleast one outcome.
;; RETURNS : the number of outcomes in which c1 defeats or ties another
;;           competitor divided by the number of outcomes that mention c1.
;; EXAMPLES :
;;(non-losing-percent "A"
;;                    (list (defeated "A" "B")
;;                          (tie "B" "A"))) => 1
;;(non-losing-percent "A"
;;                    (list (defeated "A" "B")
;;                          (tie "B" "C")
;;                          (defeated "C" "A")
;;                          (tie "A" "B"))) => 2/3                   
;; DESIGN STRATEGY : call simpler functions.

(define (non-losing-percent c1 oclst)
  (/ (non-losing-numerator c1 oclst)
     (non-losing-denominator c1 oclst)))



(begin-for-test
  (check-equal? (non-losing-percent "A"
                                    (list (defeated "A" "B")
                                          (tie "B" "A")))
                1
                "should be 1")
  (check-equal? (non-losing-percent "A"
                                    (list (defeated "A" "B")
                                          (tie "B" "C")
                                          (defeated "C" "A")
                                          (tie "A" "B")))
                2/3
                "should be 2/3"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; power-rank-greater-than : Competitor Competitor OutcomeList -> Boolean
;; GIVEN : a Competitor c1, a Competitor c2, and an OutcomeList oclst
;; WHERE : c1 != C2
;; AND : oclst must atleast contain one Outcome.
;; RETURNS : true, if the power-rank of c1 is greater than c2
;; EXAMPLES :
;;(power-rank-greater-than "A" "B" (list (defeated "A" "B"))) => true
;;(power-rank-greater-than "A" "B" (list (tie "B" "A"))) => true
;;(power-rank-greater-than "B" "A" (list (tie "B" "A"))) => false
;;(power-rank-greater-than
;;                 "A"
;;                 "B"
;;                 (list (defeated "E" "A")
;;                       (defeated "E" "B")
;;                       (defeated "A" "D"))) => true
;;(power-rank-greater-than
;;                 "A"
;;                 "B"
;;                 (list (defeated "E" "A")
;;                       (defeated "E" "B")
;;                       (defeated "B" "D"))) => false
;;(power-rank-greater-than
;;                 "B"
;;                 "A"
;;                 (list (defeated "B" "X")
;;                       (defeated "C" "B")
;;                       (defeated "A" "C")
;;                       (defeated "B" "E")
;;                       (tie "A" "X")
;;                       (defeated "E" "F"))) => false
;;(power-rank-greater-than
;;                 "A"
;;                 "B"
;;                 (list (defeated "B" "X")
;;                       (defeated "C" "B")
;;                       (defeated "A" "C")
;;                       (defeated "B" "E")
;;                       (tie "A" "X")
;;                       (defeated "E" "F"))) => true
;; DESIGN STRATEGY : call simper functions.

(define (power-rank-greater-than c1 c2 oclst)
  (cond
    [(< (length (outranked-by c1 oclst)) (length (outranked-by c2 oclst)))
     true]
    [(> (length (outranked-by c1 oclst)) (length (outranked-by c2 oclst)))
     false]
    [(> (length (outranks c1 oclst)) (length (outranks c2 oclst))) true]
    [(< (length (outranks c1 oclst)) (length (outranks c2 oclst))) false]
    [(> (non-losing-percent c1 oclst) (non-losing-percent c2 oclst)) true]
    [(< (non-losing-percent c1 oclst) (non-losing-percent c2 oclst)) false]
    [(string<? c1 c2) true]
    [else false]))



(begin-for-test
  (check-equal? (power-rank-greater-than "A" "B" (list (defeated "A" "B")))
                true
                "B is outranked by more competitors than A")
  (check-equal? (power-rank-greater-than "A" "B" (list (tie "B" "A")))
                true
                "string<? A B results in true")
  (check-equal? (power-rank-greater-than "B" "A" (list (tie "B" "A")))
                false
                "string<? A B results in false")
  (check-equal? (power-rank-greater-than
                 "A"
                 "B"
                 (list (defeated "E" "A")
                       (defeated "E" "B")
                       (defeated "A" "D")))
                true
                "A outranks more Competitors that B")
  (check-equal? (power-rank-greater-than
                 "A"
                 "B"
                 (list (defeated "E" "A")
                       (defeated "E" "B")
                       (defeated "B" "D")))
                false
                "B outranks more Competitors than A")
  (check-equal? (power-rank-greater-than
                 "B"
                 "A"
                 (list (defeated "B" "X")
                       (defeated "C" "B")
                       (defeated "A" "C")
                       (defeated "B" "E")
                       (tie "A" "X")
                       (defeated "E" "F")))
                false
                "non-losing % of A > B")
  (check-equal? (power-rank-greater-than
                 "A"
                 "B"
                 (list (defeated "B" "X")
                       (defeated "C" "B")
                       (defeated "A" "C")
                       (defeated "B" "E")
                       (tie "A" "X")
                       (defeated "E" "F")))
                true
                "non-losing % of A > B"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; odd-elements : CompetitorList -> CompetitorList
;; GIVEN : a CompetitorList clst
;; WHERE : the first elemet in the list at an odd position.
;; RETURNS : a CompetitorList containing only elements at the odd positions in
;;           the list
;; EXAMPLES :
;; (odd-elements (list "A" "B" "C" "D")) => (list "A" "C")
;; (odd-elements (list "A" "B" "C")) => (list "A" "C")
;; (odd-elements empty) => empty
;; DESIGN STRATEGY : use the observer template for CompetitorList.

(define (odd-elements clst)
  (cond
    [(empty? clst) empty]
    [(empty? (rest clst)) clst]
    [else
     (cons (first clst)
        (odd-elements (rest (rest clst))))]))




(begin-for-test
  (check-equal? (odd-elements (list "A" "B" "C" "D")) (list "A" "C"))
  (check-equal? (odd-elements (list "A" "B" "C")) (list "A" "C"))
  (check-equal? (odd-elements empty) empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; even-elements : CompetitorList -> CompetitorList
;; GIVEN : a CompetitorList clst
;; WHERE : the first element in the list at an odd position.
;; RETURNS : a CompetitorList containing only elements at the odd positions in
;;           the list
;; EXAMPLES :
;; (even-elements (list "A" "B" "C" "D")) => (list "B" "D")
;; (even-elements (list "A" "B" "C" "D" "E")) => (list "B" "D")
;; (even-elements empty) => empty
;; DESIGN STRATEGY : call a simpler function.

(define (even-elements clst)
  (cond
    [(empty? clst) empty]
    [else (odd-elements (rest clst))]))



(begin-for-test
  (check-equal? (even-elements (list "A" "B" "C" "D")) (list "B" "D"))
  (check-equal? (even-elements (list "A" "B" "C" "D" "E")) (list "B" "D"))
  (check-equal? (even-elements empty) empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; merge : CompetitorList  CompetitorList OutcomeList -> CompetitorList
;; GIVEN : a CompetitorList clst1, CompetitorList clst2 and an OutcomeList oclst
;; WHERE : both lists are sorted, with competitor A
;;         coming before competitor B in the list if and only if
;;         the power-ranking of A is higher than the power ranking
;;         of B.
;; RETURNS : the sorted merge of its two arguments
;; EXAMPLES :
;; (merge (list "C" "A" "F")
;;                        (list "E" "B" "D")
;;                        (list (defeated "A" "D")
;;                              (defeated "A" "E")
;;                              (defeated "C" "B")
;;                              (defeated "C" "F")
;;                              (tie "D" "B")
;;                              (defeated "F" "E")))
;; => (list "C" "A" "F" "E" "B" "D")
;;
;; (merge (list "C" "F" "B")
;;                        (list "A" "E" "D")
;;                        (list (defeated "A" "D")
;;                              (defeated "A" "E")
;;                              (defeated "C" "B")
;;                              (defeated "C" "F")
;;                              (tie "D" "B")
;;                              (defeated "F" "E")))
;; => (list "C" "A" "F" "E" "B" "D")
;;
;; DESIGN STRATEGY : recur on (rest clst1) or (rest clst2)
;; HALTING MEASURE : (length clst1) + (length clst2)
;; JUSTIFICATION :
;;-- (length clst1) and (length clst2) are both always non-negative, therefore
;;    their sum is non-negative.
;;-- At each recursive call, either clst1 or clst2 becomes shorter, therefore
;;   the sum of their lengths is always shorter.

(define (merge clst1 clst2 oclst)
  (cond
    [(empty? clst1) clst2]
    [(empty? clst2) clst1]
    [(power-rank-greater-than (first clst1) (first clst2) oclst)
     (cons (first clst1) (merge (rest clst1) clst2 oclst))]
    [else
     (cons (first clst2) (merge clst1 (rest clst2) oclst))]))



(begin-for-test
  (check-equal?  (merge (list "C" "A" "F")
                        (list "E" "B" "D")
                        (list (defeated "A" "D")
                              (defeated "A" "E")
                              (defeated "C" "B")
                              (defeated "C" "F")
                              (tie "D" "B")
                              (defeated "F" "E")))
                 (list "C" "A" "F" "E" "B" "D")
                 "must be a sorted list based on power-rank")
  
  (check-equal? (merge (list "C" "F" "B")
                        (list "A" "E" "D")
                        (list (defeated "A" "D")
                              (defeated "A" "E")
                              (defeated "C" "B")
                              (defeated "C" "F")
                              (tie "D" "B")
                              (defeated "F" "E")))
                (list "C" "A" "F" "E" "B" "D")
                "must be a sorted list based on power-rank"))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; merge-sort : CompetitorList OutcomeList -> CompetitorList
;; GIVEN : a CompetitorList clst and an OutcomeList oclst
;; WHERE : clst and OutcomeList must contain atleast one elemet each.
;; RETURNS : a CompetitorList like clst but sorted, with competitor A
;;         coming before competitor B in the list if and only if
;;         the power-ranking of A is higher than the power ranking
;;         of B.
;; EXAMPLES :
;;(merge-sort
;; (list "A" "B" "C" "D" "E" "F")
;; (list (defeated "A" "D")
;;       (defeated "A" "E")
;;       (defeated "C" "B")
;;       (defeated "C" "F")
;;       (tie "D" "B")
;;       (defeated "F" "E")))
;;=> (list "C" "A" "F" "E" "B" "D")
;; DESIGN STRATEGY : recur on clst
;; HALTING MEASURE : (length clst)
;; JUSTIFICATION :
;; -- (length clst) is always a non negative integer
;; -- At each recursive call (length clst) >= 2
;;    (length  (even-elements clst)) and (length (odd-elements clst)) both are
;;    strictly less than (length of clst)

(define (merge-sort clst oclst)
  (cond
    [(empty? (rest clst)) clst]
    [else
     (merge
      (merge-sort (even-elements clst) oclst)
      (merge-sort (odd-elements clst) oclst)
      oclst)]))



(begin-for-test
  (check-equal? (merge-sort
                 (list "A" "B" "C" "D" "E" "F")
                 (list (defeated "A" "D")
                       (defeated "A" "E")
                       (defeated "C" "B")
                       (defeated "C" "F")
                       (tie "D" "B")
                       (defeated "F" "E")))
                (list "C" "A" "F" "E" "B" "D"))
  (check-equal? (merge-sort (list "A" "B" "C")
                            (list (defeated "A" "B") (tie "B" "C")))
                (list "A" "C" "B")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; power-ranking : OutcomeList -> CompetitorList
;; GIVEN: a list of outcomes
;; RETURNS: a list of all competitors mentioned by one or more
;;     of the outcomes, without repetitions, with competitor A
;;     coming before competitor B in the list if and only if
;;     the power-ranking of A is higher than the power ranking
;;     of B.
;; EXAMPLE:
;;     (power-ranking
;;      (list (defeated "A" "D")
;;            (defeated "A" "E")
;;            (defeated "C" "B")
;;            (defeated "C" "F")
;;            (tie "D" "B")
;;            (defeated "F" "E")))
;;  => (list "C"   ; outranked by 0, outranks 4
;;           "A"   ; outranked by 0, outranks 3
;;           "F"   ; outranked by 1
;;           "E"   ; outranked by 3
;;           "B"   ; outranked by 4, outranks 12, 50%
;;           "D")  ; outranked by 4, outranks 12, 50%
;; DESIGN STRATEGY : call a simpler function.

(define (power-ranking oclst)
  (merge-sort (comp-list oclst empty) oclst))


(begin-for-test
  (check-equal? 
   (power-ranking
    (list (defeated "A" "D")
          (defeated "A" "E")
          (defeated "C" "B")
          (defeated "C" "F")
          (tie "D" "B")
          (defeated "F" "E")))
   (list "C" "A" "F" "E" "B" "D")))