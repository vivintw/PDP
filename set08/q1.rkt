;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide
 tie
 defeated
 defeated?
 outranks
 outranked-by
 comp-list
 defeat-outcome?
 tie-outcome?
 tie-outcome-c1
 tie-outcome-c2
 defeat-outcome-c1
 defeat-outcome-c2)

(check-location "08" "q1.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Competitor is represented as a String (any string will do).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tie is defined as a struct:
;; (make-tie-outcome c1 c2)

;; INTERP :
;; c1, c2 : competitors who have taken part in a match
;; WHERE : the outcome of the match was a tie. 

;; IMPLEMENTATION:
(define-struct tie-outcome (c1 c2))

;; CONSTRUCTOR TEMPLATE:
;; (make-tie-outcome Competitor Competitor)

;; OBSERVER TEMPLATE:
;; tie-fn : Tie -> ??
#;
(define (tie-fn t)
  (... (tie-outcome-c1 t)
       (tie-outcome-c2 t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defeat is defined as a struct:
;;(make-defeat-outcome c1 c2)

;; INTERP:
;; c1, c2 : competitors who have taken part in a match
;; WHERE : c1 defeated c2.

;; IMPLEMENTATION:
(define-struct defeat-outcome (c1 c2))

;; CONSTRUCTOR TEMPLATE:
;; (make-defeat-outcome Competitor Competitor)

;; OBSERVER TEMPLATE:
;; defeat-fn : Defeat -> ??
#;
(define (defeat-fn d)
  (... (defeat-outcome-c1 d)
       (defeat-outcome-c2 d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Outcome is one of
;;     -- a Tie
;;     -- a Defeat
;;
;; OBSERVER TEMPLATE:
;; outcome-fn : Outcome -> ??
#;
(define (outcome-fn o)
  (cond ((tie? o) ...)
        ((defeat? o) ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An OutcomeList is one of
;; -- empty
;; -- (cons Outcome OutcomeList)

;;CONSTRUCTOR TEMPLATE:
;; -- empty
;; -- (cons Outcome OutcomeList)

;; OBSERVER TEMPLATE:
;; oc-list-fn: OutcomeList -> ??
#;
(define (oc-list-fn oclst)
  (cond
    [(empty? oclst) empty]
    [else (...
           (outcome-fn (first oclst))
           (oc-list-fn (rest oclst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CompetitorList is one of
;; -- empty
;; -- (cons Competitor CompetitorList)

;; CONSTRUCTOR TEMPLATE:
;; --empty
;; --(cons Competitor CompetitorList)

;; OBSERVER TEMPLATE:
;; cl-fn : CompetitorList -> ??
#;
(define (cl-fn cl)
  (cond
    [(empty? cl) empty]
    [else (...
           (first cl)
           (cl-fn (rest cl)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tie : Competitor Competitor -> Tie
;; GIVEN: the names of two competitors
;; RETURNS: an indication that the two competitors have
;;     engaged in a contest, and the outcome was a tie
;; EXAMPLE:
;; (tie "A" "B") => (make-tie-outcome "A" "B")
;; DESIGN STRATEGY : use the constructor template for Tie.

(define (tie c1 c2)
  (make-tie-outcome c1 c2))

(begin-for-test
  (check-equal? (tie "A" "B") (make-tie-outcome "A" "B")
                "must return an element of type Tie."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defeated : Competitor Competitor -> Defeat
;; GIVEN: the names of two competitors
;; RETURNS: an indication that the two competitors have
;;     engaged in a contest, with the first competitor
;;     defeating the second
;; EXAMPLE:
;; (defeated "A" "B") => (make-defeat-outcome "A" "B")
;; DESIGN STRATEGY: use the constructor template for Defeat.
(define (defeated c1 c2)
  (make-defeat-outcome c1 c2))

(begin-for-test
  (check-equal? (defeated "A" "B") (make-defeat-outcome "A" "B")
                "must return an element of type Defeat."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defeat-check : Competitor Competitor Defeat -> Boolean
;; GIVEN : the names of two competitors (c1 and c2) and a Defeat oc.
;; RETURNS : true if Defeat indicates that c1 defeated c2 else false.
;; EXAMPLE:
;; (defeat-check "A" "B" (defeated "A" "B")) => true
;; (defeat-check "A" "B" (defeated "B" "A")) => false
;; (defeat-check "A" "B" (defeated "A" "C")) => false
;; (defeat-check "A" "B" (defeated "C" "D")) => false
;; DESIGN STRATEGY : make sure c1 and c2 are listed as the first and second
;;                   Competitors respectively in oc.


(define (defeat-check c1 c2 oc)
  (and (equal? (defeat-outcome-c1 oc) c1)
       (equal? (defeat-outcome-c2 oc) c2)))


(begin-for-test
  (check-equal? (defeat-check "A" "B" (defeated "A" "B")) true
                "must return true")
  (check-equal? (defeat-check "A" "B" (defeated "B" "A")) false
                "must return false")
  (check-equal? (defeat-check "A" "B" (defeated "A" "C")) false
                "must return false")
  (check-equal? (defeat-check "A" "B" (defeated "C" "D")) false
                "must return false"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tie-inorder-check Competitor Competitor Tie -> Boolean
;; GIVEN : the names of two competitors (c1 and c2) and a Tie oc.
;; RETURNS : true if Tie indicates that c1 ties c2 in the same order else false.
;; EXAMPLE :
;; (tie-inorder-check "A" "B" (tie "A" "B")) => true
;; (tie-inorder-check "B" "A" (tie "A" "B")) => false
;; (tie-inorder-check "A" "B" (tie "A" "B")) => false
;; (tie-inorder-check "A" "B" (tie "C" "D")) => false
;; DESIGN STRATEGY : make sure c1 and c2 are listed as the first and second
;;                   Competitors respectively in oc.


(define (tie-inorder-check c1 c2 oc)
  (and (equal? (tie-outcome-c1 oc) c1)
       (equal? (tie-outcome-c2 oc) c2)))



(begin-for-test
  (check-equal? (tie-inorder-check "A" "B" (tie "A" "B")) true
                "must return true")
  (check-equal? (tie-inorder-check "B" "A" (tie "A" "B")) false
                "must return false")
  (check-equal? (tie-inorder-check "A" "B" (tie "A" "C")) false
                "must return false")
  (check-equal? (tie-inorder-check "A" "B" (tie "C" "D")) false
                "must return false"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tie-check : Competitor Competitor Tie -> Boolean
;; GIVEN : the names of two competitors (c1 and c2) and a Tie oc.
;; RETURNS : true if Tie indicates that c1 ties c2 else false.
;; EXAMPLE :
;; (tie-check "A" "B" (tie "A" "B")) => true
;; (tie-check "B" "A" (tie "A" "B")) => true
;; (tie-check "A" "B" (tie "A" "B")) => false
;; (tie-check "A" "B" (tie "C" "D")) => false
;; DESIGN STRATEGY : call a simpler function.

(define (tie-check c1 c2 oc)
  (or (tie-inorder-check c1 c2 oc)
      (tie-inorder-check c2 c1 oc)))

(begin-for-test
  (check-equal? (tie-check "A" "B" (tie "A" "B")) true
                "must return true")
  (check-equal? (tie-check "B" "A" (tie "A" "B")) true
                "must return true")
  (check-equal? (tie-check "A" "B" (tie "A" "C")) false
                "must return false")
  (check-equal? (tie-check "A" "B" (tie "C" "D")) false
                "must return false"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defeat-or-tie-check : Competitor Competitor Outcome -> Boolean
;; GIVEN : the names of two competitors (c1 and c2) and a Outcome oc.
;; RETURNS : true if the oc indicates that the first competitor has defeated or
;;           tied the second.
;; EXAMPLE:
;; (defeat-or-tie-check "A" "B" (tie "A" "B")) true
;; (defeat-or-tie-check "B" "A" (tie "A" "B")) true
;; (defeat-or-tie-check "A" "B" (tie "A" "B")) false
;; (defeat-or-tie-check "A" "B" (tie "C" "D")) false
;; (defeat-or-tie-check "A" "B" (defeated "A" "B")) true
;; (defeat-or-tie-check "A" "B" (defeated "B" "A")) false
;; (defeat-or-tie-check "A" "B" (defeated "A" "C")) false
;; (defeat-or-tie-check "A" "B" (defeated "C" "D")) false
;; DESIGN STRATEGY : call simpler function


(define (defeat-or-tie-check c1 c2 oc)
  (cond
    [(tie-outcome? oc) (tie-check c1 c2 oc)]
    [else (defeat-check c1 c2 oc)]))

(begin-for-test
  (check-equal? (defeat-or-tie-check "A" "B" (tie "A" "B")) true
                "must return true")
  (check-equal? (defeat-or-tie-check "B" "A" (tie "A" "B")) true
                "must return true")
  (check-equal? (defeat-or-tie-check "A" "B" (tie "A" "C")) false
                "must return false")
  (check-equal? (defeat-or-tie-check "A" "B" (tie "C" "D")) false
                "must return false")
  (check-equal? (defeat-or-tie-check "A" "B" (defeated "A" "B")) true
                "must return true")
  (check-equal? (defeat-or-tie-check "A" "B" (defeated "B" "A")) false
                "must return false")
  (check-equal? (defeat-or-tie-check "A" "B" (defeated "A" "C")) false
                "must return false")
  (check-equal? (defeat-or-tie-check "A" "B" (defeated "C" "D")) false
                "must return false"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defeated? : Competitor Competitor OutcomeList -> Boolean
;; GIVEN: the names of two competitors and a list of outcomes
;; RETURNS: true if and only if one or more of the outcomes indicates
;;     the first competitor has defeated or tied the second
;; EXAMPLES:
;;     (defeated? "A" "B" (list (defeated "A" "B") (tie "B" "C")))
;;  => true
;;
;;     (defeated? "A" "C" (list (defeated "A" "B") (tie "B" "C")))
;;  => false
;;
;;     (defeated? "B" "A" (list (defeated "A" "B") (tie "B" "C")))
;;  => false
;;
;;     (defeated? "B" "C" (list (defeated "A" "B") (tie "B" "C")))
;;  => true
;;
;;     (defeated? "C" "B" (list (defeated "A" "B") (tie "B" "C")))
;;  => true
;; DESIGN STRATEGY : use HOF ormap.


(define (defeated? c1 c2 oclst)
  (cond
    [(empty? oclst) false]
    [else (ormap
           ;; Outcome -> Boolean
           ;; GIVEN : an Outcome x
           ;; RETURNS : true if x indicated that C1 defeated C2 else false.
           (lambda (x) (defeat-or-tie-check c1 c2 x)) oclst)]))

(begin-for-test
  (check-equal? (defeated? "A" "B" (list (defeated "A" "B") (tie "B" "C")))
                true
                "must return true as A defeated B")
  (check-equal? (defeated? "A" "C" (list (defeated "A" "B") (tie "B" "C")))
                false
                "must return false")
  (check-equal? (defeated? "B" "A" (list (defeated "A" "B") (tie "B" "C")))
                false
                "must return false")
  (check-equal? (defeated? "B" "C" (list (defeated "A" "B") (tie "B" "C")))
                true
                "must return true")
  (check-equal? (defeated? "C" "B" (list (defeated "A" "B") (tie "B" "C")))
                true
                "must return true")
  (check-equal? (defeated? "A" "B" empty) false "no Outcomes in Outcomelist"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-append : Competitor CompetitorList -> CompetitorList
;; GIVEN : a Competitor c and a CompetitorList clst
;; RETURNS : a CompetitorList like clst but with c appended to it
;;           if it doesnot already exist in clst.
;; EXAMPLES :
;;(set-append "A" (list "A" "B")) => (list "A" "B")
;;(set-append "B" (list "A" "B")) => (list "A" "B")
;;(set-append "C" (list "A" "B")) => (list "A" "B" "C")
;; DESIGN STRATEGY : make sure c is not a member of clst before appending.

(define (set-append c clst)
  (cond [(member? c clst) clst]
        [else (cons c clst)]))

(begin-for-test
  (check-equal? (set-append "A" (list "A" "B")) (list "A" "B")
                "single A in list")
  (check-equal? (set-append "B" (list "A" "B")) (list "A" "B")
                "single B in list")
  (check-equal? (set-append "C" (list "A" "B")) (list "C" "A" "B")
                "list must include C"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defeat-outcome-list : OutcomeList CompetitorList-> CompetitorList
;; GIVEN : an OutcomeList oclst and a CompetitorList clst
;; WHERE : the first Outcome in the OutcomeList is a Defeat.
;; RETURNS : a CompetitorList like clst but with the the Competitors of the
;;           first Outcome in oclst appended to it.
;; EXAMPLE:
;;(defeat-outcome-list (list (defeat "A" "B") (tie "B" "C")) empty)
;;=> (list "A" "B")
;;
;;(defeat-outcome-list (list (defeat "A" "B") (tie "B" "C")) (list "E"))
;;=> (list "A" "B" "E")
;; DESIGN STRATEGY : call a simpler function.

(define (defeat-outcome-list oclst clst)
  (set-append
   (defeat-outcome-c1 (first oclst))
   (set-append
    (defeat-outcome-c2 (first oclst))
    clst)))

(begin-for-test
  (check-equal?
   (defeat-outcome-list (list (defeated "A" "B") (tie "B" "C")) empty)
   (list "A" "B") "must contain A and B")
  (check-equal?
   (defeat-outcome-list (list (defeated "A" "B") (tie "B" "C")) (list "E"))
   (list "A" "B" "E") "must contain A B and E"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tie-outcome-list : OutcomeList CompetitorList  -> CompetitorList
;; GIVEN : an OutcomeList oclst and a CompetitorList clst
;; RETURNS : a CompetitorList like clst but with the the Competitors of the
;;           first Outcome in oclst appended to it.
;; WHERE : the first Outcome in the OutcomeList is a Tie.
;; EXAMPLE:
;;(tie-outcome-list (list (tie "A" "B") (tie "B" "C")) empty)
;;=> (list "A" "B")
;;
;;(tie-outcome-list (list (tie "A" "B") (tie "B" "C")) (list "E"))
;;=> (list "A" "B" "E")
;; DESIGN STRATEGY : call a simpler function.

(define (tie-outcome-list oclst clst)
  (set-append
   (tie-outcome-c1 (first oclst))
   (set-append
    (tie-outcome-c2 (first oclst))
    clst)))


(begin-for-test
  (check-equal? (tie-outcome-list (list (tie "A" "B") (tie "B" "C")) empty)
                (list "A" "B") "must contain A and B")
  (check-equal? (tie-outcome-list (list (tie "A" "B") (tie "B" "C")) (list "E"))
                (list "A" "B" "E") "must contain A B and E"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comp-list : OutcomeList CompetitorList -> CompetitorList
;; GIVEN : an OutcomeList oclst and a CompetitorList clst
;; RETURNS : List of all Comptitors present in oclst.
;; EXAMPLES :
;;(comp-list (list (defeated "A" "B") (tie "B" "C")) empty)
;;=> (list "A" "B" "C")
;;(comp-list (list (defeated "A" "B") (tie "C" "D")) empty)
;;=> (list "C" "D" "A" "B" )
;; DESIGN STRATEGY : call simpler functions.

(define (comp-list oclst clst)
  (cond [(empty? oclst) clst]
        
        [(defeat-outcome? (first oclst))
         (comp-list (rest oclst) (defeat-outcome-list oclst clst))]
        
        [(tie-outcome? (first oclst))
         (comp-list (rest oclst) (tie-outcome-list oclst clst))]))



(begin-for-test
  (check-equal? (comp-list (list (defeated "A" "B") (tie "B" "C")) empty)
                (list "C" "A" "B") "must contain C A and B")
  (check-equal? (comp-list (list (defeated "A" "B") (tie "C" "D")) empty)
                (list "C" "D" "A" "B") "must contain C D A and B"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dedup-list : CompetitorList CompetitorList -> CompetitorList
;; GIVEN : a CompetitorList l and a CompetitorList seen
;; WHERE : the initial value provided at function call for seen is always empty.
;; RETURNS : a CompetitorList like l but with all the duplicates removed.
;; EXAMPLES:
;;(dedup-list (list "A" "B" "C" "A") empty)  => (list "A" "B" "C")
;;(dedup-list (list "A" "B") empty) => (list "A" "B")
;;(dedup-list empty empty) => empty
;;(dedup-list (list "C" "C") => empty)
;; DESIGN STRATEGY : use HOF ormap.

(define (dedup-list l seen)		
(cond		
  [(empty? l) empty]		
  [(not (ormap
         ;; Competitor -> Boolean
         ;; GIVEN : a Competitor x, from seen
         ;; RETURNS : true if the first Competitor from l matches x.  
         (lambda (x) (equal? (first l) x)) seen))		
   (cons (first l) (dedup-list (rest l) (cons (first l) seen)))]		
  [else (dedup-list (rest l) seen)]))


(begin-for-test
  (check-equal? (dedup-list (list "A" "B" "C" "A") empty) (list "A" "B" "C")
                "must not contain any duplicates")
  (check-equal? (dedup-list (list "A" "B") empty) (list "A" "B")
                "must not contain any duplicates")
  (check-equal? (dedup-list empty empty) empty
                "must not contain any duplicates")
  (check-equal? (dedup-list (list "C" "C") empty) (list "C")
                "must not contain any duplicates"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; members? : CompetitorList CompetitorList -> Boolean
;; GIVEN : two  CompetitorLists list1 list2
;; RETURNS : false if even one of the  Competitors in list1 do not belong
;;           to list2
;; EXAMPLE:
;;(members? (list "A" "B") (list "C" "D")) => false
;;(members? (list "A" "B") (list "B")) => false
;;(members? empty (list "A" "B")) => false
;;(members? (list "A" "B") (list "A" "B" "C")) => true
;;(members? (list "A" "K") (list "A" "B" "C")) => false
;; DESIGN STRATEGY : use HOF andmap

(define (members? list1 list2)
  (cond [(empty? list1) false]
        [else  (andmap
                ;; Competitor -> Boolean
                ;; GIVEN : a Competitor x from list1
                ;; RETURNS : true if x is a member of list2
                (lambda (x) (member? x list2))
                list1)]))

(begin-for-test
  (check-equal? (members? (list "A" "B") (list "C" "D")) false
                "must return false")
  (check-equal? (members? (list "A" "B") (list "B")) false
                "must return false")
  (check-equal? (members? empty (list "A" "B")) false
                "must return false")
  (check-equal? (members? (list "A" "B") (list "A" "B" "C")) true
                "must return true")
  (check-equal? (members? (list "A" "K") (list "A" "B" "C")) false
                "must return false"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directly-defeated : Competitor OutcomeList CompetitorList -> CompetitorList
;; GIVEN : a Competitor c1, OutcomeList oclst and a CompetitorList memlst
;; WHERE : memlist is the list of all Competitors mentioned in oclst.
;; RETURNS : the list of all Competitors that were directly defeated by c1
;; EXAMPLES:
;;(directly-defeated "A"
;;                   (list (defeated "A" "B") (tie "B" "C"))
;;                   (list "A" "B" "C")) => (list "B")
;;                                       
;;(directly-defeated "B" (list (defeated "A" "B") (tie "B" "C"))
;;                   (list "A" "B" "C")) => (list "C")
;;                                       
;;(directly-defeated "C" (list (defeated "A" "B") (tie "B" "C"))
;;                   (list "A" "B" "C")) => (list "B")
;;(directly-defeated "A"
;;                   (list (defeated "C" "B"))
;;                   (list "A" "B" "C")) => empty
;; DESIGN STRATEGY : use HOF foldr

(define (directly-defeated c1 oclst memlist)
  (foldr
   ;; Competitor CompetitorList -> CompetitorList
   ;; GIVEN  : a Competitor x and a CompetitorList r
   ;; RETURNS : a CompetitorList where x is appended to r if x was defeated
   ;;           by C1
   (lambda (x r) (append (if (defeated? c1 x oclst) (list x) empty) r))
   empty
   memlist))


(begin-for-test
  (check-equal? (directly-defeated "A"
                   (list (defeated "A" "B") (tie "B" "C"))
                   (list "A" "B" "C"))
                (list "B")
                "must return B")
  (check-equal? (directly-defeated "B" (list (defeated "A" "B") (tie "B" "C"))
                   (list "A" "B" "C"))
                (list "C")
                "must return C")
  (check-equal? (directly-defeated "C" (list (defeated "A" "B") (tie "B" "C"))
                   (list "A" "B" "C"))
                (list "B")
                "must return B")
  (check-equal? (directly-defeated "A"
                   (list (defeated "C" "B"))
                   (list "A" "B" "C"))
                empty
                "must return empty"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directly-defeated-lst : CompetitorList OutcomeList CompetitorList
;;                         -> CompetitorList
;; GIVEN : a CompetitorList clst, an OutcomeList oclst, and a CompetitorList
;;         memlst
;; WHERE : memlist is the list of all Competitors mentioned in oclst.
;; RETURNS : a list of all Competitors defeated by clst
;; EXAMPLES:
;;(directly-defeated-lst (list "A" "B")
;;                       (list (defeated "A" "B") (tie "B" "C"))
;;                       (list "A" "B" "C")) => (list "B" "C")
;;                                           
;;(directly-defeated-lst (list "B")
;;                       (list (defeated "A" "B") (tie "B" "C"))
;;                       (list "A" "B" "C")) => (list "C")
;; DESIGN STRATEGY : use HOF foldr.

(define (directly-defeated-lst clst oclst memlst)
  (foldr
   ;; Competitor CompetitorList -> CompetitorList
   ;; GIVEN : Competitor x, CompetitorList r
   ;; RETURNS : r appended to list of all Competitors defeated by x.
   (lambda (x r) (append (directly-defeated x oclst memlst) r))
   empty
   clst))

(begin-for-test
  (check-equal? (directly-defeated-lst (list "A" "B")
                                       (list (defeated "A" "B") (tie "B" "C"))
                                       (list "A" "B" "C"))
                (list "B" "C")
                "must return list with B and C")
  
  (check-equal? (directly-defeated-lst (list "B")
                                       (list (defeated "A" "B") (tie "B" "C"))
                                       (list "A" "B" "C"))
                (list "C")
                "must return list with C"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all-defeated : CompetitorList OutcomeList CompetitorList
;;                       CompetitorList -> CompetitorList
;; GIVEN : CompetitorList clst, OutcomeList oclst, CompetitorList memlst and
;;         CompetitorList out
;; WHERE : out contains all the Competitors defeated by all Competitors in clst
;;         till now.
;; AND  : memlst is the list of all competitors present in OutcomeList.
;; AND  : out is the list of all Competitors defeated by all the Competitors in
;;        clist till now.
;; RETURNS : list of all Competitors defeated by the list of all Competitors in
;;           clst.
;; EXAMPLE : 
;;(all-defeated (list "A")
;;              (list (defeated "A" "B") (tie "B" "C"))
;;              (list "A" "B" "C")
;;              empty) => (list "B" "C")
;;(all-defeated (list "A")
;;              (list (defeated "A" "B") (tie "B" "C") (defeated "C" "A"))
;;              (list "A" "B" "C")
;;              empty) => (list "A" "B" "C")
;;(all-defeated (list "A")
;;              (list (defeated "C" "D") (tie "E" "F"))
;;              (list "C" "D" "E" "F")
;;              empty) => empty
;; DESIGN STRATEGY : recur on out
;; HALTING MEASURE : (length (remove-duplicates (append out clst)))
;;                    - (length out)
;; WHERE : remove-duplictes is an operation which removes all duplicates from a
;;         list
 
(define (all-defeated clst oclst memlst out)
  (cond
    [(empty? (directly-defeated-lst clst oclst memlst)) out]
    [(members? (directly-defeated-lst clst oclst memlst) out) out]
    [else (all-defeated (directly-defeated-lst clst oclst memlst)
              oclst
              memlst
              (append (directly-defeated-lst clst oclst memlst) out))]))



(begin-for-test
  (check-equal? (all-defeated (list "A")
                              (list (defeated "A" "B") (tie "B" "C"))
                              (list "A" "B" "C")
                              empty)
                (list "C" "B")
                "must return list with B and C")
  (check-equal? (all-defeated (list "A")
                              (list (defeated "A" "B")
                                    (tie "B" "C")
                                    (defeated "C" "A"))
                              (list "A" "B" "C")
                              empty)
                (list "A" "B" "C" "B")
                "must return list with A B and C")
  (check-equal? (all-defeated (list "A")
                              (list (defeated "C" "D") (tie "E" "F"))
                              (list "C" "D" "E" "F")
                              empty)
                empty
                "must return an empty list"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outranks : Competitor OutcomeList -> CompetitorList
;; GIVEN: the name of a competitor and a list of outcomes
;; RETURNS: a list of the competitors outranked by the given
;;     competitor, in alphabetical order
;; NOTE: it is possible for a competitor to outrank itself
;; EXAMPLES:
;;     (outranks "A" (list (defeated "A" "B") (tie "B" "C")))
;;  => (list "B" "C")
;;
;;     (outranks "A" (list (defeated "A" "B") (tie "B" "C") (defeated "C" "A"))
;;  => (list "A" "B" "C")
;;
;;     (outranks "A" (list (tie "B" "C") (defeated "A" "B")))
;;  => (list "B" "C")
;;
;;     (outranks "B" (list (defeated "A" "B") (defeated "B" "A")))
;;  => (list "A" "B")
;;
;;     (outranks "C" (list (defeated "A" "B") (tie "B" "C")))
;;  => (list "B" "C")
;;
;;     (outranks "A" (list (defeated "C" "D") (tie "E" "F")))
;;  => empty
;; DESIGN STRATEGY : call simpler function, use HOF sort.


(define (outranks c oclst)
  (sort
   (dedup-list
    (all-defeated (list c) oclst (comp-list oclst empty) empty)
    empty)
   string<?))

(begin-for-test
  (check-equal? (outranks "A" (list (defeated "A" "B") (tie "B" "C")))
                (list "B" "C"))
  (check-equal? (outranks "A" (list (defeated "A" "B")
                                    (tie "B" "C")
                                    (defeated "C" "A")))
                (list "A" "B" "C"))
  (check-equal? (outranks "A" (list (tie "B" "C") (defeated "A" "B")))
                (list "B" "C"))
  (check-equal? (outranks "B" (list (defeated "A" "B") (defeated "B" "A")))
                (list "A" "B"))
  (check-equal? (outranks "C" (list (defeated "A" "B") (tie "B" "C")))
                (list "B" "C"))
  (check-equal? (outranks "A" (list (defeated "C" "D") (tie "E" "F")))
                empty))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outranked-by-list : Competitor OutcomeList CompetitorList -> CompetitorList
;; GIVEN : a Competitor c, a OutcomeList oclst, a CompetitorList memlst
;; WHERE :  memlst is the list of all competitors present in oclst.
;; RETURNS : CompetitorList which contain all the Competitors who outranked c.
;; EXAMPLES :
;;(outranked-by-list "A"
;;                   (list (defeated "A" "B") (tie "B" "C"))
;;                   (list "A" "B" "C")) => empty
;;                                       
;;(outranked-by-list "B"
;;              (list (defeated "A" "B") (defeated "B" "A"))
;;              (list "A" "B")) => (list "A" "B")
;;                              
;;(outranked-by-list  "C"
;;                    (list (defeated "A" "B") (tie "B" "C"))
;;                    (list "A" "B" "C")) => (list "A" "B")
;;                                        
;; DESIGN STRATEGY : use HOF foldr.

(define (outranked-by-list c oclst memlst)
  (foldr
   ;; Competitor CompetitorList -> CompetitorList
   ;; GIVEN : a Competitor x and a CompetitorList r
   ;; RETURNS : appends x to r if x outranks c.
   (lambda(x r) (append (if (member? c (outranks x oclst)) (list x) empty) r))
   empty
   memlst))

(begin-for-test
  (check-equal? (outranked-by-list "A"
                                   (list (defeated "A" "B") (tie "B" "C"))
                                   (list "A" "B" "C"))
                empty
                "must return empty list")
  (check-equal? (outranked-by-list "B"
                                   (list (defeated "A" "B") (defeated "B" "A"))
                                   (list "A" "B"))
                (list "A" "B")
                "must return list containing A and B")
  (check-equal? (outranked-by-list  "C"
                                    (list (defeated "A" "B") (tie "B" "C"))
                                    (list "A" "B" "C"))
                (list "A" "B" "C")
                "must return list containing A  B and C"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outranked-by : Competitor OutcomeList -> CompetitorList
;; GIVEN: the name of a competitor and a list of outcomes
;; RETURNS: a list of the competitors that outrank the given
;;     competitor, in alphabetical order
;; NOTE: it is possible for a competitor to outrank itself
;; EXAMPLES:
;;     (outranked-by "A" (list (defeated "A" "B") (tie "B" "C")))
;;  => (list)
;;
;;     (outranked-by "B" (list (defeated "A" "B") (defeated "B" "A")))
;;  => (list "A" "B")
;;
;;     (outranked-by "C" (list (defeated "A" "B") (tie "B" "C")))
;;  => (list "A" "B" "C")
;; DESIGN STRATEGY : call a simpler function.

(define (outranked-by c oclst)
  (sort (outranked-by-list c oclst (comp-list oclst empty)) string<?))


(begin-for-test
  (check-equal? (outranked-by "A" (list (defeated "A" "B") (tie "B" "C")))
                empty "must return an empty list")
  (check-equal? (outranked-by "B" (list (defeated "A" "B") (defeated "B" "A")))
                (list "A" "B") "must return A and B")
  (check-equal? (outranked-by "C" (list (defeated "A" "B") (tie "B" "C")))
                (list "A" "B" "C") "must return A B and C"))




