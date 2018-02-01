;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")


(check-location "02" "q2.rkt")


(provide
 initial-state
 next-state
 is-red?
 is-green?)

;; Color is one of the following :
;; -- "red"
;; -- "green"
;; -- "blank"
;; INTERP : colours that a chinese traffic signal has.

;; OBSERVER TEMPLATE :
;; (define (color-fn s)
;;   (cond[(string=? s "red") ...]
;;        [(string=? s "green") ...]
;;        [(string=? s "blank") ...]))




;; ChineseTrafficSignal is defined as a struct (color, time)
;; with the following fields :
;; color : Color : defines the color of the traffic signal
;; timer  : PosInt : defines a time in seconds during which the color
;;         doesnot change.
;; init-timer : PosInt : define the initial value to which timer was set to
;;         in seconds.
;; blink-state : PosInt : keeps count of the green/blank blinks.

;; IMPLEMENTATION :
(define-struct chinese-traffic-signal (color timer init-timer blink-state))

;; CONSTRUCTOR TEMPLATE:
;; (make-chinese-traffic-signal Color PosInt)

;; OBSERVER TEMPLATE:
(define (chinese-traffic-signal-fn c)
  (... (chinese-traffic-signal-color c)
  (chinese-traffic-signal-timer c)
  (chinese-traffic-signal-init-timer c)
  (chinese-traffic-signal-blink-state c)))
 



(define INITIAL-STATE (make-chinese-traffic-signal "red" 5 5 3))
(define SINGLE-DEC (make-chinese-traffic-signal "red" 4 5 3))
(define RESET-GREEN (make-chinese-traffic-signal "green" 2 5 3))
(define RESET-GREEN-DEC (make-chinese-traffic-signal "green" 1 5 3))
(define RESET-BLANK (make-chinese-traffic-signal "blank" 2 5 3))
(define BLANK-BLINK-STATE (make-chinese-traffic-signal "blank" 1 5 2))
(define GREEN-BLINK-STATE (make-chinese-traffic-signal "green" 1 5 2))
(define BLINK-STATE-RESET (make-chinese-traffic-signal "blank" 1 5 0))





;; initial-state : PosInt -> ChineseTrafficSignal
;; GIVEN: an integer n greater than 3
;; RETURNS: a representation of a Chinese traffic signal
;;     at the beginning of its red state, which will last
;;     for n seconds
;; EXAMPLE:
;;     (is-red? (initial-state 4))  =>  true
;;     (initial-state 5) => (make-chinese-traffic-signal "red" 5 5 3)
;; DESIGN STRATEGY : use the observer template for ChineseTrafficSignal.

(define (initial-state num)
  (make-chinese-traffic-signal "red" num num 3))


(begin-for-test
  (check-equal? (initial-state 5) INITIAL-STATE)
  (check-equal? (is-red? (initial-state 4)) true))




;; timer-decrement ChineseTrafficSignal -> ChineseTrafficSignal
;; GIVEN: a representation of a traffic signal whose timer > 1
;; RETURNS : a representation of a traffic signal with its timer
;;          decremented by 1.
;; EXAMPLE:
;;    (timer-decrement (make-chinese-traffic-signal "red" 5 5 3)) =>
;;        (make-chinese-traffic-signal "red" 4 5 3)
;;    (timer-decrement (make-chinese-traffic-signal "green" 2 5 3)) =>
;;        (make-chinese-traffic-signal "green" 1 5 3)
;; DESIGN STRATEGY: use the observer template for ChineseTrafficSignal.

(define (timer-decrement c)
  (make-chinese-traffic-signal (chinese-traffic-signal-color c)
                               (- (chinese-traffic-signal-timer c) 1)
                               (chinese-traffic-signal-init-timer c)
                               (chinese-traffic-signal-blink-state c)))

(begin-for-test
 (check-equal? (timer-decrement INITIAL-STATE) SINGLE-DEC)
 (check-equal? (timer-decrement RESET-GREEN) RESET-GREEN-DEC))




;; timer-reset-green ChineseTrafficSignal -> ChineseTrafficSignal
;; GIVEN : a representation of a traffic signal after completing  red state.
;; RETURNS : a representation of a traffic signal with green color state and
;;          timer reset to 3 seconds less than initial timer
;; EXAMPLE:
;;    (timer-reset-green (make-chinese-traffic-signal "red" 5 5 3)) =>
;;    (make-chinese-traffic-signal "green" 2 5 3)
;; DESIGN STRATEGY : use the constructor template for ChineseTrafficSignal.

(define (timer-reset-green c)
  (make-chinese-traffic-signal "green"
                               (- (chinese-traffic-signal-init-timer c) 3)
                               (chinese-traffic-signal-init-timer c)
                               (chinese-traffic-signal-blink-state c)))

(begin-for-test
  (check-equal? (timer-reset-green INITIAL-STATE)
                RESET-GREEN))




;; blink-color Color -> Color
;; GIVEN : a color during the blinking state of the traffic signal.
;; RETURNS : "green" if "blank" else "blank"
;; EXAMPLES :
;;     (blink-color (make-chinese-traffic-signal "green" 2 5 3))) =>
;;        (make-chinese-traffic-signal "blank" 2 5 3))
;;     (blink-color (make-chinese-traffic-signal "blank" 2 5 3))) =>
;;        (make-chinese-traffic-signal "green" 2 5 3))
;; DESIGN STRATEGY : use the observer template for Color.

(define (blink-color color)
  (cond[(string=? color "green") "blank"]
       [else "green"]))


(begin-for-test
  (check-equal? (blink-color "green") "blank")
  (check-equal? (blink-color "blank") "green"))




;; timer-reset-blank-green ChineseTrafficSignal -> ChineseTrafficSignal
;; GIVEN : a representation of a traffic signal after completing  green state.
;; RETURNS : a representation of a traffic signal with green color state and
;;          timer reset to 1 second and the blink-state decremented by 1.
;; EXAMPLES:
;;     (timer-reset-blank-green (make-chinese-traffic-signal "green" 2 5 3)) =>
;;        (make-chinese-traffic-signal "blank" 1 5 2)
;;     (timer-reset-blank-green (make-chinese-traffic-signal "blank" 2 5 3)) =>
;;        (make-chinese-traffic-signal "green" 1 5 2)
;; DESIGN STRATEGY : use the constructor template for ChineseTrafficSignal.

(define (timer-reset-blank-green c)
  (make-chinese-traffic-signal (blink-color (chinese-traffic-signal-color c))
                               1
                               (chinese-traffic-signal-init-timer c)
                               (- (chinese-traffic-signal-blink-state c) 1)))

(begin-for-test
  (check-equal? (timer-reset-blank-green RESET-GREEN) BLANK-BLINK-STATE)
  (check-equal? (timer-reset-blank-green RESET-BLANK) GREEN-BLINK-STATE))






;; blink-state-check ChineseTrafficSignal -> ChineseTrafficSignal
;; GIVEN : representation of a traffic signal during the blinking phase
;; RETURNS : if the blink state is 0 reset to initial state
;;           else continue blinking
;; EXAMPLES:
;;     (blink-state-check (make-chinese-traffic-signal "blank" 1 5 0)) =>
;;          (make-chinese-traffic-signal "red" 5 5 3)
;;     (timer-reset-blank-green (make-chinese-traffic-signal "blank" 2 5 3)) =>
;;          (make-chinese-traffic-signal "green" 1 5 2)    
;; DESIGN STRATEGY : combine simpler functions.

(define (blink-state-check c)
  (cond [(= (chinese-traffic-signal-blink-state c) 0)
         (initial-state (chinese-traffic-signal-init-timer c))]
        [else (timer-reset-blank-green c)]))

(begin-for-test
  (check-equal? (blink-state-check BLINK-STATE-RESET) INITIAL-STATE)
  (check-equal? (blink-state-check RESET-BLANK) GREEN-BLINK-STATE))






;; next-color : ChineseTrafficSignal -> ChineseTrafficSignal
;; GIVEN : a representation of traffic signal when the color has to change
;; RETURNS : the next color state of the traffic signal.
;; EXAMPLES:
;;     (next-color (make-chinese-traffic-signal "red" 5 5 3)) =>
;;          (make-chinese-traffic-signal "green" 2 5 3)
;;     (next-color (make-chinese-traffic-signal "blank" 1 5 0)) =>
;;           (make-chinese-traffic-signal "red" 5 5 3)
;;     (next-color (make-chinese-traffic-signal "blank" 2 5 3)) =>
;;           (make-chinese-traffic-signal "green" 1 5 2)
;; DESIGN STRATEGY : combine simpler functions.

(define (next-color c)
  (cond[(string=? (chinese-traffic-signal-color c ) "red")
        (timer-reset-green c) ]
       [else (blink-state-check c)]))

(begin-for-test
  (check-equal? (next-color INITIAL-STATE) RESET-GREEN)
  (check-equal? (next-color BLINK-STATE-RESET) INITIAL-STATE)
  (check-equal? (next-color RESET-BLANK) GREEN-BLINK-STATE))



;; next-state : ChineseTrafficSignal -> ChineseTrafficSignal
;; GIVEN: a representation of a traffic signal in some state
;; RETURNS: the state that traffic signal should have one
;;     second later
;; EXAMPLES:
;;    (next-state (next-state (next-state (next-state (next-state
;;       (make-chinese-traffic-signal "red" 5 5 3)))))) =>
;;       (make-chinese-traffic-signal "green" 2 5 3)
;;     (next-state (make-chinese-traffic-signal "blank" 1 5 0)) =>
;;        (make-chinese-traffic-signal "red" 5 5 3)
;; DESIGN STRATEGY : combine simpler functions.

(define (next-state c)
  (cond[(= (chinese-traffic-signal-timer c) 1) (next-color c)]
       [else (timer-decrement c)]))

(begin-for-test
  (check-equal? (next-state (next-state (next-state (next-state (next-state
       INITIAL-STATE))))) RESET-GREEN)
  (check-equal? (next-state BLINK-STATE-RESET) INITIAL-STATE))




;; is-red? : ChineseTrafficSignal -> Boolean
;; GIVEN: a representation of a traffic signal in some state
;; RETURNS: true if and only if the signal is red
;; EXAMPLES:
;;     (is-red? (next-state (initial-state 4)))  =>  true
;;     (is-red?
;;      (next-state
;;       (next-state
;;        (next-state (initial-state 4)))))  =>  true
;;     (is-red?
;;      (next-state
;;       (next-state
;;        (next-state
;;         (next-state (initial-state 4))))))  =>  false
;;     (is-red?
;;      (next-state
;;       (next-state
;;        (next-state
;;         (next-state
;;          (next-state (initial-state 4)))))))  =>  false
;; DESIGN STRATEGY : use oberver template for ChineseTrafficSignal

(define (is-red? c)
  (string=? (chinese-traffic-signal-color c) "red"))




(begin-for-test
  (check-equal? (is-red? (next-state (initial-state 4))) true)
  (check-equal? (is-red?
      (next-state
        (next-state
         (next-state (initial-state 4))))) true)
  (check-equal?  (is-red?
      (next-state
       (next-state
        (next-state
         (next-state (initial-state 4)))))) false)
  (check-equal? (is-red?
      (next-state
       (next-state
        (next-state
         (next-state
          (next-state (initial-state 4))))))) false))







;; is-green? : ChineseTrafficSignal -> Boolean
;; GIVEN: a representation of a traffic signal in some state
;; RETURNS: true if and only if the signal is green
;; EXAMPLES:
;;     (is-green?
;;      (next-state
;;       (next-state
;;        (next-state
;;         (next-state (initial-state 4))))))  =>  true
;;     (is-green?
;;      (next-state
;;       (next-state
;;        (next-state
;;         (next-state
;;          (next-state (initial-state 4)))))))  =>  false
;; DESIGN STRATEGY : use oberver template for ChineseTrafficSignal.

(define (is-green? c)
  (string=? (chinese-traffic-signal-color c) "green"))


(begin-for-test
  (check-equal?  (is-green?
      (next-state
       (next-state
        (next-state
         (next-state (initial-state 4)))))) true)
  (check-equal? (is-green?
      (next-state
       (next-state
        (next-state
         (next-state
          (next-state (initial-state 4))))))) false))






