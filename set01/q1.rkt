;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require rackunit)
(require "extras.rkt")
(check-location "01" "q1.rkt")

(provide pyramid-volume)

;;DATA DEFINITIONS:
;; height of a pyramid is represented as a Real Positive Number.
;; side of the square base of the pyramid is represented as a Real Positive
;; Number.
;; Volume of the pyramid is represented as a Real Positive Number.

;; pyramid-volume : RealPos RealPos -> RealPos
;; GIVEN : the height h and one side of its square bottom x of a pyramid
;; RETURNS : the volume of the pyramid
;; DESIGN STRATEGY: transcribe formula
;; (/ (* (expt x 2) h) 3)

;; EXAMPLE :
;; (pyramid-volume 6 5) => 50
;; (pyramid-volume 6 6) => 72
;; (pyramid-volume 9 10) => 300

(define (pyramid-volume h x)
          (/ (* (expt x 2) h) 3))

(begin-for-test
  (check-eq? (pyramid-volume 6 5) 50)
  (check-eq? (pyramid-volume 6 6) 72)
  (check-eq? (pyramid-volume 9 10) 300))


