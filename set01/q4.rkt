;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require rackunit)
(require "extras.rkt")
(check-location "01" "q4.rkt")

(provide flopy)

;; Numberical Constants
(define SEC-IN-MIN 60)
(define MIN-IN-HR 60)
(define HR-IN-DAY 24)
(define DAY-IN-YEAR 365)

;; DATA DEFINITIONS:
;; flops is represented as a Positive Integer.
;; the Number of floating point operations in a year is represented as a
;; Positive Integer.

;; flopy : PosInt -> PosInt
;; GIVEN : the speed of a microprocessor in flops
;; RETURNS : the number of floating point operations in a year.
;; DESIGN STRATEGY : use the information provided with question.

;; EXAMPLES:
;; (floppy 500)  => 15768000000
;; (floppy 1000) => 31536000000
;; (floppy 10)   => 315360000

(define (flopy flops)
  (* (* (* (* flops SEC-IN-MIN) MIN-IN-HR) HR-IN-DAY) DAY-IN-YEAR))

(begin-for-test
  (check-equal? (flopy 500) 15768000000)
  (check-equal? (flopy 1000) 31536000000)
  (check-equal? (flopy 10) 315360000))
