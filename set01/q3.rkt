;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require rackunit)
(require "extras.rkt")
(check-location "01" "q3.rkt")

(provide kelvin-to-fahrenheit)

;; DATA DEFINITIONS:
;; Temperature in Kelvin is represented as a Real.
;; Temperature in Farenheit is represented as a Real.

;; kelvin-to-fahrenheit Real -> Real
;; GIVEN   : temperature in kelvin [0 - infinity]
;; RETURNS : the corresponding temperature in Farenheit
;; DESIGN STRATEGY : transcribe the formula
;; T(°F) = T(K) × 9/5 - 459.67

;; EXAMPLES :
;; (kelvin-to-fahrenheit 0)   = -459.67
;; (kelvin-to-fahrenheit 70)  = -333.67
;; (kelvin-to-fahrenheit 260) = 8.33
;; (kelvin-to-fahrenheit 500) = 440.33

(define (kelvin-to-fahrenheit kel)
  (- (* kel 9/5) 459.67))

(begin-for-test
  (check-equal? (kelvin-to-fahrenheit 0) -459.67)
  (check-equal? (kelvin-to-fahrenheit 70) -333.67)
  (check-equal? (kelvin-to-fahrenheit 260) 8.33)
  (check-equal? (kelvin-to-fahrenheit 500) 440.33))

