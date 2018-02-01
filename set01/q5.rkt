;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require rackunit)
(require "extras.rkt")
(check-location "01" "q5.rkt")

(provide years-to-test)

; numerical constants
(define EXTENSIVE-ADD (expt 2 128))
(define SEC-IN-MIN 60)
(define MIN-IN-HR 60)
(define HR-IN-DAY 24)
(define DAY-IN-YEAR 365)

;; DATA DEFINITIONS:
;; FLOPS is represented as a Positive Integer.
;; Number of years to complete exhaustive testing of double precision addition
;; is represented as a Positive Integer.

;; years-to-test : PosInt -> PosInt
;; GIVEN : the speed of a processor in FLOPS.
;; WHERE : exhaustive testing of double precision addition involves 2^128
;; additions (floating operations)
;; RETURNS : the number of years to complete  exhaustive testing of double
;; precision addition.
;; DESIGN STRATEGY : use the information provided with question.

;; EXAMPLE :
;; (years-to-test (expt 2 100)) => 8.51 ~ 9 (approximately)
;; (years-to-test (expt 2 50)) => 9583696565945500.18 ~ 9583696565945500
;; (approximately)

(define (years-to-test flops)
  (/ (/ (/ (/ (/ EXTENSIVE-ADD flops) SEC-IN-MIN) MIN-IN-HR) HR-IN-DAY) DAY-IN-YEAR))


(begin-for-test
  (check-equal? (round (years-to-test (expt 2 100))) 9)
  (check-equal? (round (years-to-test (expt 2 50))) 9583696565945500))
