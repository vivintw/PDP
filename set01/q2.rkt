;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require rackunit)
(require "extras.rkt")
(check-location "01" "q2.rkt")

(provide furlongs-to-barleycorns)

;; constant for converting one Furlong to Barleycorns
(define FURLONG-TO-BARLEYCORNS 23760)

;; DATA DEFINITIONS:
;; length in furlongs is represented as a Real Number.
;; length in barleycorns in represented as a Real Number.

;; furlongs-to-barleycorns Real -> Real
;; GIVEN : a length in furlongs.
;; WHERE : 1 Furlong  =  10 * 4 * 16.5 * 12 * 3 barleycorns = 23760 barleycorns
;; RETURNS : equivalent length in barleycorns.
;; DESIGN STRATEGY :
;; use the defined conversion

;; EXAMPLES :
;; (furlongs-to-barleycorns 1)  => 23760
;; (furlongs-to-barleycorns 5)  => 118800
;; (furlongs-to-barleycorns 10) => 237600
;; (furlongs-to-barleycorns 13.5) => 320760
;; (furlongs-to-barleycorns -10) => -237600

(define (furlongs-to-barleycorns furlong)
  (* furlong FURLONG-TO-BARLEYCORNS))

(begin-for-test
  (check-eq? (furlongs-to-barleycorns 1) 23760)
  (check-eq? (furlongs-to-barleycorns 5) 118800)
  (check-eq? (furlongs-to-barleycorns 10) 237600)
  (check-eq? (furlongs-to-barleycorns 13.5) 320760)
  (check-eq? (furlongs-to-barleycorns -10) -237600))

