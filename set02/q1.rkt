;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")


(check-location "02" "q1.rkt")

(provide
        make-lexer
        lexer-token
        lexer-input
        initial-lexer
        lexer-stuck?
        lexer-shift
        lexer-reset)



;; Lexer is defined as a struct (token input)
;; with the following fields :
;; token : String (any String will do) defines the token of the Lexer.
;; input : String (any String will do) defines the input of the Lexer.

;; IMPLEMENTATION:
(define-struct lexer (tkn inpt))

;; CONSTRUCTOR TEMPLATE:
;; (make-lexer String String)

;; OBSERVER TEMPLATE:
(define (lexer-fn l)
  (... (lexer-tkn l)
       (lexer-inpt l)))



(define EMPTY-LEXER (make-lexer "" ""))
(define GENERIC-LEXER (make-lexer "abc" "1234"))
(define GENERIC-INPUT "1234")
(define GENERIC-TOKEN "abc")

(define LEXER-STUCK (make-lexer "abc" "+1234"))
(define LEXER-EMPTY-INPUT (make-lexer "abc" ""))
(define LEXER-EMPTY-TOKEN (make-lexer "" "1234"))
(define GENERIC-LEXER-SHIFT (make-lexer "abc1" "234"))


;; lexer-token : Lexer -> String
;; GIVEN: a Lexer
;; RETURNS: its token string
;; EXAMPLE:
;; (lexer-token (make-lexer "abc" "1234")) =>  "abc"
;; DESIGN STRATEGY : implement observer template.

(define (lexer-token l)
  (lexer-tkn l))


(begin-for-test
  (check-equal? (lexer-token GENERIC-LEXER) GENERIC-TOKEN))


;; lexer-input : Lexer -> String
;; GIVEN: a Lexer
;; RETURNS: its input string
;; EXAMPLE:
;;     (lexer-input (make-lexer "abc" "1234")) =>  "1234"
;; DESIGN STRATEGY : implement observer template.

(define (lexer-input l)
  (lexer-inpt l))



(begin-for-test
  (check-equal? (lexer-input GENERIC-LEXER) GENERIC-INPUT))

;; initial-lexer : String -> Lexer
;; GIVEN: an arbitrary string
;; RETURNS: a Lexer lex whose token string is empty
;;     and whose input string is the given string
;; EXAMPLE:
;; (initial-lexer "abc") => (make-lexer "" "abc")
;; DESIGN STRATEGY : implement constructor template.

(define(initial-lexer str)
  (make-lexer "" str))



(begin-for-test
  (check-equal? (initial-lexer "abc") (make-lexer "" "abc")))


;; input-empty? String -> Boolean
;; GIVEN : an input String (Any string will do)
;; RETURNS : true if the string is empty else false
;; EXAMPLE :
;; (input-empty? "") => true
;; (input-empty? "abc") => false
;; DESIGN STRATEGY : compare with empty string. 

(define (input-empty? str)
  (string=? "" str))


(begin-for-test
  (check-equal? (input-empty? "") true)
  (check-equal? (input-empty? "abc") false))


;; first-character String -> Character
;; GIVEN : an input String (Any String will do)
;; RETURNS : the first character of the string
;; EXAMPLE :
;; (first-character "abc") => #\a
;; (first-character "123") => #\1
;; DESIGN STRATEGY : combine simpler functions.

(define (first-character str)
  (string-ref str 0))


(begin-for-test
  (check-equal? (first-character "abc") #\a)
  (check-equal? (first-character "123") #\1))


;; letter-digit? String -> Boolean
;; GIVEN : A string (Any string will do).
;; RETURNS : true  if the first character is an alphabet or number else false
;; EXAMPLE :
;; (letter-digit? "abc") => true
;; (letter-digit? "123") => true
;; (letter-digit? "+123") => false
;; DESIGN STRATEGY : combine simpler functions.

(define (letter-digit? str)
  (or (char-alphabetic? (first-character str))
  (char-numeric? (first-character str))))

(begin-for-test
  (check-equal? (letter-digit? "abc") true)
  (check-equal? (letter-digit? "123") true)
  (check-equal? (letter-digit? "+123") false))


;; lexer-stuck? : Lexer -> Boolean
;; GIVEN: a Lexer
;; RETURNS: false if and only if the given Lexer's input string
;;     is non-empty and begins with an English letter or digit;
;;     otherwise returns true.
;; EXAMPLES:
;;     (lexer-stuck? (make-lexer "abc" "1234"))  =>  false
;;     (lexer-stuck? (make-lexer "abc" "+1234"))  =>  true
;;     (lexer-stuck? (make-lexer "abc" ""))  =>  true
;; DESIGN STRATEGY : combine simpler functions.


(define (lexer-stuck? l)
  (if (not (input-empty? (lexer-inpt l)))
      (not (letter-digit? (lexer-inpt l)))
      true))


(begin-for-test
  (check-equal? (lexer-stuck? GENERIC-LEXER) false)
  (check-equal? (lexer-stuck? LEXER-STUCK) true)
  (check-equal? (lexer-stuck? LEXER-EMPTY-INPUT) true))


;; input-first-letter-append String String -> String
;; GIVEN : two Strings s1 and s2 (Any strings will do).
;; RETURNS : another String where the first letter of s2 appended to s1
;; EXAMPLES :
;; (input-first-letter-append "abc" "123") => "abc1"
;; DESIGN STRATEGY : combine simpler functions.

(define (input-first-letter-append str1 str2)
  (string-append str1 (string-ith str2 0)))

(begin-for-test
  (check-equal? (input-first-letter-append "abc" "123") "abc1"))

;; all-but-first String -> String
;; GIVEN : A String (Any String will do).
;; RETURNS : A string without the first character.
;; EXAMPLES:
;; (all-but-first "abc") => "bc"
;; DESIGN STRATEGY : combine simpler functions.

(define (all-but-first str)
  (substring str 1 (string-length str)))

(begin-for-test
  (check-equal? (all-but-first "abc") "bc"))

;; move-letter Lexer  -> Lexer
;; GIVEN : a Lexer l
;; RETURNS : a lexer like l where the first letter from the  input is
;;           removed and appended to its token.
;; EXAMPLES:
;; (move-letter (make-lexer "abc" "1234"))
;;         =>  (make-lexer "abc1" "234")
;; DESIGN STRATEGY : combine simpler functions.

(define(move-letter l)
  (make-lexer (input-first-letter-append (lexer-tkn l) (lexer-inpt l))
              (all-but-first (lexer-inpt l))))


(begin-for-test
  (check-equal? (move-letter GENERIC-LEXER)
                GENERIC-LEXER-SHIFT ))



;; lexer-shift : Lexer -> Lexer
;; GIVEN: a Lexer
;; RETURNS:
;;   If the given Lexer is stuck, returns the given Lexer.
;;   If the given Lexer is not stuck, then the token string
;;       of the result consists of the characters of the given
;;       Lexer's token string followed by the first character
;;       of that Lexer's input string, and the input string
;;       of the result consists of all but the first character
;;       of the given Lexer's input string.
;; EXAMPLES:
;;     (lexer-shift (make-lexer "abc" ""))
;;         =>  (make-lexer "abc" "")
;;     (lexer-shift (make-lexer "abc" "+1234"))
;;         =>  (make-lexer "abc" "+1234")
;;     (lexer-shift (make-lexer "abc" "1234"))
;;         =>  (make-lexer "abc1" "234")
;; DESIGN STRATEGY : combine simpler functions.
  
(define (lexer-shift l)
  (cond[(lexer-stuck? l) l]
       [else (move-letter l)]))

(begin-for-test
  (check-equal?  (lexer-shift LEXER-EMPTY-INPUT) LEXER-EMPTY-INPUT)
  (check-equal?  (lexer-shift LEXER-STUCK) LEXER-STUCK)
  (check-equal? (lexer-shift GENERIC-LEXER) GENERIC-LEXER-SHIFT))



;; reset-input String -> String
;; GIVEN : A String (any string will do)
;; RETURNS : if the input string is empty returns empty
;;           else returns string without the first character.
;; EXAMPLES :
;; (reset-input "") => ""
;; (reset-input "abc") => "bc"
;; DESIGN STRATEGY : combine simpler functions.

(define (reset-input str)
  (cond[(input-empty? str) ""]
       [else (all-but-first str)]))

(begin-for-test
  (check-equal? (reset-input "") "")
  (check-equal? (reset-input "abc") "bc"))

;; lexer-reset : Lexer -> Lexer
;; GIVEN: a Lexer
;; RETURNS: a Lexer whose token string is empty and whose
;;     input string is empty if the given Lexer's input string
;;     is empty and otherwise consists of all but the first
;;     character of the given Lexer's input string.
;; EXAMPLES:
;;     (lexer-reset (make-lexer "abc" ""))
;;         =>  (make-lexer "" "")
;;     (lexer-reset (make-lexer "abc" "+1234"))
;;         =>  (make-lexer "" "1234")
;; DESIGN STRATEGY : use the constructor template.

  
(define (lexer-reset l)
  (make-lexer "" (reset-input (lexer-inpt l))))

(begin-for-test
  (check-equal? (lexer-reset LEXER-EMPTY-INPUT) EMPTY-LEXER)
  (check-equal? (lexer-reset LEXER-STUCK) LEXER-EMPTY-TOKEN))

