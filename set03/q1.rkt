;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Squash Practice Simulation :
;;
;;  A game of squash is simulated where the racket can be controlled by the
;;  user using the arrow keys.
;;
;;  the ball responds to being hit be the racket/ walls 
;;  press the space bar to toggle between the ready-to-serve and rally state.
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require rackunit)

(provide simulation
         initial-world
         world-ready-to-serve?
         world-after-tick
         world-after-key-event
         world-ball
         world-racket
         ball-x
         ball-y
         racket-x
         racket-y
         ball-vx
         ball-vy
         racket-vx
         racket-vy)

(check-location "03" "q1.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; numeric constants : 
(define COURT-HEIGHT 649)
(define COURT-WIDTH 425)
(define RACKET-WIDTH 47)
(define RACKET-HEIGHT 7)
(define BALL-RADIUS 3)
(define READY-TO-SERVE-X 330)
(define READY-TO-SERVE-Y 384)
(define RACKET-SPEED 1)
(define HALF-RACKET-WIDTH (floor (/ RACKET-WIDTH 2)))
(define HALF-RACKET-HEIGHT (floor (/ RACKET-HEIGHT 2)))
(define INITIAL-VELOCITY-X 0)
(define INITIAL-VELOCITY-Y 0)
(define RALLY-BALL-VELOCITY-X 3)
(define RALLY-BALL-VELOCITY-Y -9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; image constants :
(define RACKET (rectangle RACKET-WIDTH RACKET-HEIGHT "solid" "green"))
(define BALL (circle BALL-RADIUS "solid" "black"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Racket is implemented as a structure
;;     (make-racket x y vx vy)
;; where :
;; x  : Int ,the value of the x co-ordinate of racket in pixels.
;; y  : Int ,the value of the y co-ordinate of racket in pixels.
;; vx : Int ,the velocity of the racket along x axis in pixels per tick.
;; vy : Int ,the velocity of the racket along the y axis in pixels per tick.

;; IMPLEMENTATION
(define-struct racket (x y vx vy))

;; CONSTRUCTOR TEMPLATE
;; (make-racket Int Int Int Int)

;; OBSERVER TEMPLATE
;; racket-fn : Racket -> ??
(define (racket-fn r)
  (... (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ball is implemented as a structure
;;     (make-ball x y vx vy)
;; where :
;; x  : Int, the value of the x co-ordinate of ball in pixels.
;; y  : Int, the value of the y co-ordinate of ball in pixels.
;; vx : Int, the velocity of the ball along x axis in pixels per tick.
;; vy : Int, the velocity of the ball along the y axis in pixels per tick.

;; IMPLEMENTATION
(define-struct ball (x y vx vy))

;; CONSTRUCTOR TEMPLATE:
;; (make-ball Int Int Int Int)


;; OBSERVER TEMPLATE:
;; ball-fn Ball -> ??
(define (ball-fn b)
  (... (ball-x b)
       (ball-y b)
       (ball-vx b)
       (ball-vy b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CourtColor is one of the following:
;; -- "white"
;; -- "yellow"
;; INTERP : the color that the court changes to based on the state of the
;;          simulation

(define WHITE  "white")
(define YELLOW "yellow")

;; OBSERVER TEMPLATE:
;; (define (court-color c)
;;   (cond
;;     [(string=? c "white") ...]
;;     [(string=? c "yellow") ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; World is implemented as a structure
;;    (make-world ball
;;                racket
;;                court-color
;;                speed
;;                paused?
;;                paused-counter
;;                ready-to-serve?)
;; where :
;; ball            : Ball,       is a Ball object currently in the World.
;; racket          : Racket,     is a Racket object currently in the World.
;; court-color     : CourtColor, is the current color of the court.
;; speed           : PosReal,    the speed at which the simulation is run.
;;                               in ticks per second.
;; paused?         : Boolean,    true if the world is paused else false.
;; paused-counter  : PosInt,     the count in ticks to the end of paused state.
;; ready-to-serve? : Boolean,    decides if the world is in the ready to serve
;;                               state.

;; IMPLEMENTATION
(define-struct world
  (ball racket court-color speed paused? paused-counter ready-to-serve?))

;; CONSTRUCTOR TEMPLATE
;; (make-world Ball Racket CourtColor PosReal Boolean PosInt Boolean)

;; OBSERVER TEMPLATE
;; world-fn : WORLD -> ??
(define (world-fn c)
  (... (world-ball c)
       (world-racket c)
       (world-court-color c)
       (world-speed c)
       (world-paused? c)
       (world-paused-counter c)
       (world-ready-to-serve? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants :

(define INITIAL-BALL
  (make-ball READY-TO-SERVE-X
             READY-TO-SERVE-Y
             INITIAL-VELOCITY-X
             INITIAL-VELOCITY-Y))

(define INITIAL-RACKET
  (make-racket READY-TO-SERVE-X
               READY-TO-SERVE-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define BALL-COLLISION-SIDE-RIGHT-WALL
  (make-ball (+ COURT-WIDTH 1)
             1
             RALLY-BALL-VELOCITY-X
             RALLY-BALL-VELOCITY-Y))

(define BALL-AFTER-COLLISION-SIDE-RIGHT-WALL
  (make-ball (- COURT-WIDTH 1)
             1
             (- RALLY-BALL-VELOCITY-X)
             RALLY-BALL-VELOCITY-Y))

(define BALL-COLLISION-SIDE-LEFT-WALL
  (make-ball -1
             1
             RALLY-BALL-VELOCITY-X
             RALLY-BALL-VELOCITY-Y))

(define BALL-AFTER-COLLISION-SIDE-LEFT-WALL
  (make-ball 1
             1
             (- RALLY-BALL-VELOCITY-X)
             RALLY-BALL-VELOCITY-Y))

(define BALL-COLLISION-TOP-WALL
  (make-ball 1
             -1
             RALLY-BALL-VELOCITY-X
             RALLY-BALL-VELOCITY-Y))


(define BALL-AFTER-COLLISION-TOP-WALL
  (make-ball 1
             1
             RALLY-BALL-VELOCITY-X
             (- RALLY-BALL-VELOCITY-Y)))

(define BALL-COLLISION-BOTTOM-WALL
  (make-ball 1
             (+ COURT-HEIGHT 1)
             RALLY-BALL-VELOCITY-X
             RALLY-BALL-VELOCITY-Y))

(define BALL-AFTER-COLLISION-BOTTOM-WALL
  (make-ball 1 646 0 0))


(define RACKET-COLLISION-SIDE-RIGHT-WALL
  (make-racket COURT-WIDTH
               1
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y))

(define RACKET-AFTER-COLLISION-SIDE-RIGHT-WALL
  (make-racket 402
               1
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y))

(define RACKET-COLLISION-SIDE-LEFT-WALL
  (make-racket 0
               1
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y))

(define RACKET-AFTER-COLLISION-SIDE-LEFT-WALL
  (make-racket 23
               1
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y))

(define RACKET-COLLISION-TOP-WALL
  (make-racket 1
               0
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y))

(define RACKET-AFTER-COLLISION-TOP-WALL
  (make-racket 1
               3.5
               0
               0))

(define RACKET-COLLISION-BOTTOM-WALL
  (make-racket 1
               COURT-HEIGHT
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y))

(define RACKET-AFTER-COLLISION-BOTTOM-WALL
  (make-racket 1
               646
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y))


(define BALL-ABOVE-RACKET (make-ball 27
                                     30
                                     RALLY-BALL-VELOCITY-X
                                     (- RALLY-BALL-VELOCITY-Y)))

(define BALL-BELOW-RACKET0 (make-ball 30
                                      39
                                      RALLY-BALL-VELOCITY-X
                                      (- RALLY-BALL-VELOCITY-Y)))

(define BALL-BELOW-RACKET1 (make-ball 32
                                      30
                                      RALLY-BALL-VELOCITY-X
                                      (- RALLY-BALL-VELOCITY-Y)))

(define BALL-BELOW-RACKET2 (make-ball 25
                                      35
                                      RALLY-BALL-VELOCITY-X
                                      (- RALLY-BALL-VELOCITY-Y)))

(define BALL-BELOW-RACKET3 (make-ball 32
                                      35
                                      RALLY-BALL-VELOCITY-X
                                      (- RALLY-BALL-VELOCITY-Y)))

(define RACKET-COLLISION-BALL (make-racket 30 30 0 0))

(define BALL-AFTER-COLLISION-RACKET1 (make-ball 32
                                         30
                                         RALLY-BALL-VELOCITY-X
                                         RALLY-BALL-VELOCITY-Y))

(define BALL-AFTER-COLLISION-RACKET2 (make-ball 25
                                         30
                                         RALLY-BALL-VELOCITY-X
                                         RALLY-BALL-VELOCITY-Y))

(define PAUSED-WORLD
  (make-world INITIAL-BALL INITIAL-RACKET WHITE 1 true 1 true))
(define PAUSED-WORLD1
  (make-world INITIAL-BALL INITIAL-RACKET WHITE 1 true 0 true))
(define INITIAL-WORLD (make-world INITIAL-BALL
                                  INITIAL-RACKET
                                  WHITE
                                  1
                                  false
                                  3
                                  true))
(define RALLY-WORLD  (make-world
                      (make-ball READY-TO-SERVE-X
                                 READY-TO-SERVE-Y
                                 RALLY-BALL-VELOCITY-X
                                 RALLY-BALL-VELOCITY-Y)
                      INITIAL-RACKET
                      WHITE
                      1
                      false
                      3
                      false))

(define EXCEPTION-WORLD (make-world BALL-COLLISION-BOTTOM-WALL
                                    RACKET-COLLISION-TOP-WALL
                                    WHITE
                                    1
                                    false
                                    3
                                    true))

(define EXCEPTION-WORLD-OUTPUT (make-world (make-ball 1 650 3 -9)
                                           (make-racket 1 0 3 -9)
                                           YELLOW
                                           1
                                           true
                                           3
                                           true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simulation : PosReal -> World
;; GIVEN: the speed of the simulation, in seconds per tick
;;     (so larger numbers run slower)
;; EFFECT: runs the simulation, starting with the initial world
;; RETURNS: the final state of the world
;; EXAMPLES:
;;     (simulation 1) runs in super slow motion
;;     (simulation 1/24) runs at a more realistic speed
;; DESIGN STRATEGY : call big-bang with the required functions as inputs.

(define (simulation speed)
  (big-bang (initial-world speed)
            (on-tick world-after-tick speed)
            (on-key world-after-key-event)
            (to-draw draw-world)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : PosReal -> World
;; GIVEN: the speed of the simulation, in seconds per tick
;;     (so larger numbers run slower)
;; RETURNS: the ready-to-serve state of the world
;; WHERE : the number of ticks per 3 seconds, required for the paused state
;;         can be calculated as :
;;         number of ticks in 3 seconds = ( 1 / n) * 3, where :
;;         n = speed of simulation in ticks per second. 
;; EXAMPLE:
;; (initial-world 1)
;;    => (make-world INITIAL-BALL INITIAL-RACKET WHITE 1 false 3 true)
;; DESIGN STRATEGY : use the constructor template of World.

(define (initial-world speed)
 (make-world INITIAL-BALL
             INITIAL-RACKET
             WHITE
             speed
             false
             (* (/ 1 speed) 3)
             true))

(begin-for-test
  (check-equal? (initial-world 1) (make-world INITIAL-BALL
                                              INITIAL-RACKET
                                              WHITE
                                              1
                                              false
                                              3
                                              true)
                "when speed is set as 1 sec, number of ticks for
 pause must be 3"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; distance-after-tick : Int Int -> Int
;; GIVEN : the current x or y co-ordinate where the object is located in
;;         the graphical co-ordinate system and the velocity of the object in
;;         pixels per tick.
;; RETURNS : the next x or y co-ordinate position at the next tick in pixels.
;; EXAMPLES: (distance-after-tick 5 10) => 15
;; DESIGN STRATEGY : Transcribe formula.

(define (distance-after-tick distance velocity)
    (+ distance velocity))

(begin-for-test
  (check-equal? (distance-after-tick 5 10) 15)
  "distance must be sum of 5 and 10 = 15")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-collides-with-side-wall : Ball -> Ball
;; GIVEN : A Ball that may collide with any of the side walls.
;; RETURNS : A Ball after the collision with the wall or the same ball if the
;;           ball doesnot collide.
;; EXAMPLES:
;;(ball-collides-with-side-wall BALL-COLLISION-SIDE-RIGHT-WALL)
;;    => BALL-AFTER-COLLISION-SIDE-RIGHT-WALL
;;
;;(ball-collides-with-side-wall BALL-COLLISION-SIDE-LEFT-WALL)
;;    => BALL-AFTER-COLLISION-SIDE-LEFT-WALL
;;
;;(ball-collides-with-side-wall INITIAL-BALL) => INITIAL-BALL
;; DESIGN STRATEGY : conditions on the x co-ordinate of the ball.

(define (ball-collides-with-side-wall b)
  (cond
    [(< (ball-x b) 0)
     (make-ball (- (ball-x b))
                (ball-y b)
                (- (ball-vx b))
                (ball-vy b))]
    
    [(> (ball-x b) COURT-WIDTH)
     (make-ball (- COURT-WIDTH (-  (ball-x b) COURT-WIDTH))
                (ball-y b)
                (- (ball-vx b))
                (ball-vy b))]
    [else b]))

(begin-for-test
  (check-equal?  (ball-collides-with-side-wall BALL-COLLISION-SIDE-RIGHT-WALL)
                 BALL-AFTER-COLLISION-SIDE-RIGHT-WALL
                 "collision condition with right wall failed")
  (check-equal? (ball-collides-with-side-wall BALL-COLLISION-SIDE-LEFT-WALL)
                 BALL-AFTER-COLLISION-SIDE-LEFT-WALL
                 "collision condition with left wall failed")
  (check-equal? (ball-collides-with-side-wall INITIAL-BALL)
                INITIAL-BALL
                "ball with no collision conditions must be unchanged."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-collides-with-bottom-wall? : Ball -> Boolean
;; GIVEN : a Ball which might collide with bottom wall
;; RETURNS : true if the ball has collided with the bottom wall
;;           else false.
;; EXAMPLE :
;; (ball-collides-with-bottom-wall? BALL-COLLISION-BOTTOM-WALL) => true
;; (ball-collides-with-bottom-wall? INITIAL-BALL) => false
;; DESIGN STRATEGY : Transcribe formula.

(define (ball-collides-with-bottom-wall? b)
  (>= (ball-y b) (- COURT-HEIGHT BALL-RADIUS)))


(begin-for-test
  (check-equal? (ball-collides-with-bottom-wall? BALL-COLLISION-BOTTOM-WALL)
                true "ball with collision conditions must return true")
  (check-equal? (ball-collides-with-bottom-wall? INITIAL-BALL)
                false "ball without collision condition must return false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-collides-with-top-wall : Ball -> Ball
;; GIVEN : a ball which may collide with the top wall.
;; RETURNS : the ball after the collision if the collision takes place.
;;           if no collision returns the same ball.
;; EXAMPLES :
;;(ball-collides-with-top-wall BALL-COLLISION-TOP-WALL)
;;    => BALL-AFTER-COLLISION-TOP-WALL
;;
;;(ball-collides-with-top-wall INITIAL-BALL)
;;    => INITIAL-BALL
;;
;;(ball-collides-with-top-wall BALL-COLLISION-BOTTOM-WALL)
;;    => BALL-AFTER-COLLISION-BOTTOM-WALL
;;
;; DESIGN STRATEGY : conditions on y co-ordinate of the ball.

(define (ball-collides-with-top-wall b)
  (cond
    [(< (ball-y b) 0)
     (make-ball (ball-x b)
                (- (ball-y b))
                (ball-vx b)
                (- (ball-vy b)))]
    
    [(>= (ball-y b) COURT-HEIGHT)
     (make-ball (ball-x b)
                (- COURT-HEIGHT BALL-RADIUS)
                0
                0)]
    
    [else b]))


(begin-for-test
  (check-equal? (ball-collides-with-top-wall BALL-COLLISION-TOP-WALL)
                BALL-AFTER-COLLISION-TOP-WALL
                "ball after top wall collision condition failed")
  (check-equal? (ball-collides-with-top-wall INITIAL-BALL)
                INITIAL-BALL
                "ball that doesnot have collision conditions must not be
 affected")
  (check-equal? (ball-collides-with-top-wall BALL-COLLISION-BOTTOM-WALL)
                BALL-AFTER-COLLISION-BOTTOM-WALL
                "ball after bottom wall collision condition failed."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-after-racket-collision : Racket Ball Ball -> Ball
;; GIVEN : the current Racket , tentative ball and original ball.
;;         (the ball has to collide with the racket.)
;; RETURNS : the Ball after it has collided with the racket.
;; EXAMPLES :
;;(ball-after-racket-collision RACKET-COLLISION-BALL
;;                              BALL-BELOW-RACKET1
;;                              BALL-ABOVE-RACKET)
;;    => BALL-AFTER-COLLISION-RACKET1
;;
;; (ball-after-racket-collision RACKET-COLLISION-BALL
;;                              BALL-BELOW-RACKET2
;;                              BALL-ABOVE-RACKET)
;;    => BALL-AFTER-COLLISION-RACKET2
;; (ball-after-racket-collision RACKET-COLLISION-BALL
;;                              BALL-BELOW-RACKET3
;;                              BALL-ABOVE-RACKET)
;;    => BALL-AFTER-COLLISION-RACKET1
;;
;; DESIGN STRATEGY : use the constructor template for ball.

(define (ball-after-racket-collision r b old-b)
  (make-ball (ball-x b)
                 (ball-y old-b)
                 (ball-vx old-b)
                 (- (racket-vy r) (ball-vy old-b))))



(begin-for-test
  (check-equal? (ball-after-racket-collision RACKET-COLLISION-BALL
                              BALL-BELOW-RACKET1
                              BALL-ABOVE-RACKET)
                BALL-AFTER-COLLISION-RACKET1 "invalid ball state returned.")
  
  (check-equal? (ball-after-racket-collision RACKET-COLLISION-BALL
                              BALL-BELOW-RACKET2
                              BALL-ABOVE-RACKET)
                BALL-AFTER-COLLISION-RACKET2 "invalid ball state returned.")
  
  (check-equal? (ball-after-racket-collision RACKET-COLLISION-BALL
                              BALL-BELOW-RACKET3
                              BALL-ABOVE-RACKET)
                BALL-AFTER-COLLISION-RACKET1 "invalid ball state returned."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-collides-with-racket? : Racket Ball Ball -> Boolean
;; GIVEN : the current Racket at the tick, the tentative ball
;;         and the original ball.
;; RETURNS : true if the ball collides with the racket else false.
;; EXAMPLES :
;; (ball-collides-with-racket? RACKET-COLLISION-BALL
;;                             BALL-BELOW-RACKET1
;;                             BALL-ABOVE-RACKET) => true
;;
;; (ball-collides-with-racket? RACKET-COLLISION-BALL
;;                             BALL-BELOW-RACKET2
;;                             BALL-ABOVE-RACKET) => true
;;
;; (ball-collides-with-racket? RACKET-COLLISION-BALL
;;                             BALL-BELOW-RACKET3
;;                             BALL-ABOVE-RACKET) => true
;;
;; (ball-collides-with-racket? RACKET-COLLISION-BALL
;;                             INITIAL-BALL
;;                             INITIAL-BALL) => false
;; DESIGN STRATEGY : Transcribe formula.

(define (ball-collides-with-racket? r b b-old)
  (and
       (<= (- (racket-x r) HALF-RACKET-WIDTH) (ball-x b))
       (>= (+ (racket-x r) HALF-RACKET-WIDTH) (ball-x b))
       
       (<= (- (racket-x r) HALF-RACKET-WIDTH) (ball-x b-old))
       (>= (+ (racket-x r) HALF-RACKET-WIDTH) (ball-x b-old))
     
       (<=  (ball-y b-old) (racket-y r))
       (>=   (ball-y b) (racket-y r))))


(begin-for-test
  (check-equal? (ball-collides-with-racket? RACKET-COLLISION-BALL
                              BALL-BELOW-RACKET1
                              BALL-ABOVE-RACKET) true "collision condition
 should return true.")
  (check-equal? (ball-collides-with-racket? RACKET-COLLISION-BALL
                              BALL-BELOW-RACKET2
                              BALL-ABOVE-RACKET) true "collision condition
 should return true.")
  (check-equal? (ball-collides-with-racket? RACKET-COLLISION-BALL
                              BALL-BELOW-RACKET3
                              BALL-ABOVE-RACKET) true "collision condition
 should return true.")
  (check-equal? (ball-collides-with-racket? RACKET-COLLISION-BALL
                              INITIAL-BALL
                              INITIAL-BALL) false "not collision condition
 should return false."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tentative-ball : Ball -> Ball
;; GIVEN : a ball
;; RETURNS : the tentative next position of the ball
;; WHERE : the position of the tentative ball is (vx + x, vy + y)
;; here x  : Int, the graphical x co-ordinate of the ball in pixels.
;;      y  : Int, the graphical y co-ordinate of the ball in pixels.
;;      xv : Int, the velocity component of the ball in the x direction
;;           in pixels per tick.
;;      vy : Int, the velocity component of the ball in the y direction
;;           in pixels per tick.
;; EXAMPLES :
;; (tentative-ball BALL-ABOVE-RACKET) => BALL-BELOW-RACKET0
;; DESIGN STRATEGY : use the constructor template of Ball.

(define (tentative-ball b)
   (make-ball (distance-after-tick (ball-x b) (ball-vx b))
             (distance-after-tick (ball-y b) (ball-vy b))
             (ball-vx b)
             (ball-vy b)))

(begin-for-test
  (check-equal? (tentative-ball BALL-ABOVE-RACKET) BALL-BELOW-RACKET0
                "tentative ball calculation wrong."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-collides : Ball Racket -> Ball
;; GIVEN : a ball and a Racket
;; RETURNS : ball for the current tick.
;; EXAMPLES :
;; (ball-collides BALL-ABOVE-RACKET RACKET-COLLISION-BALL)
;;    => (make-ball 30 30 3 -9)
;; (ball-collides BALL-COLLISION-SIDE-RIGHT-WALL RACKET-COLLISION-BALL)
;;    => (make-ball 421 8 -3 9)
;; DESIGN STRATEGY : combine simpler functions.

(define (ball-collides b r )
  (if (ball-collides-with-racket? r (tentative-ball b) b)
      (ball-after-racket-collision r (tentative-ball b) b)
      (ball-collides-with-side-wall
       (ball-collides-with-top-wall (tentative-ball b)))))



(begin-for-test
  (check-equal? (ball-collides BALL-ABOVE-RACKET RACKET-COLLISION-BALL)
               (make-ball 30 30 3 -9)
               "invalid state of ball returned by ball-collides")
  
  (check-equal? (ball-collides BALL-COLLISION-SIDE-RIGHT-WALL
                               RACKET-COLLISION-BALL)
                (make-ball 421 8 -3 9)
                "invalid state of ball returned by ball-collides"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-after-tick : Ball Racket Boolean -> Ball
;; GIVEN : a Ball , a Racket and the paused state of the world.
;; RETURNS : a ball at the current tick.
;; EXAMPLES :
;; (ball-after-tick BALL-ABOVE-RACKET RACKET-COLLISION-BALL false)
;;    => (make-ball 30 30 3 -9)
;;
;; (ball-after-tick BALL-COLLISION-SIDE-RIGHT-WALL RACKET-COLLISION-BALL false)
;;    => (make-ball 421 8 -3 9)
;;
;; (ball-after-tick BALL-ABOVE-RACKET RACKET-COLLISION-BALL true)
;;    => BALL-ABOVE-RACKET
;;
;; DESIGN STRATEGY : condition on paused?, combining simpler functions.

(define (ball-after-tick b r paused?)
  (if paused? b
      (ball-collides b r)))



(begin-for-test
  (check-equal? (ball-after-tick BALL-ABOVE-RACKET RACKET-COLLISION-BALL false)
               (make-ball 30 30 3 -9) "ball-after-tick :
invalid state of ball returned")
  
  (check-equal? (ball-after-tick BALL-COLLISION-SIDE-RIGHT-WALL
                               RACKET-COLLISION-BALL
                               false)
                (make-ball 421 8 -3 9) "ball-after-tick :
invalid state of ball returned")
  
  (check-equal? (ball-after-tick BALL-ABOVE-RACKET RACKET-COLLISION-BALL true)
                BALL-ABOVE-RACKET) "ball-after-tick :
invalid state of ball returned")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tentative-racket : Racket -> Racket
;; GIVEN : a racket
;; RETURNS : the tentative next position of the racket
;; WHERE : the position of the tentative racket is (vx + x, vy + y)
;; here x  : the graphical x co-ordinate of the racket in pixels.
;;      y  : the graphical y co-ordinate of the racket in pixels.
;;      xv : the velocity component of the racket in the x direction
;;           in pixels per tick.
;;      vy : the velocity component of the racket in the y direction
;;           in pixels per tick.
;; EXAMPLES :
;; (racket? (tentative-racket RACKET-COLLISION-BALL)) => true
;; DESIGN STRATEGY : use the constructor template of Ball.

(define (tentative-racket r)
  (make-racket (distance-after-tick (racket-x r) (racket-vx r))
             (distance-after-tick (racket-y r) (racket-vy r))
             (racket-vx r)
             (racket-vy r)))

(begin-for-test
  (check-equal? (racket? (tentative-racket RACKET-COLLISION-BALL)) true
                "output should be of type Racket."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-collides-with-side-wall : Racket -> Racket
;; GIVEN : A Racket that may collide with any of the side walls.
;; RETURNS : A Racket after the collision with the wall or the same Racket if
;;           the Racket doesnot collide.
;; EXAMPLES:
;; (racket-collides-with-side-wall RACKET-COLLISION-SIDE-RIGHT-WALL)
;;           => RACKET-AFTER-COLLISION-SIDE-RIGHT-WALL
;;
;; (racket-collides-with-side-wall RACKET-COLLISION-SIDE-LEFT-WALL) 
;;           => RACKET-AFTER-COLLISION-SIDE-LEFT-WALL
;;
;; (racket-collides-with-side-wall INITIAL-RACKET)
;;           => INITIAL-RACKET
;; DESIGN STRATEGY : conditions on the x coordinate of the racket.

(define (racket-collides-with-side-wall r)
  (cond
    
     [(< (racket-x r) HALF-RACKET-WIDTH)
     (make-racket HALF-RACKET-WIDTH
                  (racket-y r)
                  (racket-vx r)
                  (racket-vy r))]
    
    [(> (racket-x r) (- COURT-WIDTH HALF-RACKET-WIDTH))
     (make-racket (- COURT-WIDTH HALF-RACKET-WIDTH)
                  (racket-y r)
                  (racket-vx r)
                  (racket-vy r))]
    [else r]))

(begin-for-test
  (check-equal? (racket-collides-with-side-wall
                 RACKET-COLLISION-SIDE-RIGHT-WALL)
                RACKET-AFTER-COLLISION-SIDE-RIGHT-WALL
                "racket colliding condition failed")
  
  (check-equal? (racket-collides-with-side-wall
                 RACKET-COLLISION-SIDE-LEFT-WALL)
                RACKET-AFTER-COLLISION-SIDE-LEFT-WALL
                "racket colliding condition failed")
  
  (check-equal? (racket-collides-with-side-wall INITIAL-RACKET)
                INITIAL-RACKET "non colliding state should return same racket"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-collides-top-wall? : Racket -> Boolean
;; GIVEN : a Racket which might collide with top wall
;; RETURNS : true, if the racket has collided with the top wall
;;           else false.
;; EXAMPLE :
;; (racket-collides-top-wall? RACKET-COLLISION-TOP-WALL) => true
;; (racket-collides-top-wall? INITIAL-RACKET) => false
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-collides-top-wall? r)
  (<= (racket-y r) HALF-RACKET-HEIGHT))

(begin-for-test
  (check-equal? (racket-collides-top-wall? RACKET-COLLISION-TOP-WALL) true
                "colliding condition should result in true")
  (check-equal? (racket-collides-top-wall? INITIAL-RACKET) false
                "non-colliding condition should return false"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-collides-with-bottom-wall : Racket -> Racket
;; GIVEN : a racket which may collide with the bottom wall.
;; RETURNS : the racket after the collision if the collision takes place.
;;           else returns the same racket.
;; EXAMPLES :
;;(racket-collides-with-bottom-wall RACKET-COLLISION-BOTTOM-WALL)
;;    => RACKET-AFTER-COLLISION-BOTTOM-WALL
;;(racket?(racket-collides-with-bottom-wall RACKET-COLLISION-TOP-WALL)) => true
;;(racket-collides-with-bottom-wall INITIAL-RACKET) => INITIAL-RACKET

;; DESIGN STRATEGY : conditions on y co-ordinate of the ball.

(define (racket-collides-with-bottom-wall r)
  (cond
    [(> (racket-y r) (- COURT-HEIGHT HALF-RACKET-HEIGHT))
     (make-racket (racket-x r)
                  (- COURT-HEIGHT HALF-RACKET-HEIGHT)
                  (racket-vx r)
                  (racket-vy r))]
    
    [(< (racket-y r) HALF-RACKET-HEIGHT)
     (make-racket (racket-x r)
                  HALF-RACKET-HEIGHT
                  0
                  0)]
    
    [else r]))

(begin-for-test
  (check-equal?  (racket-collides-with-bottom-wall RACKET-COLLISION-BOTTOM-WALL)
                 RACKET-AFTER-COLLISION-BOTTOM-WALL
                 "bottom wall collision of racket failing")
  
  (check-equal? (racket?(racket-collides-with-bottom-wall
                         RACKET-COLLISION-TOP-WALL)) true "output should
 be of type Racket")
                
  (check-equal? (racket-collides-with-bottom-wall INITIAL-RACKET)
                INITIAL-RACKET "non collision state must return the same
racket."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-collision-with-ball : Racket -> Racket
;; GIVEN : a racket that has collided with a ball
;; RETURNS :  a racket after the collision has occured.
;; WHERE : if there is a negative velocity in the y axis then that velocity
;;         is made 0.
;; EXAMPLES:
;;(racket-after-collision-with-ball INITIAL-RACKET) => INITIAL-RACKET
;;
;;(racket-after-collision-with-ball (make-racket 30 30 3 -3))
;;    => (make-racket 30 30 3 0)
;; DESIGN STRATEGY : conditions on rackets velocity along the y axis.

(define (racket-after-collision-with-ball r)
  (if (< (racket-vy r) 0)
      (make-racket (racket-x r) (racket-y r) (racket-vx r) 0)
      r))


(begin-for-test
  (check-equal? (racket-after-collision-with-ball INITIAL-RACKET)
                INITIAL-RACKET "non collisision state must result in the same
racket")
  (check-equal? (racket-after-collision-with-ball (make-racket 30 30 3 -3))
                (make-racket 30 30 3 0) "racket after collision with ball
 must loose its upward velocity"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-collision-with-ball : Racket Ball -> Racket
;; GIVEN : a racket that may or may-not collide with a ball.
;; RETURNS : if a collision occures it returns the racket after collision
;;           else returns the original racket.
;; EXAMPLES :
;;(racket-collision-with-ball INITIAL-RACKET (make-ball 30 30 3 -3))
;;    => INITIAL-RACKET
;;
;;(racket-collision-with-ball (make-racket 30 30 3 -3) (make-ball 30 30 3 3))
;;    => (make-racket 30 30 3 0)
;; DESIGN STRATEGY : combine simpler functions.

(define (racket-collision-with-ball r b)
  (if (ball-collides-with-racket? r (tentative-ball b) b)
      (racket-after-collision-with-ball r)
      r))



(begin-for-test
  (check-equal? (racket-collision-with-ball INITIAL-RACKET
                                            (make-ball 30 30 3 -3))
                                            INITIAL-RACKET "no collision start
must return same racket.")
  
  (check-equal? (racket-collision-with-ball (make-racket 30 30 3 -3)
                                            (make-ball 30 30 3 3))
                                            (make-racket 30 30 3 0) "racket
collison with ball results not matching expected results."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-tick : Racket Ball Boolean -> Ball
;; GIVEN : a Racket , a Ball and the paused state of the world.
;; RETURNS : a racket at the current tick.
;; EXAMPLES :
;;(racket-after-tick (make-racket 30 30 5 5) BALL-ABOVE-RACKET false)
;;    => (make-racket 35 35 5 5)
;;
;;(racket-after-tick  RACKET-COLLISION-SIDE-RIGHT-WALL BALL-ABOVE-RACKET false)
;;    => (make-racket 402 3 0 0)
;;
;;(racket-after-tick (make-racket 30 30 5 5) BALL-ABOVE-RACKET true)
;;    => (make-racket 30 30 5 5)

;; DESIGN STRATEGY : condition on paused?, combining simpler functions.

(define (racket-after-tick r b paused?)
  (if paused? r
   (racket-collides-with-side-wall
    (racket-collides-with-bottom-wall
     (tentative-racket (racket-collision-with-ball r b))))))
(begin-for-test
  (check-equal? (racket-after-tick (make-racket 30 30 5 5)
                                   BALL-ABOVE-RACKET
                                   false)
                (make-racket 35 35 5 5) "invalid state of racket returned.")
  
  (check-equal? (racket-after-tick  RACKET-COLLISION-SIDE-RIGHT-WALL
                                    BALL-ABOVE-RACKET
                                    false)
                (make-racket 402 3 0 0) "invalid state of racket returned.")
  
  (check-equal? (racket-after-tick (make-racket 30 30 5 5)
                                   BALL-ABOVE-RACKET
                                   true)
                (make-racket 30 30 5 5) "invalid state of racket returned."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-paused-state-wait : World -> World
;; GIVEN : a world w in paused state.
;; RETURNS : a world like w where world-paused-counter is decremented
;;        when the pause counter reaches 0 the world is reset to initial state.
;;EXAMPLE :
;; (world-paused-state-wait PAUSED-WORLD ) => PAUSED-WORLD1
;; (world-paused-state-wait PAUSED-WORLD1) => INITIAL-WORLD
;; DESIGN STRATEGY : use constructor template for world.\

(define (world-paused-state-wait w)
  (if (> (world-paused-counter w) 0)
  
  (make-world
   (world-ball w)
   (world-racket w)
   (world-court-color w)
   (world-speed w)
   (world-paused? w)
   (- (world-paused-counter w) 1)
   (world-ready-to-serve? w))
  
  (initial-world (world-speed w))))

(begin-for-test
  (check-equal? (world-paused-state-wait PAUSED-WORLD ) PAUSED-WORLD1
                "paused counter not working.")
  (check-equal? (world-paused-state-wait PAUSED-WORLD1) INITIAL-WORLD
                "paused counter not working."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-check-paused : World -> World
;; GIVEN : a world.
;; RETURNS : the state of the world at the current tick.
;; EXAMPLES :
;; (world-check-paused INITIAL-WORLD) => INITIAL-WORLD
;; (world-check-paused PAUSED-WORLD ) => PAUSED-WORLD1
;; DESIGN STRATEGY : conditions on world-paused?

(define (world-check-paused w)
  (if (world-paused? w)
      
      (world-paused-state-wait w)
      
      (make-world
       (ball-after-tick (world-ball w)
                        (racket-after-tick (world-racket w)
                                           (world-ball w)
                                           (world-paused? w))
                        (world-paused? w))
       
       (racket-after-tick (world-racket w) (world-ball w) (world-paused? w))
       (world-court-color w)
       (world-speed w)
       (world-paused? w)
       (world-paused-counter w)
       (world-ready-to-serve? w))))


(begin-for-test
  (check-equal? (world-check-paused INITIAL-WORLD) INITIAL-WORLD
                "paused counter not working.")
  (check-equal? (world-check-paused PAUSED-WORLD ) PAUSED-WORLD1
                "paused counter not working."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: any world that's possible for the simulation
;; RETURNS: the world that should follow the given world
;;     after a tick
;; EXAMPLES:
;; (world? (world-after-tick INITIAL-WORLD)) => true
;; (world-after-tick PAUSED-WORLD) => PAUSED-WORLD1
;; (world-after-tick EXCEPTION-WORLD) => EXCEPTION-WORLD-OUTPUT
;; DESIGN STRATEGY : combine simpler functions. 
  
(define (world-after-tick w)
  (if (and (or (racket-collides-top-wall? (world-racket w))
          (ball-collides-with-bottom-wall? (world-ball w)))
           (not (world-paused? w)))
      
      (pause-world w)
      
      (world-check-paused w)))

(begin-for-test
  (check-equal? (world? (world-after-tick INITIAL-WORLD)) true
                "should return type World")
  (check-equal? (world-after-tick PAUSED-WORLD ) PAUSED-WORLD1
                "paused counter not working.")
  (check-equal? (world-after-tick EXCEPTION-WORLD) EXCEPTION-WORLD-OUTPUT
                "World not handling exception cases of ball and racket."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rally-ball : Ball -> Ball
;; GIVEN : a ball
;; RETURNS : the same ball in rally state.
;; EXAMPLES :
;;(rally-ball INITIAL-BALL) => (make-ball READY-TO-SERVE-X
;;                                        READY-TO-SERVE-Y
;;                                        RALLY-BALL-VELOCITY-X
;;                                        RALLY-BALL-VELOCITY-Y)
;; DESIGN STRATEGY : change ball velocities to match rally velocity.

(define (rally-ball b)
  (make-ball (ball-x b) (ball-y b) RALLY-BALL-VELOCITY-X RALLY-BALL-VELOCITY-Y))


(begin-for-test
  (check-equal? (rally-ball INITIAL-BALL) (make-ball READY-TO-SERVE-X
                                                     READY-TO-SERVE-Y
                                                     RALLY-BALL-VELOCITY-X
                                                     RALLY-BALL-VELOCITY-Y)
                "ball not in rally state"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pause-world : World -> World.
;; GIVEN : any World.
;; RETURNS : paused version of the same world.
;; EXAMPLE :
;;(pause-world (make-world INITIAL-BALL INITIAL-RACKET WHITE 1 false 3 true))
;;    => (make-world INITIAL-BALL INITIAL-RACKET YELLOW 1 true 3 true)
;; DESIGN STRATEGY : use the constructor template for World.

(define (pause-world w)
  (make-world (world-ball w)
              (world-racket w)
              YELLOW
              (world-speed w)
              true
              (world-paused-counter w)
              (world-ready-to-serve? w)))



(begin-for-test
  (check-equal?
   (pause-world (make-world INITIAL-BALL INITIAL-RACKET WHITE 1 false 3 true))
   (make-world INITIAL-BALL INITIAL-RACKET YELLOW 1 true 3 true)
   "world not in paused state."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-up-key : Racket -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with decreased velocity on the y axis
;; EXAMPLE :
;; (racket-after-up-key INITIAL-RACKET) => (make-racket 330 384 0 -1)
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-after-up-key r )
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (- (racket-vy r) RACKET-SPEED)))
(begin-for-test
  (check-equal? (racket-after-up-key INITIAL-RACKET)
                (make-racket 330 384 0 -1)
                "racket after up key failing."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-down-key : Racket -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with increased velocity on the y axis
;; EXAMPLE :
;; (racket-after-down-key INITIAL-RACKET) => (make-racket 330 384 0 1)
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-after-down-key r )
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (+ (racket-vy r) RACKET-SPEED)))

(begin-for-test
  (check-equal? (racket-after-down-key INITIAL-RACKET)
                (make-racket 330 384 0 1) "racket after down key failing."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-left-key : Racket -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with decreased velocity on the x axis
;; EXAMPLE :
;; (racket-after-left-key INITIAL-RACKET) => (make-racket 330 384 -1 0)
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-after-left-key r )
  (make-racket (racket-x r)
               (racket-y r)
               (- (racket-vx r) RACKET-SPEED)
               (racket-vy r)))

(begin-for-test
  (check-equal? (racket-after-left-key INITIAL-RACKET)
                (make-racket 330 384 -1 0) "racket after left key failing."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-right-key : Racket -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with increased velocity on the x axis
;; EXAMPLE :
;; (racket-after-right-key INITIAL-RACKET) => (make-racket 330 384 1 0)
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-after-right-key r )
  (make-racket (racket-x r)
               (racket-y r)
               (+ (racket-vx r) RACKET-SPEED)
               (racket-vy r)))


(begin-for-test
  (check-equal? (racket-after-right-key INITIAL-RACKET)
                (make-racket 330 384 1 0) "racket after right key failing."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-key : Racket KeyEvent -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with velocities modified based on key pressed.
;; EXAMPLE :
;; (racket-after-key INITIAL-RACKET "up") => (make-racket 330 384 0 -1)
;; (racket-after-key INITIAL-RACKET "down") => (make-racket 330 384 0 1)
;; (racket-after-key INITIAL-RACKET "left") => (make-racket 330 384 -1 0)
;; (racket-after-key INITIAL-RACKET "right") => (make-racket 330 384 1 0)
;; (racket-after-key INITIAL-RACKET "q") => INITIAL-RACKET
;; DESIGN STRATEGY : condition on kev, calling simpler functions.

(define (racket-after-key r kev)
  (cond
    [(key=? "up" kev) (racket-after-up-key r)]
    [(key=? "down" kev) (racket-after-down-key r)]
    [(key=? "left" kev) (racket-after-left-key r)]
    [(key=? "right" kev) (racket-after-right-key r)]
    [else r]))


(begin-for-test
  (check-equal? (racket-after-key INITIAL-RACKET "up")
                (make-racket 330 384 0 -1) "racket after up key failing.")
  
  (check-equal? (racket-after-key INITIAL-RACKET "down")
                (make-racket 330 384 0 1) "racket after down key failing.")
  
  (check-equal? (racket-after-key INITIAL-RACKET "left")
                (make-racket 330 384 -1 0) "racket after left key failing.")
  
  (check-equal? (racket-after-key INITIAL-RACKET "right")
                (make-racket 330 384 1 0) "racket after right key failing.")
  
  (check-equal? (racket-after-key INITIAL-RACKET "q")
                INITIAL-RACKET) "racket responding to invalid input")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key : World KeyEvent -> World
;; GIVEN : a World
;; RETURNS : a World, after the input world has been modified based on the
;;          key event.
;; EXAMPLES:
;;(world? (world-after-key PAUSED-WORLD "up")) => true
;;(world? (world-after-key INITIAL-WORLD "right")) => true
;; DESIGN STRATEGY : combine simpler functions.

(define (world-after-key w kev)
  (if(world-paused? w)
     w
     (make-world (world-ball w)
                 (racket-after-key (world-racket w) kev)
                 (world-court-color w)
                 (world-speed w)
                 (world-paused? w)
                 (world-paused-counter w)
                 (world-ready-to-serve? w))))

(begin-for-test
  (check-equal? (world? (world-after-key PAUSED-WORLD "up")) true
                "should return type World")
  (check-equal? (world? (world-after-key INITIAL-WORLD "right")) true
                "should return type World"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-space-key : World -> World
;; GIVEN : a world after the space bar key is hit.
;; RETURNS : a world that is either in rally state or in paused state based on
;;           the previous state of the world.
;; EXAMPLES :
;; (world? (world-after-space-key INITIAL-WORLD)) => true
;; (world? (world-after-space-key RALLY-WORLD)) => true
;; DESIGN STRATEGY : cases on world-ready-to-serve?

(define (world-after-space-key w)
  (if (world-ready-to-serve? w)
      (make-world (rally-ball (world-ball w))
                  (world-racket w)
                  (world-court-color w)
                  (world-speed w)
                  (world-paused? w)
                  (world-paused-counter w)
                  false)
      (pause-world w)))

(begin-for-test
  (check-equal? (world? (world-after-space-key INITIAL-WORLD)) true
                "should return type World.")
  (check-equal? (world? (world-after-space-key RALLY-WORLD)) true
                "should return type World."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world and a key event
;; RETURNS: a world after the key even has been applied to it.
;; EXAMPLES :
;;(world? (world-after-key-event INITIAL-WORLD  " ")) => true
;;(world? (world-after-key-event RALLY-WORLD "up")) => true
;;(world? (world-after-key-event INITIAL-WORLD  "up")) => true
;; DESIGN STRATEGY : causes on keyEvent.

(define (world-after-key-event w kev)
  (cond
    [(key=? " " kev) (world-after-space-key w)]
    [(not (world-ready-to-serve? w)) (world-after-key w kev)]
    [else w]))

(begin-for-test
  (check-equal? (world? (world-after-key-event INITIAL-WORLD  " ")) true
                "should return type World.")
  (check-equal? (world? (world-after-key-event RALLY-WORLD "up")) true
                "should return type World.")
  (check-equal? (world? (world-after-key-event INITIAL-WORLD  "up")) true
                "should return type World."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw-world : World -> Scene.
;; GIVEN: a world.
;; RETURNS : a scene which describes the world provided.
;; EXAMPLE:
;; (image? (draw-world INITIAL-WORLD)) => true
;; DESIGN STRATEGY : combine simpler functions.

(define (draw-world w)
  (place-image RACKET
               (racket-x (world-racket w))
               (racket-y (world-racket w))
               (place-image BALL
                            (ball-x (world-ball w))
                            (ball-y (world-ball w))
                            (empty-scene COURT-WIDTH
                                         COURT-HEIGHT
                                         (world-court-color w)))))
(begin-for-test
  (check-equal? (image? (draw-world INITIAL-WORLD)) true)
  "should return type image")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       







