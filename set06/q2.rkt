;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Squash Practice Simulation :
;;
;;  A game of squash is simulated where the racket can be controlled by the
;;  user using the arrow keys.
;;
;;  the ball responds to being hit be the racket/ walls 
;;  press the space bar to toggle between the ready-to-serve and rally state.
;;
;; TO START TYPE : (simulation 1/24), where  1/24 = speed of simulation. 
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
         world-balls
         world-racket
         ball-x
         ball-y
         racket-x
         racket-y
         ball-vx
         ball-vy
         racket-vx
         racket-vy
         world-after-mouse-event
         racket-after-mouse-event
         racket-selected?)

(check-location "06" "q2.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; numeric constants : 
(define COURT-HEIGHT 649)
(define COURT-WIDTH 425)
(define RACKET-WIDTH 47)
(define RACKET-HEIGHT 7)
(define BALL-RADIUS 3)
(define SELECTOR-RADIUS 4)
(define READY-TO-SERVE-X 330)
(define READY-TO-SERVE-Y 384)
(define RACKET-SPEED 1)
(define HALF-RACKET-WIDTH (floor (/ RACKET-WIDTH 2)))
(define HALF-RACKET-HEIGHT (floor (/ RACKET-HEIGHT 2)))
(define INITIAL-VELOCITY-X 0)
(define INITIAL-VELOCITY-Y 0)
(define RALLY-BALL-VELOCITY-X 3)
(define RALLY-BALL-VELOCITY-Y -9)
(define INITIAL-SELECTOR-X (+ COURT-WIDTH 5))
(define INITIAL-SELECTOR-Y (+ COURT-HEIGHT 5))
(define RACKET-RANGE 25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key constants :
(define UP-KEY "up")
(define DOWN-KEY "down")
(define LEFT-KEY "left")
(define RIGHT-KEY "right")
(define SPACE-KEY " ")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; image constants :
(define RACKET (rectangle RACKET-WIDTH RACKET-HEIGHT "solid" "green"))
(define BALL (circle BALL-RADIUS "solid" "black"))
(define SELECTOR (circle SELECTOR-RADIUS "solid" "blue"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Racket is implemented as a structure
;;     (make-racket x y vx vy selected? sx sy pvx pvy)
;; where :
;; x         : Int,     the value of the x co-ordinate of racket in pixels.
;; y         : Int,     the value of the y co-ordinate of racket in pixels.
;; vx        : Int,     the velocity of the racket along x axis in pixels per
;;                      tick.
;; vy        : Int,     the velocity of the racket along the y axis in pixels
;;                      per tick.
;; selected? : Boolean, determines if the racket has been selected by using
;;                      the  mouse.
;; sx        : Int,     the x co-ordinate of the blue circle which indicates
;;                      the position of the mouse on button press in pixels.
;; sy        : Int,     the y co-ordinate of the blue circle which indicates
;;                      the position of the mouse on button press in pixels.
;; pvx       : Int,     the previous velocity of the racket along the x axis
;;                      in pixels per tick. 
;; pvy       : Int,     the previous velocity of the racket along the y axis
;;                      in pixels per tick


;; IMPLEMENTATION
(define-struct racket (x y vx vy selected? sx sy pvx pvy))

;; CONSTRUCTOR TEMPLATE
;; (make-racket Int Int Int Int Boolean Int Int Int Int)

;; OBSERVER TEMPLATE
;; racket-fn : Racket -> ??
(define (racket-fn r)
  (... (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)
       (racket-selected? r)
       (racket-sx r)
       (racket-sy r)
       (racket-pvx r)
       (racket-pvy r)))


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
;; ball-fn : Ball -> ??
(define (ball-fn b)
  (... (ball-x b)
       (ball-y b)
       (ball-vx b)
       (ball-vy b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BallList is represented as a list of Ball, in the order of the creation of
;; the Ball during simulation.
;;
;; CONSTRUCTOR TEMPLATE:
;; -- empty
;; -- (cons Ball BallList)
;;
;; OBSERVER TEMPLATE:
;; ball-list-fn: BallList-> ??

;;(define (ball-list-fn bl)
;;  (cond
;;    [(empty? bl) empty]
;;    [else (...
;;           (ball-fn (first bl))
;;           (ball-list-fn (rest bl)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CourtColor is one of the following Strings:
;; -- "white"
;; -- "yellow"
;; INTERP : the color that the squash court changes to, based on the state of
;;          the simulation.

(define WHITE  "white")
(define YELLOW "yellow")


;; OBSERVER TEMPLATE:
;; (define (court-color c)
;;   (cond
;;     [(string=? c "white") ...]
;;     [(string=? c "yellow") ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; World is implemented as a structure
;;     (make-world balls
;;                 racket
;;                 court-color
;;                 speed
;;                 paused?
;;                 paused-counter
;;                 ready-to-serve?)
;; where :
;; balls           : BallList,   is a list of balls currently in the world.
;; racket          : Racket,     is a Racket object currently in the World.
;; court-color     : CourtColor, is the current color of the court.
;; speed           : PosReal,    the speed at which the simulation is run.
;;                                in seconds per tick.
;; paused?         : Boolean,    true if the world is paused else false.
;; paused-counter  : PosInt,     the count in ticks to the end of paused state.
;; ready-to-serve? : Boolean,    decides if the world is in the ready to serve
;;                               state.

;; IMPLEMENTATION
(define-struct world
  (balls racket court-color speed paused? paused-counter ready-to-serve?))

;; CONSTRUCTOR TEMPLATE
;; (make-world BallList Racket CourtColor PosReal Boolean PosInt Boolean)

;; OBSERVER TEMPLATE
;; world-fn : WORLD -> ??
(define (world-fn c)
  (... (world-balls c)
       (world-racket c)
       (world-court-color c)
       (world-speed c)
       (world-paused? c)
       (world-paused-counter c)
       (world-ready-to-serve? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants :

(define INITIAL-BALL-LIST
  (list (make-ball READY-TO-SERVE-X
             READY-TO-SERVE-Y
             INITIAL-VELOCITY-X
             INITIAL-VELOCITY-Y)))

(define INITIAL-BALL
  (make-ball READY-TO-SERVE-X
             READY-TO-SERVE-Y
             INITIAL-VELOCITY-X
             INITIAL-VELOCITY-Y))

(define INITIAL-RACKET
  (make-racket READY-TO-SERVE-X
               READY-TO-SERVE-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
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
               RALLY-BALL-VELOCITY-Y
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define RACKET-AFTER-COLLISION-SIDE-RIGHT-WALL
  (make-racket 402
               1
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define RACKET-COLLISION-SIDE-LEFT-WALL
  (make-racket 0
               1
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define RACKET-AFTER-COLLISION-SIDE-LEFT-WALL
  (make-racket 23
               1
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define RACKET-COLLISION-TOP-WALL
  (make-racket 1
               0
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define RACKET-AFTER-COLLISION-TOP-WALL
  (make-racket 1
               3
               0
               0
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define RACKET-COLLISION-BOTTOM-WALL
  (make-racket 1
               COURT-HEIGHT
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define RACKET-AFTER-COLLISION-BOTTOM-WALL
  (make-racket 1
               646
               RALLY-BALL-VELOCITY-X
               RALLY-BALL-VELOCITY-Y
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))


(define BALL-ABOVE-RACKET (make-ball 27
                                     27
                                     RALLY-BALL-VELOCITY-X
                                     (- RALLY-BALL-VELOCITY-Y)))

(define BALL-BELOW-RACKET0 (make-ball 30
                                      36
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

(define RACKET-COLLISION-BALL
  (make-racket 30
               30
               0
               0
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define RACKET-COLLISION-BALL-LIST
  (list (make-racket 30
               30
               0
               0
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y)))

(define BALL-AFTER-COLLISION-RACKET1 (make-ball 32
                                                27
                                                RALLY-BALL-VELOCITY-X
                                                RALLY-BALL-VELOCITY-Y))

(define BALL-AFTER-COLLISION-RACKET2 (make-ball 25
                                                27
                                                RALLY-BALL-VELOCITY-X
                                                RALLY-BALL-VELOCITY-Y))



(define PAUSED-WORLD
  (make-world INITIAL-BALL-LIST INITIAL-RACKET WHITE 1 true 1 true))

(define PAUSED-WORLD1
 (make-world (list (make-ball 330 384 0 0))
             (make-racket 330 384 0 0 false 430 654 0 0)
             WHITE
             1
             false
             2
             true))

(define INITIAL-WORLD (make-world INITIAL-BALL-LIST
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

(define EXCEPTION-WORLD (make-world (list BALL-COLLISION-BOTTOM-WALL)
                                    RACKET-COLLISION-TOP-WALL
                                    WHITE
                                    1
                                    false
                                    3
                                    true))

(define EXCEPTION-WORLD-OUTPUT (make-world
                                empty
                                (make-racket 23
                                             3
                                             0
                                             0
                                             false
                                             INITIAL-SELECTOR-X
                                             INITIAL-SELECTOR-Y
                                             INITIAL-VELOCITY-X
                                             INITIAL-VELOCITY-Y)
                                YELLOW
                                1
                                true

                                3
                                true))


(define SELECTED-RACKET
  (make-racket 0
               0
               0
               0
               true
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))

(define UNSELECTED-RACKET
  (make-racket 0
               0
               0
               0
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))
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
            (on-mouse world-after-mouse-event)
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
;;(initial-world 1)
;;=> (make-world INITIAL-BALL-LIST INITIAL-RACKET WHITE 1 false 3 true)
;; DESIGN STRATEGY : use the constructor template of World.

(define (initial-world speed)
  (make-world INITIAL-BALL-LIST
              INITIAL-RACKET
              WHITE
              speed
              false
              (* (/ 1 speed) 3)
              true))

(begin-for-test
  (check-equal? (initial-world 1) (make-world INITIAL-BALL-LIST
                                              INITIAL-RACKET
                                              WHITE
                                              1
                                              false
                                              3
                                              true)
                "for a speed of 1 sec the number of ticks should equal 3."))


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
;; ball-after-collision-side-wall : Int Int Ball -> Ball
;; GIVEN : the x coordinate ballx, and y coordinate bally, of the ball along
;;         with the Ball b
;; RETURNS : a ball like b but with the ballx and bally as the x and y
;;           co-ordinates respectively.
;; EXAMPLES :
;;  (ball-after-collision-side-wall 0 0 INITIALBALL) => (make-ball 0 0 0 0)
;; DESIGN STRATEGY : use the constructor template for ball.

(define (ball-after-collision-side-wall ballx bally b)
  (make-ball ballx
             bally
             (- (ball-vx b))
             (ball-vy b)))

(begin-for-test
  (check-equal? (ball-after-collision-side-wall 0 0 INITIAL-BALL)
                (make-ball 0 0 0 0)
                "the new ball should have 0,0 as new x and y coordinates."))

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
;;(ball-collides-with-side-wall INITIAL-BALL)
;;    => INITIAL-BALL

;; DESIGN STRATEGY : call a more generic function.

(define (ball-collides-with-side-wall b)
  (cond
    [(< (ball-x b) 0)
     (ball-after-collision-side-wall (- (ball-x b)) (ball-y b) b)]
    
    [(> (ball-x b) COURT-WIDTH)
     (ball-after-collision-side-wall (- COURT-WIDTH (- (ball-x b) COURT-WIDTH))
                           (ball-y b) b)]
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
;; RETURNS : the ball after the collision, if the collision takes place.
;;           else returns the same ball.
;; EXAMPLES :
;;(ball-collides-with-top-wall BALL-COLLISION-TOP-WALL)
;;=> BALL-AFTER-COLLISION-TOP-WALL
;;
;;(ball-collides-with-top-wall INITIAL-BALL) => INITIAL-BALL 
;; DESIGN STRATEGY : conditions on y co-ordinate of the ball.

(define (ball-collides-with-top-wall b)
  (cond
    [(< (ball-y b) 0)
     (make-ball (ball-x b)
                (- (ball-y b))
                (ball-vx b)
                (- (ball-vy b)))]
    [else b]))


(begin-for-test
  (check-equal? (ball-collides-with-top-wall BALL-COLLISION-TOP-WALL)
                BALL-AFTER-COLLISION-TOP-WALL
                 "ball after top wall collision condition failed")
  (check-equal? (ball-collides-with-top-wall INITIAL-BALL)
                INITIAL-BALL
                "ball that doesnot have collision conditions must not be
 affected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-after-racket-collision : Racket Ball Ball -> Ball
;; GIVEN : the current Racket , tentative ball and original ball.
;;         (the ball has to collide with the racket.)
;; RETURNS : the Ball after it has collided with the racket.
;; EXAMPLES :
;;(ball-after-racket-collision RACKET-COLLISION-BALL
;;                              BALL-BELOW-RACKET1
;;                              BALL-ABOVE-RACKET )
;;=> BALL-AFTER-COLLISION-RACKET1 
;;
;;(ball-after-racket-collision RACKET-COLLISION-BALL
;;                              BALL-BELOW-RACKET2
;;                              BALL-ABOVE-RACKET )
;;=> BALL-AFTER-COLLISION-RACKET2 
;;
;;(ball-after-racket-collision RACKET-COLLISION-BALL
;;                              BALL-BELOW-RACKET3
;;                              BALL-ABOVE-RACKET )
;;=> BALL-AFTER-COLLISION-RACKET1 
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
                                             BALL-ABOVE-RACKET )
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
;; current-racket : Racket -> Racket
;; GIVEN : the Racket of the previous world state.
;; RETURNS : the racket in the current world state before collision with ball.
;; EXAMPLE :
;;(current-racket INITIAL-RACKET) => INITIAL-RACKET
;;(current-racket (make-racket 330 384 1 1 #false 430 654 0 0))
;;=> (make-racket 331 385 1 1 #false 430 654 0 0)
;; DESIGN STRATEGY : combine simpler functions.
(define (current-racket r)
  (racket-collides-with-bottom-wall
  (racket-collides-with-side-wall
   (tentative-racket r))))

(begin-for-test
  (check-equal? (current-racket INITIAL-RACKET)
                INITIAL-RACKET
                "invalid current racket generated.")
  (check-equal? (current-racket (make-racket 330 384 1 1 #false 430 654 0 0))
                (make-racket 331 385 1 1 #false 430 654 0 0)
                "invalid current racket generated."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; previous-tick-check : Racket Ball -> Boolean
;; GIVEN : the Racket and Ball of the previous world state.
;; RETURNS : true, if the ball doesnot lie on the racket.
;; EXAMPLES:
;;(previous-tick-check RACKET-COLLISION-BALL BALL-ABOVE-RACKET) => true
;;(previous-tick-check RACKET-COLLISION-BALL (make-ball 30 30 1 1 )) => false
;; DESIGN STRATEGY: Transcribe formula.

(define (previous-tick-check r b)
  (not (and
         (<= (- (racket-x r) HALF-RACKET-WIDTH) (ball-x b))
         (>= (+ (racket-x r) HALF-RACKET-WIDTH) (ball-x b))
         (= (racket-y r) (ball-y b))
         )))

(begin-for-test
  (check-equal? (previous-tick-check RACKET-COLLISION-BALL BALL-ABOVE-RACKET)
                true
                "true, the ball doesnot lie on the racket.")
  (check-equal?
   (previous-tick-check RACKET-COLLISION-BALL (make-ball 30 30 1 1 ))
   false
   "false, the ball lies on the racket."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ball-path-check : Racket Ball -> Boolean
;; GIVEN : a racket and a ball of the previous world state.
;; RETURNS : true if the path between the previous state ball and calculated
;; tentative state ball collides with the racket.
;; EXAMPLES:
;;(ball-collides-with-racket? RACKET-COLLISION-BALL BALL-ABOVE-RACKET) => true 
;;(ball-collides-with-racket? RACKET-COLLISION-BALL INITIAL-BALL) => false 
;; DESIGN STRATEGY : Transcribe formula.

(define (ball-path-check r b)
   (and (<= (- (racket-x (current-racket r)) HALF-RACKET-WIDTH) (ball-x b))
        (>= (+ (racket-x (current-racket r)) HALF-RACKET-WIDTH) (ball-x b))
        (<= (ball-y b) (racket-y (current-racket r)))
        (>= (ball-y (tentative-ball b)) (racket-y (current-racket r)))))

(begin-for-test
   (check-equal? (ball-collides-with-racket? RACKET-COLLISION-BALL                                            
                                            BALL-ABOVE-RACKET) true "collision
condition should return true.")
    (check-equal? (ball-collides-with-racket? RACKET-COLLISION-BALL
                                            INITIAL-BALL) false "not collision
condition should return false."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ball-collides-with-racket? :  Racket Ball -> Boolean
;; GIVEN : the current racket at the tick, the tentative ball
;;         and the original ball.
;; RETURNS : true if the ball collides with the racket else false.
;; EXAMPLES :
;;(ball-collides-with-racket? RACKET-COLLISION-BALL BALL-ABOVE-RACKET) => true 
;;(ball-collides-with-racket? RACKET-COLLISION-BALL INITIAL-BALL) => false 
;; DESIGN STRATEGY : Transcribe formula.

(define (ball-collides-with-racket? r b)
  (and
   (previous-tick-check r b)
   (>= (ball-vy b) 0)
   (ball-path-check r b)
  ))


(begin-for-test
  (check-equal? (ball-collides-with-racket? RACKET-COLLISION-BALL                                            
                                            BALL-ABOVE-RACKET) true "collision
condition should return true.")
  (check-equal? (ball-collides-with-racket? RACKET-COLLISION-BALL
                                            INITIAL-BALL) false "not collision
condition should return false."))

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
;;(ball-collides BALL-ABOVE-RACKET RACKET-COLLISION-BALL)
;;=> (make-ball 30 27 3 -9)
;;(ball-collides BALL-COLLISION-SIDE-RIGHT-WALL RACKET-COLLISION-BALL)
;;=> (make-ball 421 8 -3 9)

;; DESIGN STRATEGY : combine simpler functions.

(define (ball-collides b r)
  (if (ball-collides-with-racket? r b)
      (ball-after-racket-collision r (tentative-ball b) b)
      (ball-collides-with-side-wall
       (ball-collides-with-top-wall (tentative-ball b)))))



(begin-for-test
  (check-equal? (ball-collides BALL-ABOVE-RACKET RACKET-COLLISION-BALL)
                (make-ball 30 27 3 -9)
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
;;(ball-after-tick
;; (list BALL-COLLISION-BOTTOM-WALL BALL-COLLISION-SIDE-RIGHT-WALL)
;; RACKET-COLLISION-BALL
;; false)
;;=> (list (make-ball 421 8 -3 9))
;;
;;(ball-after-tick (list BALL-COLLISION-SIDE-RIGHT-WALL)
;;                 RACKET-COLLISION-BALL
;;                 false)
;;=> (list (make-ball 421 8 -3 9))
;;
;;(ball-after-tick (list BALL-ABOVE-RACKET) RACKET-COLLISION-BALL true)
;;=> (list BALL-ABOVE-RACKET)

;; DESIGN STRATEGY : condition on paused?, use HOF map and filter.

(define (ball-after-tick bl r paused?)
  (cond
   [paused? bl]
   [else
     (map
      ;; Ball -> Ball
      ;; GIVEN : a Ball b
      ;; RETURNS : a Ball which represents b at the current tick.
      (lambda (b) (ball-collides b r))
      (filter
       ;; Ball -> Boolean
       ;; GIVEN : a Ball b
       ;; RETURNS : true if the ball has not collided with the bottom wall.
       (lambda (b) (not (ball-collides-with-bottom-wall? b))) bl))]))

(begin-for-test
  (check-equal? (ball-after-tick (list BALL-COLLISION-BOTTOM-WALL
                                      BALL-COLLISION-SIDE-RIGHT-WALL)
                                 RACKET-COLLISION-BALL
                                 false)
               (list (make-ball 421 8 -3 9))"ball-after-tick :
invalid state of ball returned")
                
  (check-equal? (ball-after-tick (list BALL-COLLISION-SIDE-RIGHT-WALL)
                                 RACKET-COLLISION-BALL
                                 false)
                (list (make-ball 421 8 -3 9)) "ball-after-tick :
invalid state of ball returned")
  
  (check-equal? (ball-after-tick (list BALL-ABOVE-RACKET)
                                 RACKET-COLLISION-BALL
                                 true)
                (list BALL-ABOVE-RACKET)) "ball-after-tick :
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
               (racket-vy r)
               (racket-selected? r)
               (racket-sx r)
               (racket-sy r)
               (racket-pvx r)
               (racket-pvy r)))

(begin-for-test
  (check-equal? (racket? (tentative-racket RACKET-COLLISION-BALL)) true)
  "output should be of type Racket.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; racket-after-collision-side-wall : Int Racket -> Racket
;; GIVEN : the x coordinate racketx of the Racket and the Racket r.
;; RETURNS : a Racket like r but with racketx as its x coordinate.
;; EXAMPLES :
;; (racket-after-collision-side-wall 0 INITIAL-RACKET) =>
;; (make-racket 0 384 0 0 #false 430 654 0 0)
;; DESIGN STRATEGY : use the constructor template of Racket.

(define (racket-after-collision-side-wall racketx r)
  (make-racket    racketx
                  (racket-y r)
                  (racket-vx r)
                  (racket-vy r)
                  (racket-selected? r)
                  (racket-sx r)
                  (racket-sy r)
                  (racket-pvx r)
                  (racket-pvy r)))
(begin-for-test
  (check-equal? (racket-after-collision-side-wall 0 INITIAL-RACKET)
                (make-racket 0 384 0 0 false 430 654 0 0)))

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
;; DESIGN STRATEGY : call a more generic function.

(define (racket-collides-with-side-wall r)
  (cond
    [(< (racket-x r) HALF-RACKET-WIDTH)
     (racket-after-collision-side-wall HALF-RACKET-WIDTH r)]

    [(> (racket-x r) (- COURT-WIDTH HALF-RACKET-WIDTH))
     (racket-after-collision-side-wall (- COURT-WIDTH HALF-RACKET-WIDTH) r)]

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
;;(racket-collides-top-wall? RACKET-COLLISION-TOP-WALL) => true
;;(racket-collides-top-wall? INITIAL-RACKET) => false
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
;;
;;(racket?(racket-collides-with-bottom-wall RACKET-COLLISION-TOP-WALL)) => true
;;
;;(racket-collides-with-bottom-wall INITIAL-RACKET) => INITIAL-RACKET
;;
;;
;; DESIGN STRATEGY : conditions on y co-ordinate of the ball.

(define (racket-collides-with-bottom-wall r)
  (cond
    [(> (racket-y r) (- COURT-HEIGHT HALF-RACKET-HEIGHT))
     (make-racket (racket-x r)
                  (- COURT-HEIGHT HALF-RACKET-HEIGHT)
                  (racket-vx r)
                  (racket-vy r)
                  (racket-selected? r)
                  (racket-sx r)
                  (racket-sy r)
                  (racket-pvx r)
                  (racket-pvy r))]
    
   
    [(< (racket-y r) HALF-RACKET-HEIGHT)
     (make-racket (racket-x r)
                  HALF-RACKET-HEIGHT
                  0
                  0
                  (racket-selected? r)
                  (racket-sx r)
                  (racket-sy r)
                  (racket-pvx r)
                  (racket-pvy r))]
     
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
;;(racket-after-collision-with-ball
;; (make-racket 30
;;              30
;;              3
;;              -3
;;              false
;;              INITIAL-SELECTOR-X
;;              INITIAL-SELECTOR-Y
;;              INITIAL-VELOCITY-X
;;              INITIAL-VELOCITY-Y))
;;=> (make-racket 30
;;                30
;;                3
;;                0
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;; DESIGN STRATEGY : conditions on rackets velocity along the y axis.

(define (racket-after-collision-with-ball r)
  (if (< (racket-vy r) 0)
      (make-racket
       (racket-x r)
       (racket-y r)
       (racket-vx r)
       0
       (racket-selected? r)
       (racket-sx r)
       (racket-sy r)
       (racket-pvx r)
       (racket-pvy r))
      r))


(begin-for-test
  (check-equal? (racket-after-collision-with-ball INITIAL-RACKET)
                INITIAL-RACKET "non collisision state must result in the same
racket")
  (check-equal?
   (racket-after-collision-with-ball
    (make-racket 30
                 30
                 3
                 -3
                 false
                 INITIAL-SELECTOR-X
                 INITIAL-SELECTOR-Y
                 INITIAL-VELOCITY-X
                 INITIAL-VELOCITY-Y))
   
   (make-racket 30
                30
                3
                0
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y) "racket
after collision with ball must loose its upward velocity"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-collision-with-ball : Racket BallList  -> Racket
;; GIVEN : a racket that may or may-not collide with a ball.
;; RETURNS : if a collision occures with any ball, return the racket after
;;           collision else returns the original racket.
;; EXAMPLES :
;;(racket-collision-with-ball INITIAL-RACKET
;;                            (list (make-ball 30 30 3 -3)))
;;=> INITIAL-RACKET
;;
;;(racket-collision-with-ball (make-racket 30
;;                                         30
;;                                         3
;;                                         -3
;;                                         false
;;                                         INITIAL-SELECTOR-X
;;                                         INITIAL-SELECTOR-Y
;;                                         INITIAL-VELOCITY-X
;;                                         INITIAL-VELOCITY-Y)
;;                            (list (make-ball 30 27 3 3)))
;;=> (make-racket 30
;;                30
;;                3
;;                0
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;; DESIGN STRATEGY : use HOF andmap to check if any ball collides with racket.

(define (racket-collision-with-ball r bl)
  (if (andmap
       ;; Ball -> Boolean
       ;; GIVEN : a Ball b
       ;; RETURNS : true, if the ball collides with the racket.
       (lambda (b) (ball-collides-with-racket? r b)) bl)
      (racket-after-collision-with-ball r)
      r))
 


(begin-for-test
  (check-equal? (racket-collision-with-ball INITIAL-RACKET
                                            (list (make-ball 30 30 3 -3)))
                INITIAL-RACKET "no collision start must return same racket.")
  
  (check-equal?
   (racket-collision-with-ball
    (make-racket 30
                 30
                 3
                 -3
                 false
                 INITIAL-SELECTOR-X
                 INITIAL-SELECTOR-Y
                 INITIAL-VELOCITY-X
                 INITIAL-VELOCITY-Y)
    (list (make-ball 30 27 3 3)))
   (make-racket 30
                30
                3
                0
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y) "racket
collison with ball results not matching expected results."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-tick : Racket Ball Boolean -> Racket.
;; GIVEN : a Racket , a Ball and the paused state of the world.
;; RETURNS : a racket at the current tick.
;; EXAMPLES :
;;(racket-after-tick
;; (make-racket 30
;;              30
;;              5
;;              5
;;              false
;;              INITIAL-SELECTOR-X
;;              INITIAL-SELECTOR-Y
;;              INITIAL-VELOCITY-X
;;              INITIAL-VELOCITY-Y)
;; (list BALL-ABOVE-RACKET)
;; false )
;;=> (make-racket 35
;;                35
;;                5
;;                5
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;;(racket-after-tick
;; RACKET-COLLISION-SIDE-RIGHT-WALL
;; (list BALL-ABOVE-RACKET) false )
;;=> (make-racket 402
;;                3
;;                0
;;                0
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;;(racket-after-tick
;; (make-racket 30
;;              30
;;              5
;;              5
;;              false
;;              INITIAL-SELECTOR-X
;;              INITIAL-SELECTOR-Y
;;              INITIAL-VELOCITY-X
;;              INITIAL-VELOCITY-Y)
;; (list BALL-ABOVE-RACKET)
;; true)
;;=> (make-racket 30
;;                30
;;                5
;;                5
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;; DESIGN STRATEGY : conditions on paused?, combining simpler functions.

(define (racket-after-tick r bl paused?)
  (if paused? r
      (racket-collides-with-side-wall
       (racket-collides-with-bottom-wall
        (tentative-racket (racket-collision-with-ball r bl))))))


(begin-for-test
  (check-equal?
   (racket-after-tick
    (make-racket 30
                 30
                 5
                 5
                 false
                 INITIAL-SELECTOR-X
                 INITIAL-SELECTOR-Y
                 INITIAL-VELOCITY-X
                 INITIAL-VELOCITY-Y)
    (list BALL-ABOVE-RACKET)
    false )
   
   (make-racket 35
                35
                5
                5
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "invalid state of racket returned.")
  
  (check-equal?
   (racket-after-tick  RACKET-COLLISION-SIDE-RIGHT-WALL
                       (list BALL-ABOVE-RACKET)
                       false )
   (make-racket 402
                3
                0
                0
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "invalid state of racket returned.")
  
  (check-equal?
   (racket-after-tick
    (make-racket 30
                 30
                 5
                 5
                 false
                 INITIAL-SELECTOR-X
                 INITIAL-SELECTOR-Y
                 INITIAL-VELOCITY-X
                 INITIAL-VELOCITY-Y)
    (list BALL-ABOVE-RACKET)
    true)
   (make-racket 30
                30
                5
                5
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "invalid state of racket returned."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-paused-state-wait : World -> World
;; GIVEN : a world w in paused state.
;; RETURNS : a world like w where paused-counter is decremented.
;;        When the pause counter reaches 0 the world is reset to initial state.
;;EXAMPLE :
;;(world-paused-state-wait PAUSED-WORLD ) => INITIAL-WORLD 
;;(world-paused-state-wait INITIAL-WORLD) => PAUSED-WORLD1
;; DESIGN STRATEGY : use constructor template for world.

(define (world-paused-state-wait w)
  (if (> (world-paused-counter w) 1)
  
      (make-world
       (world-balls w)
       (world-racket w)
       (world-court-color w)
       (world-speed w)
       (world-paused? w)
       (- (world-paused-counter w) 1)
       (world-ready-to-serve? w))
  
      (initial-world (world-speed w))))

(begin-for-test
  (check-equal? (world-paused-state-wait PAUSED-WORLD ) INITIAL-WORLD
                "paused counter not working.")
  (check-equal? (world-paused-state-wait INITIAL-WORLD) PAUSED-WORLD1
                "paused counter not working."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-check-paused : World -> World
;; GIVEN : a world.
;; RETURNS : the state of the world at the current tick.
;; EXAMPLES :
;;(world-check-paused INITIAL-WORLD) => INITIAL-WORLD 
;;(world-check-paused PAUSED-WORLD ) => INITIAL-WORLD  
;; DESIGN STRATEGY : conditions on world-paused?

(define (world-check-paused w)
  (if (world-paused? w)
      
      (world-paused-state-wait w)
      
      (make-world
       (ball-after-tick (world-balls w) (world-racket w) (world-paused? w))
       (racket-after-tick (world-racket w)
                          (world-balls w)
                          (world-paused? w))
       (world-court-color w)
       (world-speed w)
       (world-paused? w)
       (world-paused-counter w)
       (world-ready-to-serve? w))))


(begin-for-test
  (check-equal? (world-check-paused INITIAL-WORLD) INITIAL-WORLD
                "paused counter not working.")
  (check-equal? (world-check-paused PAUSED-WORLD ) INITIAL-WORLD
                "paused counter not working."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; last-ball-collides-with-bottom-wall? : BallList -> Boolean
;; GIVEN : a list of Balls.
;; RETURNS : true, if there is only one ball and the ball collides with the
;; bottom wall
;; EXAMPLE:
;;(last-ball-collides-with-bottom-wall? (list INITIAL-BALL INITIAL-BALL))
;;=> false 
;;(last-ball-collides-with-bottom-wall?
;; (list (make-ball COURT-WIDTH COURT-HEIGHT -3 -9)))
;;=> true 
;; DESIGN STRATEGY : combine simpler functions.

(define (last-ball-collides-with-bottom-wall? bl)
  (cond
    [(= (length bl) 1) (ball-collides-with-bottom-wall? (first bl))]
    [else false]))

(begin-for-test
  (check-equal? (last-ball-collides-with-bottom-wall?
                 (list INITIAL-BALL INITIAL-BALL))
                false
                "the list has more than 1 ball hence false.")
  (check-equal? (last-ball-collides-with-bottom-wall?
                 (list (make-ball COURT-WIDTH COURT-HEIGHT -3 -9)))
                true
                "there is only one ball in the list as it has collided with the
bottom wall."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: any world that's possible for the simulation
;; RETURNS: the world that should follow the given world
;;     after a tick
;; EXAMPLES:
;;(world? (world-after-tick INITIAL-WORLD)) => true 
;;(world-after-tick PAUSED-WORLD ) => INITIAL-WORLD 
;;(world-after-tick EXCEPTION-WORLD) => EXCEPTION-WORLD-OUTPUT
;; DESIGN STRATEGY : combine simpler functions. 
  
(define (world-after-tick w)
  (if (and (or (racket-collides-top-wall? (world-racket w))
               (last-ball-collides-with-bottom-wall? (world-balls w)))
           (not (world-paused? w)))
      
      (pause-world (world-check-paused w))
      
      (world-check-paused w)))


(begin-for-test
  (check-equal? (world? (world-after-tick INITIAL-WORLD)) true
                "should return type World")
  (check-equal? (world-after-tick PAUSED-WORLD ) INITIAL-WORLD
                "paused counter not working.")
  (check-equal? (world-after-tick EXCEPTION-WORLD) EXCEPTION-WORLD-OUTPUT
                "World not handling exception cases of ball and racket."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rally-ball : Ball -> Ball
;; GIVEN : a ball
;; RETURNS : the same ball in rally state.
;; EXAMPLES :
;;(rally-ball INITIAL-BALL-LIST)
;;=> (list (make-ball READY-TO-SERVE-X
;;                    READY-TO-SERVE-Y
;;                    RALLY-BALL-VELOCITY-X
;;                    RALLY-BALL-VELOCITY-Y))
;; DESIGN STRATEGY : change ball velocities to match rally velocity.

(define (rally-ball bl)
  (cond
    [(empty? bl) empty]
    [else (cons (make-ball
                 (ball-x (first bl))
                 (ball-y (first bl))
                 RALLY-BALL-VELOCITY-X
                 RALLY-BALL-VELOCITY-Y)
                (rally-ball (rest bl)))]))


(begin-for-test
  (check-equal? (rally-ball INITIAL-BALL-LIST)
                (list (make-ball READY-TO-SERVE-X
                                 READY-TO-SERVE-Y
                                 RALLY-BALL-VELOCITY-X
                                 RALLY-BALL-VELOCITY-Y))
                "ball not in rally state"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pause-world : World -> World.
;; GIVEN : any World.
;; RETURNS : paused version of the same world.
;; EXAMPLE :
;;(pause-world
;; (make-world INITIAL-BALL-LIST INITIAL-RACKET WHITE 1 false 3 true))
;;=> (make-world INITIAL-BALL-LIST INITIAL-RACKET YELLOW 1 true 3 true)
;; DESIGN STRATEGY : use the constructor template for World.

(define (pause-world w)
  (make-world (world-balls w)
              (world-racket w)
              YELLOW
              (world-speed w)
              true
              (world-paused-counter w)
              (world-ready-to-serve? w)))


(begin-for-test
  (check-equal?
   (pause-world
    (make-world INITIAL-BALL-LIST INITIAL-RACKET WHITE 1 false 3 true))
   (make-world INITIAL-BALL-LIST INITIAL-RACKET YELLOW 1 true 3 true)
   "world not in paused state."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-up-key : Racket -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with decreased velocity on the y axis
;; EXAMPLE :
;;(racket-after-up-key INITIAL-RACKET)
;;=> (make-racket 330
;;                384
;;                0
;;                -1
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-after-up-key r )
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (- (racket-vy r) RACKET-SPEED)
               (racket-selected? r)
               (racket-sx r)
               (racket-sy r)
               (racket-pvx r)
               (racket-pvy r)))

(begin-for-test
  (check-equal?
   (racket-after-up-key INITIAL-RACKET)
   (make-racket 330
                384
                0
                -1
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "racket after up key failing."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-down-key : Racket -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with increased velocity on the y axis
;; EXAMPLE :
;;(racket-after-down-key INITIAL-RACKET)
;;=> (make-racket 330
;;                384
;;                0
;;                1
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-after-down-key r )
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (+ (racket-vy r) RACKET-SPEED)
               (racket-selected? r)
               (racket-sx r)
               (racket-sy r)
               (racket-pvx r)
               (racket-pvy r)))

(begin-for-test
  (check-equal?
   (racket-after-down-key INITIAL-RACKET)
   (make-racket 330
                384
                0
                1
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "racket after down key failing."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-left-key : Racket -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with decreased velocity on the x axis
;; EXAMPLE :
;;(racket-after-left-key INITIAL-RACKET)
;;=> (make-racket 330
;;                384
;;                -1
;;                0
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-after-left-key r )
  (make-racket (racket-x r)
               (racket-y r)
               (- (racket-vx r) RACKET-SPEED)
               (racket-vy r)
               (racket-selected? r)
               (racket-sx r)
               (racket-sy r)
               (racket-pvx r)
               (racket-pvy r)))

(begin-for-test
  (check-equal?
   (racket-after-left-key INITIAL-RACKET)
   (make-racket 330
                384
                -1
                0
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "racket after left key failing."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-right-key : Racket -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with increased velocity on the x axis
;; EXAMPLE :
;;(racket-after-right-key INITIAL-RACKET)
;;=> (make-racket 330
;;                384
;;                1
;;                0
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-after-right-key r )
  (make-racket (racket-x r)
               (racket-y r)
               (+ (racket-vx r) RACKET-SPEED)
               (racket-vy r)
               (racket-selected? r)
               (racket-sx r)
               (racket-sy r)
               (racket-pvx r)
               (racket-pvy r)))


(begin-for-test
  (check-equal?
   (racket-after-right-key INITIAL-RACKET)
   (make-racket 330
                384
                1
                0
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "racket after right key failing."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-key : Racket KeyEvent -> Racket
;; GIVEN : a racket
;; RETURNS : a racket with velocities modified based on key pressed.
;; EXAMPLE :
;;(racket-after-key INITIAL-RACKET "up")
;;=> (make-racket 330
;;                384
;;                0
;;                -1
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;;
;;(racket-after-key INITIAL-RACKET "down")
;;=> (make-racket 330
;;                384
;;                0
;;                1
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;;
;;(racket-after-key INITIAL-RACKET "left")
;;=> (make-racket 330
;;                384
;;                -1
;;                0
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;;(racket-after-key INITIAL-RACKET "right")
;;=> (make-racket 330
;;                384
;;                1
;;                0
;;                false
;;                INITIAL-SELECTOR-X
;;                INITIAL-SELECTOR-Y
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;;
;;(racket-after-key INITIAL-RACKET "q") => INITIAL-RACKET 
;; DESIGN STRATEGY : condition on kev, calling simpler functions.

(define (racket-after-key r kev)
  (cond
    [(key=? UP-KEY kev) (racket-after-up-key r)]
    [(key=? DOWN-KEY kev) (racket-after-down-key r)]
    [(key=? LEFT-KEY kev) (racket-after-left-key r)]
    [(key=? RIGHT-KEY kev) (racket-after-right-key r)]
    [else r]))


(begin-for-test
  (check-equal?
   (racket-after-key INITIAL-RACKET "up")
   (make-racket 330
                384
                0
                -1
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "racket after up key failing.")
  
  (check-equal?
   (racket-after-key INITIAL-RACKET "down")
   (make-racket 330
                384
                0
                1
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "racket after down key failing.")
  
  (check-equal?
   (racket-after-key INITIAL-RACKET "left")
   (make-racket 330
                384
                -1
                0
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "racket after left key failing.")
  
  (check-equal?
   (racket-after-key INITIAL-RACKET "right")
   (make-racket 330
                384
                1
                0
                false
                INITIAL-SELECTOR-X
                INITIAL-SELECTOR-Y
                INITIAL-VELOCITY-X
                INITIAL-VELOCITY-Y)
   "racket after right key failing.")
  
  (check-equal?
   (racket-after-key INITIAL-RACKET "q")
   INITIAL-RACKET) "racket responding to invalid input")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balls-after-key : BallList KeyEvent -> BallList
;; GIVEN : a list of balls bl, and a key event.
;; RETURNS : a list of balls like bl, but with a new ball at rally state
;; appended to it if KeyEvent is "b" else the same list bl.
;; EXAMPLES:
;;(balls-after-key empty "b")
;;=> (list (make-ball READY-TO-SERVE-X
;;                    READY-TO-SERVE-Y
;;                    RALLY-BALL-VELOCITY-X
;;                    RALLY-BALL-VELOCITY-Y))
;;
;;(balls-after-key empty "c")
;;=> empty
;; DESIGN STRATEGY : conditions on KeyEvent, use the constructor template
;; for ball

(define (balls-after-key bl kev)
  (if (key=? kev "b")
  (append bl
          (list (make-ball READY-TO-SERVE-X
                           READY-TO-SERVE-Y
                           RALLY-BALL-VELOCITY-X
                           RALLY-BALL-VELOCITY-Y)))
  bl))

(begin-for-test
  (check-equal? (balls-after-key empty "b")
                (list (make-ball READY-TO-SERVE-X
                           READY-TO-SERVE-Y
                           RALLY-BALL-VELOCITY-X
                           RALLY-BALL-VELOCITY-Y))
                "if b is pressed a new ball at rally state must be generated")
  (check-equal? (balls-after-key empty "c")
                empty
                "no new ball must be generated at any other key press."))
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
     (make-world (balls-after-key (world-balls w) kev)
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
      (make-world (rally-ball (world-balls w))
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
    [(key=? SPACE-KEY kev) (world-after-space-key w)]
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
;; ball-list-images : BallList -> ImageList
;; GIVEN : a list of balls bl
;; RETURNS : a list the size of bl, but each element is an Image of ball.
;; EXAMPLE:
;; (length (ball-list-images  INITIAL-BALL-LIST)) => (length INITIAL-BALL-LIST)
;; DESIGN STRATEGY : use HOF map.

(define (ball-list-images bl)
  (map
   ;; Ball -> Image
   ;; GIVEN : a Ball b
   ;; RETURNS : a corresponding image of the Ball.
   (lambda (b) BALL) bl))

(begin-for-test
  (check-equal? (length (ball-list-images  INITIAL-BALL-LIST))
                (length INITIAL-BALL-LIST)
                "the number of images generated of the ball must be equal to the
number of ball is the world-balls list."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ball-list-posns : BallList -> PosnList
;; GIVEN : a list of balls.
;; RETURNS : a list of positions corresponding to each ball.
;; EXAMPLES:
;; (length (ball-list-posns  INITIAL-BALL-LIST)) => (length INITIAL-BALL-LIST)
;; DESIGN STRATEGY : use HOF map.

(define (ball-list-posns bl)
  (map
   ;; Ball -> Posn
   ;; GIVEN : a Ball.
   ;; RETURNS : a Posn corresponding to the x and y coordinates of the Ball.
   (lambda (b) (make-posn (ball-x b)
                           (ball-y b))) bl))

(begin-for-test
  (check-equal? (length (ball-list-posns  INITIAL-BALL-LIST))
                (length INITIAL-BALL-LIST)
                "the number of posns generated of the ball must be equal to the
number of ball is the world-balls list."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw-world : World -> Scene.
;; GIVEN: a world.
;; RETURNS : a scene which describes the world provided.
;; EXAMPLE:
;; (image? (draw-world INITIAL-WORLD)) => true
;; DESIGN STRATEGY : combine simpler functions.


(define (draw-world w)
  (place-image SELECTOR
               (racket-sx (world-racket w))
               (racket-sy (world-racket w))
               (place-image RACKET
                            (racket-x (world-racket w))
                            (racket-y (world-racket w))
                            (place-images (ball-list-images (world-balls w))
                                          (ball-list-posns (world-balls w))
                            (empty-scene COURT-WIDTH
                                         COURT-HEIGHT
                                         (world-court-color w))))))
(begin-for-test
  (check-equal? (image? (draw-world INITIAL-WORLD)) true
                "should return type image"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-within-range? : Racket Int Int -> Boolean
;; GIVEN : a racket, the x and y co-ordinates of a mouse in pixels.
;; WHERE : the racket is selected if the mouse is 25 pixels or closer to
;;         the center of the racket.
;; RETURNS : true if the  racket is selected else false
;; EXAMPLE :
;;(racket-within-range? INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y)
;;    => true
;;(racket-within-range? INITIAL-RACKET (+ READY-TO-SERVE-X 25) READY-TO-SERVE-Y)
;;    => true
;;(racket-within-range? INITIAL-RACKET (+ READY-TO-SERVE-X 26) READY-TO-SERVE-Y)
;;    => false
;; DESIGN STRATEGY : Transcribe formula.

(define (racket-within-range? r x y)
  (<=
   (sqrt (+
          (expt (- (racket-x r) x) 2)
          (expt (- (racket-y r) y) 2)))
   RACKET-RANGE))

(begin-for-test
  (check-equal?(racket-within-range?
                INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y)
               true "invalid output racket in range")
  (check-equal? (racket-within-range?
                 INITIAL-RACKET (+ READY-TO-SERVE-X 25) READY-TO-SERVE-Y)
                true "invalid output racket in range")
  (check-equal? (racket-within-range?
                 INITIAL-RACKET (+ READY-TO-SERVE-X 26) READY-TO-SERVE-Y)
                false "invalid output racket not in range"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; select-racket : Racket Int Int -> Racket
;; GIVEN : a Racket r ,that is not selected, x and y coordinates of the
;;         mouse in pixels.
;; RETURNS : a Racket like r but selected.
;; EXAMPLE:
;;(racket-selected?
;; (select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y)) => true 
;;(racket-vx INITIAL-RACKET)
;;=> (racket-pvx
;;(select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
;;(racket-vy INITIAL-RACKET)
;;=> (racket-pvy
;;(select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
;;(racket-vx (select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
;;=> INITIAL-VELOCITY-X 
;;(racket-vy (select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
;;=> INITIAL-VELOCITY-Y 

;; DESIGN STRATEGY : use the constructor template for Racket.

(define (select-racket r x y)
  (make-racket (racket-x r)
               (racket-y r)
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y
               true
               x
               y
               (racket-vx r)
               (racket-vy r)))


(begin-for-test
  (check-equal?
   (racket-selected?
    (select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
   true
   "racket not selected")
  
  (check-eq? (racket-vx INITIAL-RACKET)
             (racket-pvx
              (select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
             "pvx of the selected racket must be equal to vx of original")

  (check-eq? (racket-vy INITIAL-RACKET)
             (racket-pvy
              (select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
             "pvy of the selected racket must be equal to vy of original")
  
  (check-eq? (racket-vx
              (select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
              INITIAL-VELOCITY-X
             "vx of the selected racket must be equal to INITIAL-VELOCITY-X")
  
  (check-eq? (racket-vy
              (select-racket INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
             INITIAL-VELOCITY-Y
             "vy of the selected racket must be equal to INITIAL-VELOCITY-Y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unselect-racket : Racket -> Racket
;; GIVEN : a Racket r, that is selected.
;; RETURNS : a Racket like r but unselected.
;; EXAMPLE:
;;(racket-selected?
;; (unselect-racket
;;  (make-racket 330
;;               384
;;               INITIAL-VELOCITY-X
;;               INITIAL-VELOCITY-Y
;;               true
;;               INITIAL-SELECTOR-X
;;               INITIAL-SELECTOR-Y
;;               INITIAL-VELOCITY-X
;;               INITIAL-VELOCITY-Y))) => false 
;;(racket-pvx INITIAL-RACKET)
;;=> (racket-vx (unselect-racket INITIAL-RACKET))
;;
;;(racket-pvy INITIAL-RACKET)
;;=> (racket-vy (unselect-racket INITIAL-RACKET))

;; DESIGN STRATEGY: use the constructor template for Racket.

(define (unselect-racket r)
  (make-racket (racket-x r)
               (racket-y r)
               (racket-pvx r)
               (racket-pvy r)
               false
               INITIAL-SELECTOR-X
               INITIAL-SELECTOR-Y
               INITIAL-VELOCITY-X
               INITIAL-VELOCITY-Y))


(begin-for-test
  (check-equal?
   (racket-selected?
    (unselect-racket
     (make-racket 330
                  384
                  INITIAL-VELOCITY-X
                  INITIAL-VELOCITY-Y
                  true
                  INITIAL-SELECTOR-X
                  INITIAL-SELECTOR-Y
                  INITIAL-VELOCITY-X
                  INITIAL-VELOCITY-Y)))
   false "racket not selected")
  (check-equal? (racket-pvx INITIAL-RACKET)
                (racket-vx (unselect-racket INITIAL-RACKET))
                "pvx of original racket must become vx of racket.")
  
  (check-equal? (racket-pvy INITIAL-RACKET)
                (racket-vy (unselect-racket INITIAL-RACKET))
                "pvy of original racket must become vy of racket."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; button-down-handler : Racket Int Int -> World
;; GIVEN :  a Racket r, the x and y co-ordinates of the mouse.
;; RETURNS : a Racket like r, after all button down events are applied to it
;; EXAMPLE:
;;(racket-selected?
;;    (button-down-handler INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
;;   => true
;;
;;(racket-selected?
;;    (button-down-handler INITIAL-RACKET READY-TO-SERVE-X 10))
;;   => false
;;
;; DESIGN STRATEGY: combine simpler functions.

(define (button-down-handler r x y)
  (if (racket-within-range? r x y)
      (select-racket r x y)
      r))
  
(begin-for-test
  (check-equal?
   (racket-selected?
    (button-down-handler INITIAL-RACKET READY-TO-SERVE-X READY-TO-SERVE-Y))
   true "racket should be selected.")
  
  (check-equal?
   (racket-selected?
    (button-down-handler INITIAL-RACKET READY-TO-SERVE-X 10))
   false "racket must not be selected."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; button-up-handler : Racket -> World
;; GIVEN   : a Racket r
;; RETURNS : a Racket like r, after all the button up events are applied to it.
;; EXAMPLES:
;; (racket-selected? (button-up-handler SELECTED-RACKET))
;;    => false
;;   
;; DESIGN STRATEGY : combine simpler functions.

(define (button-up-handler r)
  (if (racket-selected? r)
      (unselect-racket r)
      r))

(begin-for-test
  (check-equal? (racket-selected?(button-up-handler SELECTED-RACKET))
                false) "racket must be unselected.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delta : Int Int -> Int
;; GIVEN : either the x or y coordinate of the mouse and the corresponding
;;         co-ordinate of the selector
;; RETURNS : the difference between the two.
;; EXAMPLE:
;; (delta 10 5) => 5
;; DESIGN STRATEGY : Transcribe formula.

(define (delta m s)
  (- m s))

(begin-for-test
  (check-equal? (delta 10 5) 5) "delta must return change in values.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; calculate-change : Int Int Int -> Int
;; GIVEN : either the x or y co-coordinate of the mouse, the same co-ordinate
;;         of the racket and the previous co-ordinate of the mouse.
;; RETURNS : the new x or y co-ordinate of racket.
;; EXAMPLE :
;; (calculate-change 0 0 1) => -1
;; DESIGN STRATEGY : combine simpler functions.

(define (calculate-change x rx sx)
  (+ x (delta rx sx)))

(begin-for-test
  (check-equal? (calculate-change 0 0 1) -1) "must return detla + co-ordinate")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; drag-handler : Racket Int Int -> Racket
;; GIVEN : a Racket r, x and y co-ordinates of the mouse.
;; RETURNS : a Racket like r where the drag event has been applied.
;; WHERE : only if the racket is selected the drag event is applied.
;; EXAMPLE :
;;(drag-handler INITIAL-RACKET 10 10) => INITIAL-RACKET 
;;(drag-handler SELECTED-RACKET 0 0)
;;=> (make-racket (- INITIAL-SELECTOR-X)
;;                (- INITIAL-SELECTOR-Y)
;;                0
;;                0
;;                true
;;                0
;;                0
;;                INITIAL-VELOCITY-X
;;                INITIAL-VELOCITY-Y)
;; DESIGN STRATEGY : use constructor template for Racket.

(define (drag-handler r x y)
  (if (racket-selected? r)
      (make-racket (calculate-change x (racket-x r) (racket-sx r))
                   (calculate-change y (racket-y r) (racket-sy r))
                   (racket-vx r)
                   (racket-vy r)
                   (racket-selected? r)
                   x
                   y
                   (racket-pvx r)
                   (racket-pvy r))
      r))

(begin-for-test
  (check-equal? (drag-handler INITIAL-RACKET 10 10) INITIAL-RACKET
                "racket should not respond to drag event")
  (check-equal? (drag-handler SELECTED-RACKET 0 0)
                (make-racket (- INITIAL-SELECTOR-X)
                             (- INITIAL-SELECTOR-Y)
                             0
                             0
                             true
                             0
                             0
                             INITIAL-VELOCITY-X
                             INITIAL-VELOCITY-Y))
  "racket should respond to drag event")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Int Int MouseEvent -> World
;; GIVEN: a world, the x and y coordinates of a mouse event in pixels,
;;     and the mouse event
;; RETURNS: the world that should follow the given world after
;;     the given mouse event
;; EXAMPLE :
;; (world? (world-after-mouse-event RALLY-WORLD 10 10 "button-down")) => true
;; (world? (world-after-mouse-event INITIAL-WORLD 10 10 "button-up")) => true
;; DESIGN STRATEGY : use constructor template for World.

(define (world-after-mouse-event w x y mev)
  (if (or (world-paused? w) (world-ready-to-serve? w))
      w
      (make-world (world-balls w)
                  (racket-after-mouse-event (world-racket w) x y mev)
                  (world-court-color w)
                  (world-speed w)
                  (world-paused? w)
                  (world-paused-counter w)
                  (world-ready-to-serve? w))))

(begin-for-test
  (check-equal?
   (world? (world-after-mouse-event RALLY-WORLD 10 10 "button-down")) true
   "should be of type World")
  (check-equal?
   (world? (world-after-mouse-event INITIAL-WORLD 10 10 "button-up")) true
   "should be of type World"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-mouse-event : Racket Int Int MouseEvent -> Racket
;; GIVEN: a racket, the x and y coordinates of a mouse event,
;;     and the mouse event
;; RETURNS: the racket as it should be after the given mouse event
;; EXAMPLES:
;;(racket-after-mouse-event  INITIAL-RACKET 10 10 "enter")
;;    => INITIAL-RACKET
;;
;;(racket-selected?
;; (racket-after-mouse-event UNSELECTED-RACKET 10 10 "button-up"))
;;    => false
;;
;;(racket-selected?
;; (racket-after-mouse-event UNSELECTED-RACKET 10 10 "button-down"))
;;    => true
;;
;;(racket-after-mouse-event  INITIAL-RACKET 10 10 "drag")
;;    => INITIAL-RACKET
;; DESIGN STRATEGY : conditions on mev.

(define (racket-after-mouse-event r x y mev)
  (cond
    [(mouse=? mev "button-down") (button-down-handler r x y)]
    [(mouse=? mev "button-up") (button-up-handler r)]
    [(mouse=? mev "drag") (drag-handler r x y)]
    [else r]))

(begin-for-test
  (check-equal? (racket-after-mouse-event  INITIAL-RACKET 10 10 "enter")
                INITIAL-RACKET "racket should not be affected.")
  
  (check-equal? (racket-selected?
                 (racket-after-mouse-event UNSELECTED-RACKET 10 10 "button-up"))
                false "racket should not be selected.")
  
  (check-equal?
   (racket-selected?
    (racket-after-mouse-event UNSELECTED-RACKET 10 10 "button-down"))
   true "racket should be selected")
  
  (check-equal? (racket-after-mouse-event  INITIAL-RACKET 10 10 "drag")
                INITIAL-RACKET) "racket should not respond to drag.")


