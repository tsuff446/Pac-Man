;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pacman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct pac-world [pacman blinky pinky inky clyde balls last-faced])

;balls and map are list of posn
; next step is to put sprite inside of object struct so collide with wall can look at size 
(define-struct object [posn vel sprite])
(define-struct vel [dx dy])

(define SCREEN-WIDTH 608)
(define SCREEN-HEIGHT 640)
(define BLOCKSIZE 32)
(define OBJECTSIZE 32)
(define PACSPEED 4)
(define LEFT (make-vel (* -1 PACSPEED) 0))
(define RIGHT (make-vel PACSPEED 0))
(define UP (make-vel 0 (* -1 PACSPEED)))
(define DOWN (make-vel 0 PACSPEED))

(define BACKGROUND (rectangle SCREEN-WIDTH SCREEN-HEIGHT "solid" "black"))
(define block (square BLOCKSIZE "solid" "blue"))
(define pac-sprite (circle 14 "solid" "yellow"))
(define blinky-sprite (square 32 "solid" "red"))
(define inky-sprite (square 32 "solid" "cyan"))
(define pinky-sprite (square 32 "solid" "pink"))
(define clyde-sprite (square 32 "solid" "orange"))
(define mid-maze (list
                                                                                                                                                              (make-posn 304 48)
                                    (make-posn 80 80) (make-posn 112 80)               (make-posn 176 80)(make-posn 208 80)(make-posn 240 80)                 (make-posn 304 80)             (make-posn 368 80) (make-posn 400 80)(make-posn 432 80)             (make-posn 496 80)(make-posn 528 80)
                                    (make-posn 80 112)(make-posn 112 112)              (make-posn 176 112)(make-posn 208 112)(make-posn 240 112)              (make-posn 304 112)            (make-posn 368 112) (make-posn 400 112)(make-posn 432 112)           (make-posn 496 112)(make-posn 528 112)

                                    (make-posn 80 176)(make-posn 112 176)              (make-posn 176 176)              (make-posn 240 176)(make-posn 272 176)(make-posn 304 176)(make-posn 336 176) (make-posn 368 176)             (make-posn 432 176)           (make-posn 496 176) (make-posn 528 176)                             
                                                                                       (make-posn 176 208)                                                    (make-posn 304 208)                                                   (make-posn 432 208)
                  (make-posn 48 240)(make-posn 80 240)(make-posn 112 240)              (make-posn 176 240)(make-posn 208 240)(make-posn 240 240)              (make-posn 304 240)             (make-posn 368 240)(make-posn 400 240)(make-posn 432 240)           (make-posn  496 240)(make-posn 528 240) (make-posn 560 240)
                  (make-posn 48 272)(make-posn 80 272)(make-posn 112 272)              (make-posn 176 272)                                                                                                                           (make-posn 432 272)          (make-posn 496 272)(make-posn 528 272)(make-posn 560 272)
                  (make-posn 48 304)(make-posn 80 304)(make-posn 112 304)              (make-posn 176 304)              (make-posn 240 304)(make-posn 272 304)(make-posn 304 304)(make-posn 336 304)(make-posn 368 304)                (make-posn 432 304)           (make-posn 496 304)(make-posn 528 304)(make-posn 560 304)
                                                                                                                        (make-posn 240 336)(make-posn 272 336)(make-posn 304 336)(make-posn 336 336)(make-posn 368 336)
                  (make-posn 48 368)(make-posn 80 368)(make-posn 112 368)              (make-posn 176 368)              (make-posn 240 368)(make-posn 272 368)(make-posn 304 368)(make-posn 336 368)(make-posn 368 368)             (make-posn 432 368)            (make-posn 496 368)(make-posn 528 368)(make-posn 560 368)    
                  (make-posn 48 400)(make-posn 80 400)(make-posn 112 400)              (make-posn 176 400)                                                                                                                           (make-posn 432 400)            (make-posn 496 400)(make-posn 528 400)(make-posn 560 400)
                  (make-posn 48 432)(make-posn 80 432)(make-posn 112 432)              (make-posn 176 432)              (make-posn 240 432)(make-posn 272 432)(make-posn 304 432)(make-posn 336 432)(make-posn 368 432)             (make-posn 432 432)            (make-posn 496 432) (make-posn 528 432)(make-posn 560 432)
                  ))
(define PACMAN0 (make-object (make-posn 336 240) (make-vel 0 0) pac-sprite))
(define BLINKY0 (make-object (make-posn 304 272) (make-vel 4 0) blinky-sprite))
(define INKY0 (make-object (make-posn 304 272) (make-vel 0 0) inky-sprite))
(define PINKY0 (make-object (make-posn 304 272) (make-vel 0 0) pinky-sprite))
(define CLYDE0 (make-object (make-posn 304 272) (make-vel 0 0) clyde-sprite))

(define WORLD0 (make-pac-world PACMAN0 BLINKY0 PINKY0 INKY0 CLYDE0 '() LEFT))


(define (add-vertical-border i j)
  (cond
    [(< j 0) '()]
    [(< i 0) (add-vertical-border (- SCREEN-WIDTH (/ BLOCKSIZE 2)) (- j BLOCKSIZE))]
    [else (if (not (= j 336))
     (cons (make-posn i j) (add-vertical-border (- i (- SCREEN-WIDTH BLOCKSIZE)) j))
     (add-vertical-border (- i (- SCREEN-WIDTH BLOCKSIZE)) j))]
    )
  )
(define (add-horizontal-border i j)
  (cond
    [(< i 0) '()]
    [(< j 0) (add-horizontal-border (- i BLOCKSIZE) (- SCREEN-HEIGHT (/ BLOCKSIZE 2)))]
    [else (cons (make-posn i j) (add-horizontal-border i (- j (- SCREEN-HEIGHT BLOCKSIZE))))]
    )
  )

(define pac-map (append (add-vertical-border (- SCREEN-WIDTH (/ BLOCKSIZE 2)) (- SCREEN-HEIGHT (/ BLOCKSIZE 2)))
                        (add-horizontal-border (- SCREEN-WIDTH (/ BLOCKSIZE 2)) (- SCREEN-HEIGHT (/ BLOCKSIZE 2)))
                        mid-maze))

  
(define (render-world world)
               (place-image pac-sprite (posn-x (object-posn (pac-world-pacman world)))
               (posn-y (object-posn (pac-world-pacman world)))
               (render-ghosts world (render-blocks pac-map)))
                          
  )

(define (render-blocks LoP)
  (cond
    [(empty? LoP) BACKGROUND]
    [else
     (place-image block (posn-x (first LoP)) (posn-y (first LoP)) (render-blocks (rest LoP)))]
    )
  )
(define (render-targets world background)
    (place-image (circle 10 "solid" "cyan")
               (posn-x (inky-target world))
               (posn-y (inky-target world))
               (place-image (circle 10 "solid" "orange")
                            (posn-x (clyde-target world))
                            (posn-y (clyde-target world))
                            (place-image (circle 10 "solid" "pink")
                                         (posn-x (pinky-target world))
                                         (posn-y (pinky-target world))
                                         background)))
                                         
  )

(define (render-ghosts world background)
  (place-image blinky-sprite
               (posn-x (object-posn (pac-world-blinky world)))
               (posn-y (object-posn (pac-world-blinky world)))
               (place-image inky-sprite
                            (posn-x (object-posn (pac-world-inky world)))
                            (posn-y (object-posn (pac-world-inky world)))
                            (place-image pinky-sprite
                                         (posn-x (object-posn (pac-world-pinky world)))
                                         (posn-y (object-posn (pac-world-pinky world)))
                                         (place-image clyde-sprite
                                                      (posn-x (object-posn (pac-world-clyde world)))
                                                      (posn-y (object-posn (pac-world-clyde world)))
                                                      (render-targets world background)))))
                
               
  )

; will-collide-wall : Object Posn -> Boolean
(define (will-collide-wall? obj wall)
  (and (and (< (- (+ (posn-x (object-posn obj)) (vel-dx (object-vel obj))) (/ (image-height (object-sprite obj)) 2)) (+ (posn-x wall) (/ BLOCKSIZE 2)))
            (> (+ (posn-x (object-posn obj)) (vel-dx (object-vel obj)) (/ (image-height (object-sprite obj)) 2)) (- (posn-x wall) (/ BLOCKSIZE 2))))
       (and (< (- (+ (posn-y (object-posn obj)) (vel-dy (object-vel obj))) (/ (image-height (object-sprite obj)) 2)) (+ (posn-y wall) (/ BLOCKSIZE 2)))
            (> (+ (posn-y (object-posn obj)) (vel-dy (object-vel obj)) (/ (image-height (object-sprite obj)) 2)) (- (posn-y wall) (/ BLOCKSIZE 2)))))
  )
; can-move? : Object -> Boolean
(define (can-move? obj)
  (andmap (lambda (x) (not (will-collide-wall? obj x))) pac-map)
  )
; change-velocity : Object Vel -> Object
(define (change-velocity obj vel)
  (make-object
      (object-posn obj)
    vel
    (object-sprite obj))
  )
; update-position : Object -> Object
(define (update-position obj)
  (make-object
      (make-posn
       (+ (posn-x (object-posn obj)) (vel-dx (object-vel obj)))
       (+ (posn-y (object-posn obj)) (vel-dy (object-vel obj))))
    (object-vel obj)
    (object-sprite obj))
  )

; handle-keyboard : PacWorld KeyEvent -> World
(define (handle-keyboard world a-key)
  (cond
    [(string=? a-key "left")
     (if (can-move? (change-velocity (pac-world-pacman world) LEFT))
         (make-pac-world
          (change-velocity (pac-world-pacman world) LEFT)
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          LEFT)
         (make-pac-world
          (pac-world-pacman world)
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          LEFT))]
    [(string=? a-key "right")
     (if (can-move? (change-velocity (pac-world-pacman world) RIGHT))
         (make-pac-world
          (change-velocity (pac-world-pacman world) RIGHT)
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          RIGHT)
         (make-pac-world
          (pac-world-pacman world)
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          RIGHT))]
    [(string=? a-key "up")
     (if (can-move? (change-velocity (pac-world-pacman world) UP))
         (make-pac-world
          (change-velocity (pac-world-pacman world) UP)
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          UP)
         (make-pac-world
          (pac-world-pacman world)
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          UP))]
    [(string=? a-key "down")
     (if (can-move? (change-velocity (pac-world-pacman world) DOWN))
         (make-pac-world
          (change-velocity (pac-world-pacman world) (make-vel 0 PACSPEED))
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          DOWN)
         (make-pac-world
          (pac-world-pacman world)
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          DOWN))]
    [else
     world]
    )                  
  )

; handle-keyboard-release : PacWorld KeyEvent -> PacWorld
(define (handle-keyboard-release world a-key)
  (cond
    [(string=? a-key "left")
     (if (< (vel-dx (object-vel (pac-world-pacman world))) 0)
         (make-pac-world
          (change-velocity (pac-world-pacman world) (make-vel 0 0))
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          (pac-world-last-faced world))
         world)]
    [(string=? a-key "right")
     (if (> (vel-dx (object-vel (pac-world-pacman world))) 0)
         (make-pac-world
          (change-velocity (pac-world-pacman world) (make-vel 0 0))
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          (pac-world-last-faced world))
         world)]
    [(string=? a-key "up")
     (if (< (vel-dy (object-vel (pac-world-pacman world))) 0)
         (make-pac-world
          (change-velocity (pac-world-pacman world) (make-vel 0 0))
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          (pac-world-last-faced world))
         world)]
    [(string=? a-key "down")
     (if (> (vel-dy (object-vel (pac-world-pacman world))) 0)
         (make-pac-world
          (change-velocity (pac-world-pacman world) (make-vel 0 0))
          (pac-world-blinky world)
          (pac-world-pinky world)
          (pac-world-inky world)
          (pac-world-clyde world)
          '()
          (pac-world-last-faced world))
         world)]
    [else world]
    )                  
  )
; update-world : PacWorld -> PacWorld
(define (update-world world)
  (make-pac-world
   (update-object (pac-world-pacman world))
   (update-object (choose-path (pac-world-blinky world) (object-posn (pac-world-pacman world))))
   (update-object (choose-path (pac-world-pinky world) (pinky-target world)))

   (update-object (choose-path (pac-world-inky world) (inky-target world)))

   (update-object (choose-path (pac-world-clyde world) (clyde-target world)))
   '()
   (pac-world-last-faced world))

  )
; possible-moves : Object -> ListOfObjects
(define (possible-moves obj)
  (filter (lambda (x) (not (equal? (make-vel (* -1 (vel-dx (object-vel obj)))
                                             (* -1 (vel-dy (object-vel obj))))
                                   (object-vel x)))) 
                          (filter can-move?
                          (list (change-velocity obj LEFT)
                          (change-velocity obj RIGHT)
                          (change-velocity obj UP)
                          (change-velocity obj DOWN))))
          )
; distance : Posn Posn -> Number
(define (distance posn1 posn2)
  (sqrt (+ (sqr (- (posn-x posn1) (posn-x posn2)))
           (sqr (- (posn-y posn1) (posn-y posn2)))))
  )

; choose-path : Object Posn -> Object
(define (choose-path obj target)
  (local
    [(define MINDIST (foldr min 100000 (map (lambda (x) (distance (object-posn (update-object x)) target)) (possible-moves obj))))
    (define OPTION (filter (lambda (x) (= MINDIST (distance target (object-posn (update-object x))))) (possible-moves obj)))]
  (cond
    [(empty? OPTION) obj]
    [else (first OPTION)]
  )
    )
  )

; pinky-target : PacWorld -> Posn
(define (pinky-target world)
  (cond
    [(equal? (pac-world-last-faced world) LEFT) (make-posn
                                (- (posn-x (object-posn (pac-world-pacman world))) (* 16 PACSPEED))
                                (posn-y (object-posn (pac-world-pacman world))))]
    [(equal? (pac-world-last-faced world) RIGHT) (make-posn
                                (+ (posn-x (object-posn (pac-world-pacman world))) (* 16 PACSPEED))
                                (posn-y (object-posn (pac-world-pacman world))))]
    [(equal? (pac-world-last-faced world) UP) (make-posn
                              (posn-x (object-posn (pac-world-pacman world)))
                               (- (posn-y (object-posn (pac-world-pacman world))) (* 16 PACSPEED)))]
    [(equal? (pac-world-last-faced world) DOWN) (make-posn
                             (posn-x (object-posn (pac-world-pacman world)))
                             (+ (posn-y (object-posn (pac-world-pacman world))) (* 16 PACSPEED)))]
    [else (object-posn (pac-world-pacman world))]
    )
  )

; inky-target : PacWorld -> Posn
(define (inky-target world)
  (add-posns
   (posns-to-vector (inky-direction world) (object-posn (pac-world-blinky world)))
   (inky-direction world))         
  )

; inky-direction : PacWorld -> Posn
(define (inky-direction world)
  (cond
    [(equal? (pac-world-last-faced world) LEFT) (make-posn
                                (- (posn-x (object-posn (pac-world-pacman world))) (* 8 PACSPEED))
                                (posn-y (object-posn (pac-world-pacman world))))]
    [(equal? (pac-world-last-faced world) RIGHT) (make-posn
                                (+ (posn-x (object-posn (pac-world-pacman world))) (* 8 PACSPEED))
                                (posn-y (object-posn (pac-world-pacman world))))]
    [(equal? (pac-world-last-faced world) UP) (make-posn
                              (posn-x (object-posn (pac-world-pacman world)))
                               (- (posn-y (object-posn (pac-world-pacman world))) (* 8 PACSPEED)))]
    [(equal? (pac-world-last-faced world) DOWN) (make-posn
                             (posn-x (object-posn (pac-world-pacman world)))
                             (+ (posn-y (object-posn (pac-world-pacman world))) (* 8 PACSPEED)))]
    [else (object-posn (pac-world-pacman world))]
    )
  )

; posns-to-vector : Posn Posn -> Posn
(define (posns-to-vector posn1 posn2)
  (make-posn
   (- (posn-x posn1) (posn-x posn2))
   (- (posn-y posn1) (posn-y posn2)))
  )

; add-posns : Posn Posn -> Posn
(define (add-posns posn1 posn2)
  (make-posn
   (+ (posn-x posn1) (posn-x posn2))
   (+ (posn-y posn1) (posn-y posn2)))
  )
; clyde-target : PacWorld -> Posn
(define (clyde-target world)
  (cond
    [(> (distance (object-posn (pac-world-pacman world)) (object-posn (pac-world-clyde world))) (* 32 PACSPEED))
     (object-posn (pac-world-pacman world))]
    [else
     (make-posn 0 SCREEN-HEIGHT)]
     
  ))

; update-object : Object -> Object
(define (update-object obj)
  (if (can-move? obj)
      (cond
      [(< (posn-x (object-posn obj)) 0) (make-object
                                            (make-posn
                                             SCREEN-WIDTH
                                             (posn-y (object-posn obj)))
                                          (object-vel obj)
                                          (object-sprite obj))]
      [(> (posn-x (object-posn obj)) SCREEN-WIDTH) (make-object
                                            (make-posn
                                             0
                                             (posn-y (object-posn obj)))
                                          (object-vel obj)
                                          (object-sprite obj))]
      [else (update-position obj)]
      )
      obj)
  )

(define (run world)
  (big-bang world
    [to-draw render-world]
    [on-key handle-keyboard]
    [on-release handle-keyboard-release]
    [on-tick update-world]
    )
  )
(render-world WORLD0)