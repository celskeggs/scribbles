#lang racket
(require "utils.rkt")
(require "composition.rkt")
(require "functional-graphics.rkt")
(require "vector.rkt")

(r:define-style simple-style "black" 6 'solid "white" 'solid)

(compose
 (root collar 250 250)
 (bone pelvis collar (v 0 100))
 (bone head collar (v 0 -30) (r:circle head)))

#|
(define-syntax-rule (bone variant invariant length)
  (begin
    (fixed-distance variant invariant length)
    (r:line variant invariant)))

(define-syntax-rule (limb end joint base length-upper length-lower)
  (r:all
   (bone joint base length-upper)
   (bone end joint length-lower)))

(composer example ((head 150 0) (collar 150 125) (pelvis 150 500) (left-hand 0 500) (left-elbow 0 500) (right-hand 300 500) (right-elbow 300 500) (left-leg 0 500) (right-leg 300 500))
          (let* ((body-height 100) ; TODO: variable
                 (head-radius (round (/ body-height 2)))
                 (arm-length body-height)
                 (upper-arm-length (/ arm-length 2))
                 (lower-arm-length (/ arm-length 2))
                 (neck-height (round (/ body-height 5)))
                 (leg-length (+ body-height neck-height)))
            (simple-style
             (bone head collar (+ head-radius neck-height))
             (bone pelvis collar body-height)
             (limb left-hand left-elbow collar upper-arm-length lower-arm-length)
             (limb right-hand right-elbow collar upper-arm-length lower-arm-length)
             (bone left-leg pelvis leg-length)
             (bone right-leg pelvis leg-length)
             (r:circle head head-radius))))
(example 400 500)
|#
