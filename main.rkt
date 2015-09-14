#lang typed/racket
(require "utils.rkt")
(require "composition.rkt")
(require "functional-graphics.rkt")
(require "vector.rkt")

(require "segments.rkt") ; temporary

(r:define-style simple-style "black" 6 'solid "white" 'solid)

(define-syntax-rule (line-bone variant invariant dx dy extra-renders ...)
  (bone variant invariant (v dx dy) (r:line variant invariant) extra-renders ...))
(define-syntax line-bone-chain
  (syntax-rules ()
    [(line-bone-chain invariant)
     (void)]
    [(line-bone-chain (variant dx dy extra-renders ...) invariant)
     (line-bone variant invariant dx dy extra-renders ...)]
    [(line-bone-chain (variant dx dy extra-renders ...) (variant2 dx2 dy2 extra-renders2 ...) rest ...)
     (begin
       (line-bone-chain (variant2 dx2 dy2 extra-renders2 ...) rest ...)
       (line-bone variant variant2 dx dy extra-renders ...))]))

(compose-segs simple-style
 (construct
  (root collar 250 250)
  (line-bone pelvis collar 0 100)
  (line-bone head collar 0 -70 (r:circle head 50))
  (line-bone-chain (left-hand -50 0) (left-elbow -50 0) collar)
  (line-bone-chain (right-hand 50 0) (right-elbow 50 0) collar)
  (line-bone-chain (left-foot 0 60) (left-knee -42 42) pelvis)
  (line-bone-chain (right-foot 0 60) (right-knee 42 42) pelvis))
 400 500)

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
