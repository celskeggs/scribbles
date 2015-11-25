#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")

(provide fit-cubics)

; This is an implementation of Rob Spencer's bezier curve fitting algorithm
; http://scaledinnovation.com/analytics/splines/aboutSplines.html

(: calc-control-points (-> Vector2D Vector2D Vector2D Float (Values Vector2D Vector2D)))
(define (calc-control-points v0 v1 v2 t)
  (let ((d01 (vdist v0 v1))
        (d12 (vdist v1 v2))
        (r20 (v- v2 v0)))
    (let ((fa (/ (* t d01) (+ d01 d12)))
          (fb (/ (* t d12) (+ d01 d12))))
      (values (v- v1 (v*c r20 fa))
              (v+ v1 (v*c r20 fb))))))

; An implementation of Bill Spitzak's quadratic bezier curve -> cubic bezier curve algorithm.
; I don't know if Bill discovered this algorithm - but this is where I got it from:
; http://lists.cairographics.org/archives/cairo/2009-October/018351.html

(: quadratic->cubic (-> Vector2D Vector2D Vector2D (Values Vector2D Vector2D Vector2D Vector2D)))
(define (quadratic->cubic p0 p1 p2)
  (values p0
          (vinterpolate p0 p1 (/ 2.0 3.0))
          (vinterpolate p1 p2 (/ 2.0 3.0))
          p2))

(: fit-cubics (-> Vector2D Vector2D Vector2D Float (Values Vector2D Vector2D Vector2D Vector2D    ; cubic 1
                                                           Vector2D Vector2D Vector2D Vector2D))) ; cubic 2
(define (fit-cubics v0 v1 v2 t)
  (let-values (((cp1 cp2) (calc-control-points v0 v1 v2 t)))
    (let-values (((ca0 ca1 ca2 ca3) (quadratic->cubic v0 cp1 v1))
                 ((cb0 cb1 cb2 cb3) (quadratic->cubic v1 cp2 v2)))
      (values ca0 ca1 ca2 ca3 cb0 cb1 cb2 cb3))))
