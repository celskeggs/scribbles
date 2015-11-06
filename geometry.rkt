#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")

(provide midpoint right-triangle hypot-right-triangle triangle-deg triangle-rad hypot-triangle hypot-known-legs
         translate-along-sphere
         fixed-distance maximum-distance minimum-distance)

(: midpoint (-> Vector2D Vector2D Vector2D))
(define (midpoint a b)
  (vec (avg (vec-x a) (vec-x b))
       (avg (vec-y a) (vec-y b))))

; biased clockwise. (0, 0) and (5, 0) will be matched with (0, -5).
(: right-triangle (-> Vector2D Vector2D Vector2D))
(define (right-triangle center foot)
  (triangle-deg center foot -90.0)) ; TODO: make this slightly more accurate?

; biased so that L--R -> below and R--L -> above.
(: hypot-right-triangle (-> Vector2D Vector2D Vector2D))
(define (hypot-right-triangle left right)
  (right-triangle (midpoint left right) right))

(: triangle-deg (-> Vector2D Vector2D Float Vector2D))
(define (triangle-deg center foot d)
  (vrotate-deg foot center d))

(: triangle-rad (-> Vector2D Vector2D Float Vector2D))
(define (triangle-rad center foot r)
  (vrotate-rad foot center r))

; positive above L--R and negative below L--R
(: hypot-triangle (-> Vector2D Vector2D Float Vector2D))
(define (hypot-triangle left right height)
  (v+ (midpoint left right)
      (vscale (vrotate-origin-deg (v- left right)
                                  (if (negative? height) -90.0 90.0))
              (abs height))))

(: hypot-known-legs (-> Vector2D Vector2D Float Vector2D))
(define (hypot-known-legs left right leg)
  (let ((root (sqrt-opt (- (sq leg) (sq (/ (vdist left right) 2))))))
    (if root
        (hypot-triangle left right (if (positive? leg) root (- root)))
        (midpoint left right)))) ; TODO: is this graceful degradation optimal?

(: fixed-distance (-> Vector2D Vector2D Positive-Float Vector2D))
(define (fixed-distance variant invariant distance)
  (v+ invariant (vscale (v- variant invariant) distance)))

(: maximum-distance (-> Vector2D Vector2D Positive-Float Vector2D))
(define (maximum-distance variant invariant distance)
  (if (> (vdist variant invariant) distance)
      (fixed-distance variant invariant distance)
      variant))

(: minimum-distance (-> Vector2D Vector2D Positive-Float Vector2D))
(define (minimum-distance variant invariant distance)
  (if (< (vdist variant invariant) distance)
      (fixed-distance variant invariant distance)
      variant))

(: x*c (-> Vector2D Float Vector2D))
(define (x*c v s)
  (vec (* (vec-x v) s) (vec-y v)))

(: translate-along-sphere (-> Vector2D Vector2D Vector2D Float Vector2D))
(define (translate-along-sphere center top align tx)
  (let* ((rel-top (v- top center))
         (rel-align (v- align center))
         (radius (vlen rel-top))
         (rotation-to (atan (vec-x rel-top) (vec-y rel-top))) ; such that (= (vrotate-rad rel-top rotation-to) (vec 0 radius))
         (rot-align (vrotate-origin-rad rel-align rotation-to))
         (x-for-the-y (sqrt-opt (- (sq radius) (sq (vec-y rot-align))))))
    (if x-for-the-y
        (let* ((scale-factor (/ x-for-the-y (vec-x rot-align)))
               (inv-scale-factor (/ 1 scale-factor))
               (scalerot-align (x*c rot-align scale-factor))
               (translation (- (/ tx radius))) ; (/ tx radius) is the translation of length tx around a circle of length radius, in radians!
               (scalerot-tx (vrotate-origin-rad scalerot-align translation))
               (rel-tx (vrotate-origin-rad (x*c scalerot-tx inv-scale-factor) (- rotation-to)))
               (final-tx (v+ rel-tx center)))
          final-tx)
        (error "translate-along-sphere must be in sphere!"))))
