#lang typed/racket
(require "utils.rkt")

(provide Vector2D vec vec-x vec-y vunpack v+ v- v*c vlen-sq vlen vdist-sq vdist vin-origin-circle? vin-circle? vin-origin-rectangle? vin-rectangle? vscale vinterpolate)

(define-type Vector2D vecstr)

(struct vecstr ([x : Real] [y : Real]) #:transparent)

(define vec-x vecstr-x)
(define vec-y vecstr-y)

(: vec (-> Real Real Vector2D))
(define (vec x y) (vecstr x y))

(: vunpack (-> Vector2D (values Real Real)))
(define (vunpack v)
  (values (vec-x v) (vec-y v)))

(: v+ (-> Vector2D Vector2D Vector2D))
(define (v+ v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))))

(: v- (-> Vector2D Vector2D Vector2D))
(define (v- v1 v2)
  (vec (- (vec-x v1) (vec-x v2))
       (- (vec-y v1) (vec-y v2))))

(: v*c (-> Vector2D Real Vector2D))
(define (v*c ve c)
  (vec (* c (vec-x ve))
       (* c (vec-y ve))))

(: vlen-sq (-> Vector2D Nonnegative-Real))
(define (vlen-sq v)
  (+ (sq (vec-x v)) (sq (vec-y v))))

(: vlen (-> Vector2D Nonnegative-Real))
(define (vlen v)
  (sqrt (vlen-sq v)))

(: vdist-sq (-> Vector2D Vector2D Nonnegative-Real))
(define (vdist-sq v1 v2)
  (vlen-sq (v- v1 v2)))

(: vdist (-> Vector2D Vector2D Nonnegative-Real))
(define (vdist v1 v2)
  (sqrt (vdist-sq v1 v2)))

(: vin-origin-circle? (-> Vector2D Nonnegative-Real Boolean))
(define (vin-origin-circle? v radius)
  (< (vlen-sq v) (sq radius)))

(: vin-origin-rectangle? (-> Vector2D Vector2D Boolean))
(define (vin-origin-rectangle? v size)
  (and (>= (vec-x v) 0) (>= (vec-y v) 0)
       (< (vec-x v) (vec-x size)) (< (vec-y v) (vec-y size))))

(: vin-circle? (-> Vector2D Vector2D Nonnegative-Real Boolean))
(define (vin-circle? needle center radius)
  (vin-origin-circle? (v- needle center) radius))

(: vin-rectangle? (-> Vector2D Vector2D Vector2D Boolean))
(define (vin-rectangle? needle pos size)
  (vin-origin-rectangle? (v- needle pos) size))

(: vscale (-> Vector2D Nonnegative-Real Vector2D))
(define (vscale v len)
  (v*c v (/ len (vlen v))))

(: vinterpolate (-> Vector2D Vector2D Real Vector2D))
(define (vinterpolate zero one r)
  (define rn (cond ((< r 0) 0)
                   ((> r 1) 1)
                   (else r)))
  (v+ (v*c one rn) (v*c zero (- 1 rn))))
