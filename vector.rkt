#lang typed/racket
(require "utils.rkt")
(require racket/performance-hint)

(provide Vector2D vec? vec vec-x vec-y vunpack vec->pair
         v+ v- v*c
         vlen-sq vlen vdist-sq vdist
         vin-origin-circle? vin-circle? vin-origin-rectangle? vin-rectangle?
         vscale vinterpolate vrotate-origin-rad vrotate-origin-deg vrotate-rad vrotate-deg)

(define-type Vector2D vecstr)

(struct vecstr ([x : Float] [y : Float]) #:transparent)

(begin-encourage-inline
(define vec? vecstr?)
(define vec-x vecstr-x)
(define vec-y vecstr-y)
(: vec (-> Float Float Vector2D))
(define (vec x y) (vecstr x y))

(: vunpack (-> Vector2D (values Float Float)))
(define (vunpack v)
  (values (vec-x v) (vec-y v)))

(: vec->pair (-> Vector2D (Pairof Float Float)))
(define (vec->pair v)
  (cons (vec-x v) (vec-y v)))

(: v+ (-> Vector2D Vector2D Vector2D))
(define (v+ v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))))

(: v- (-> Vector2D Vector2D Vector2D))
(define (v- v1 v2)
  (vec (- (vec-x v1) (vec-x v2))
       (- (vec-y v1) (vec-y v2))))

(: v*c (-> Vector2D Float Vector2D))
(define (v*c ve c)
  (vec (* c (vec-x ve))
       (* c (vec-y ve))))

(: vlen-sq (-> Vector2D Nonnegative-Float))
(define (vlen-sq v)
  (+ (sq (vec-x v)) (sq (vec-y v))))

(: vlen (-> Vector2D Nonnegative-Float))
(define (vlen v)
  (sqrt (vlen-sq v)))

(: vdist-sq (-> Vector2D Vector2D Nonnegative-Float))
(define (vdist-sq v1 v2)
  (vlen-sq (v- v1 v2)))

(: vdist (-> Vector2D Vector2D Nonnegative-Float))
(define (vdist v1 v2)
  (sqrt (vdist-sq v1 v2)))

(: vin-origin-circle? (-> Vector2D Nonnegative-Float Boolean))
(define (vin-origin-circle? v radius)
  (< (vlen-sq v) (sq radius)))

(: vin-origin-rectangle? (-> Vector2D Vector2D Boolean))
(define (vin-origin-rectangle? v size)
  (and (>= (vec-x v) 0.0) (>= (vec-y v) 0.0)
       (< (vec-x v) (vec-x size)) (< (vec-y v) (vec-y size))))

(: vin-circle? (-> Vector2D Vector2D Nonnegative-Float Boolean))
(define (vin-circle? needle center radius)
  (vin-origin-circle? (v- needle center) radius))

(: vin-rectangle? (-> Vector2D Vector2D Vector2D Boolean))
(define (vin-rectangle? needle pos size)
  (vin-origin-rectangle? (v- needle pos) size))

(: vscale (-> Vector2D Nonnegative-Float Vector2D))
(define (vscale v len)
  (v*c v (/ len (vlen v))))

(: vinterpolate (-> Vector2D Vector2D Float Vector2D))
(define (vinterpolate zero one r)
  (define rn (cond ((< r 0.0) 0.0)
                   ((> r 1.0) 1.0)
                   (else r)))
  (v+ (v*c one rn) (v*c zero (- 1 rn))))

(: vrotate-origin-rad (-> Vector2D Float Vector2D))
(define (vrotate-origin-rad v rad)
  (let-values (((x y) (vunpack v)))
    (let ((c (cos rad)) (s (sin rad)))
      (vec (- (* x c) (* y s))
           (+ (* x s) (* y c))))))

(: vrotate-origin-deg (-> Vector2D Float Vector2D))
(define (vrotate-origin-deg v deg)
  (vrotate-origin-rad v (degrees->radians deg)))

(: vrotate-rad (-> Vector2D Vector2D Float Vector2D))
(define (vrotate-rad original around radians)
  (v+ around (vrotate-origin-rad (v- original around) radians)))

(: vrotate-deg (-> Vector2D Vector2D Float Vector2D))
(define (vrotate-deg original around degrees)
  (vrotate-rad original around (degrees->radians degrees)))
)
