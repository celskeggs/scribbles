#lang typed/racket
(require "utils.rkt")

(provide v v-x v-y)

(struct v ([x : Real] [y : Real]) #:transparent)

(: vunpack (-> v (values Real Real)))
(define-provide (vunpack v)
  (values (v-x v) (v-y v)))

(: v+ (-> v v v))
(define-provide (v+ v1 v2)
  (v (+ (v-x v1) (v-x v2))
     (+ (v-y v1) (v-y v2))))

(: v- (-> v v v))
(define-provide (v- v1 v2)
  (v (- (v-x v1) (v-x v2))
     (- (v-y v1) (v-y v2))))

(: v*c (-> v Real v))
(define-provide (v*c ve c)
  (v (* c (v-x ve))
     (* c (v-y ve))))

(: vlen-sq (-> v Nonnegative-Real))
(define-provide (vlen-sq v)
  (+ (sq (v-x v)) (sq (v-y v))))

(: vlen (-> v Nonnegative-Real))
(define-provide (vlen v)
  (sqrt (vlen-sq v)))

(: vdist-sq (-> v v Nonnegative-Real))
(define-provide (vdist-sq v1 v2)
  (vlen-sq (v- v1 v2)))

(: vdist (-> v v Nonnegative-Real))
(define-provide (vdist v1 v2)
  (sqrt (vdist-sq v1 v2)))

(: vin-origin-circle? (-> v Nonnegative-Real Boolean))
(define-provide (vin-origin-circle? v radius)
  (< (vlen-sq v) (sq radius)))

(: vin-circle? (-> v v Nonnegative-Real Boolean))
(define-provide (vin-circle? needle center radius)
  (vin-origin-circle? (v- needle center) radius))

(: vscale (-> v Nonnegative-Real v))
(define-provide (vscale v len)
  (v*c v (/ len (vlen v))))
