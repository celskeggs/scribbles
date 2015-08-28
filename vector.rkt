#lang racket
(require "utils.rkt")

(provide v v-x v-y)

(struct v (x y) #:inspector #f)

(define-provide (vunpack v)
  (values (v-x v) (v-y v)))

(define-provide (v+ v1 v2)
  (v (+ (v-x v1) (v-x v2))
     (+ (v-y v1) (v-y v2))))

(define-provide (v- v1 v2)
  (v (- (v-x v1) (v-x v2))
     (- (v-y v1) (v-y v2))))

(define-provide (v*c ve c)
  (v (* c (v-x ve))
     (* c (v-y ve))))

(define-provide (vlen-sq v)
  (+ (sq (v-x v)) (sq (v-y v))))

(define-provide (vlen v)
  (sqrt (vlen-sq v)))

(define-provide (vin-origin-circle? v radius)
  (< (vlen-sq v) (sq radius)))

(define-provide (vin-circle? needle center radius)
  (vin-origin-circle? (v- needle center) radius))

(define-provide (vscale v len)
  (v*c v (/ len (vlen v))))
