#lang typed/racket
(require "composition.rkt")
(require "functional-graphics.rkt")
(require "vector.rkt")

(define-syntax-rule (line-bone variant invariant dx dy extra-renders ...)
  (bone variant invariant (vec dx dy) (r:line variant invariant) extra-renders ...))

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

(define simple-style (r:wrap-style "black" 6 'solid "white" 'solid))

(compose simple-style 500 500
         (root 'collar 250 250)
         (line-bone 'pelvis 'collar 0 100)
         (line-bone 'head 'collar 0 -70 (r:circle head 50))
         (line-bone-chain ('left-hand -50 0) ('left-elbow -50 0) 'collar)
         (line-bone-chain ('right-hand 50 0) ('right-elbow 50 0) 'collar)
         (line-bone-chain ('left-foot 0 60) ('left-knee -42 42) 'pelvis)
         (line-bone-chain ('right-foot 0 60) ('right-knee 42 42) 'pelvis))
