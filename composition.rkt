#lang racket
(require "utils.rkt")
(require "manipulation-view.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")
(require racket/generic)
(require "segments.rkt")

(provide compose fixed-distance root bone)

(define (compose-segs segs [width 300] [height 300])
  (define (calculate)
    (apply r:all (map (lambda (seg) ((segment-update-and-render seg))) segs)))
  (define calculated-view (calculate))

  (define (render-local dc)
    (r:render-to calculated-view dc))
  (define (get-handles-local)
    (map (lambda (seg) ((segment-get seg))) segs))
  (define (update-handle-local! i vec)
    ((segment-set (list-ref segs i)) vec)
    (set! calculated-view (calculate)))
  (handle-view render-local get-handles-local update-handle-local! width height))

(define-syntax-rule (compose element ...)
  (compose-segs (construct element ...)))
