#lang racket
(require "manipulation-view.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")

;(define-syntax-rule (specific-distance state variant invariant distance)
;  (struct-copy ds state
;               [variant (v+ (get-vector state invariant) (rescale-vector (v- (get-vector state variant) (get-vector state invariant)) distance))]))
;(specific-distance state skel-head skel-collar (/ (get-distance state skel-collar skel-pelvis) 4)))

(provide composer)

(define-syntax-rule (composer name ((joint default-x default-y) ...) view)
  (define (name)
    (define joints (vector (v default-x default-y) ...))

    (define (calculate-view-internal joint ...)
      view)
    
    (define (calculate-view)
      (apply calculate-view-internal (vector->list joints)))
    
    (define calculated-view (calculate-view))

    (define (render dc)
      (r:render-to calculated-view dc))
    (define (get-handles)
      (vector->list joints))
    (define (update-handle i vec)
      (vector-set! joints i vec)
      (set! calculated-view (calculate-view)))
    (handle-view render get-handles update-handle)))
