#lang racket
(require "utils.rkt")
(require "manipulation-view.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")

;(define-syntax-rule (specific-distance state variant invariant distance)
;  (struct-copy ds state
;               [variant (v+ (get-vector state invariant) (rescale-vector (v- (get-vector state variant) (get-vector state invariant)) distance))]))
;(specific-distance state skel-head skel-collar (/ (get-distance state skel-collar skel-pelvis) 4)))

(provide composer fixed-distance)

(define-syntax-rule (fixed-distance variant invariant distance)
  (set! variant (v+ invariant (vscale (v- variant invariant) distance))))

(define-syntax-rule (composer name ((joint default-x default-y) ...) constraint ... view)
  (define (name)
    (define (apply-constraints joints)
      (apply (lambda (joint ...)
               constraint ...
               (list joint ...))
             joints))
    
    (define joints (apply-constraints (list (v default-x default-y) ...)))
    
    (define (calculate-view)
      (apply (lambda (joint ...) view) joints))
    
    (define (set-and-apply-constraints! i vec)
      (set! joints
            (apply-constraints (list-update-ref joints i vec))))
    
    (define calculated-view (calculate-view))
    
    (define (render dc)
      (r:render-to calculated-view dc))
    (define (get-handles)
      joints)
    (define (update-handle i vec)
      (set-and-apply-constraints! i vec)
      (set! calculated-view (calculate-view)))
    (handle-view render get-handles update-handle)))
