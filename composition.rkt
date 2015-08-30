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
  (define (name [width 300] [height 300])
    (define calculated-view (void)) ; should get replaced on the initial do-update below
    
    (define (update joints)
      (apply (lambda (joint ...)
               constraint ...
               (set! calculated-view view)
               (list joint ...))
             joints))
    
    (define joints (update (list (v default-x default-y) ...)))
    
    (define (render dc)
      (r:render-to calculated-view dc))
    (define (get-handles)
      joints)
    (define (update-handle i vec)
      (set! joints
            (update (list-update-ref joints i vec))))
    (handle-view render get-handles update-handle width height)))
