#lang racket

(require "utils.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")
(require racket/generic)
(require racket/stxparam)
(require racket/splicing)

(provide fixed-distance segment-get segment-set segment-update-and-render construct root bone)

(struct segment (get set update-and-render) #:inspector #f)

(define-syntax-parameter segments
  (lambda (stx)
    (raise-syntax-error #f "Use outside of a construct" stx)))

(define-syntax-rule (fixed-distance variant invariant distance)
  (set! variant (v+ invariant (vscale (v- variant invariant) distance))))

(define-syntax-rule (root name x y renders ...)
  (begin
    (define name (v x y))
    (set! segments
          (cons (segment (lambda () name)
                         (lambda (vn) (set! name vn))
                         (lambda () (r:all renders ...)))
                segments))))

(define-syntax-rule (bone variant invariant delta extra-render ...)
  (begin
    (define distance (vlen delta))
    (define variant (v+ invariant delta))
    (set! segments
          (cons (segment (lambda () variant)
                         (lambda (vn) (set! variant vn))
                         (lambda () (fixed-distance variant invariant distance)))
                segments))))

(define-syntax-rule (construct element ...)
  (let ((local-segments empty))
    (splicing-syntax-parameterize ([segments (make-rename-transformer #'local-segments)])
      element ...)
    local-segments))

#|(construct
 (root collar 250 250)
 (bone pelvis collar (v 0 100))
 (bone head collar (v 0 -30) (r:circle head)))|#
