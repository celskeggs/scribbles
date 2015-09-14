#lang typed/racket

(require "utils.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")
(require racket/stxparam)
(require racket/splicing)

(provide fixed-distance segment-get segment-set segment-update-and-render segment construct root bone)

(define-type GetSegmentFunc (-> v))
(define-type SetSegmentFunc (-> v Void))
(define-type UpdateAndRenderSegmentFunc (-> Renderer))

(struct segment ([get : GetSegmentFunc] [set : SetSegmentFunc] [update-and-render : UpdateAndRenderSegmentFunc]) #:transparent)

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
                         (lambda () (fixed-distance variant invariant distance) (r:all extra-render ...)))
                segments))))

(define-syntax-rule (construct element ...)
  (let ((local-segments : (Listof segment) empty))
    (splicing-syntax-parameterize ([segments (make-rename-transformer #'local-segments)])
      element ...)
    (reverse local-segments)))
