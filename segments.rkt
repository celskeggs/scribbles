#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")

(require racket/stxparam)
(require racket/splicing)

(provide fixed-distance segment-get segment-set segment-update-and-render segment construct root bone slotted-segment)

(define-type GetSegmentFunc (-> Vector2D))
(define-type SetSegmentFunc (-> Vector2D Void))
(define-type UpdateAndRenderSegmentFunc (-> Renderer))

(struct segment ([get : GetSegmentFunc] [set : SetSegmentFunc] [update-and-render : UpdateAndRenderSegmentFunc]) #:transparent)

(define-type SegmentProducer (-> (HashTable Symbol segment) (Listof segment)))

(struct slotted-segment ([provides : (Setof Symbol)] [requires : (Setof Symbol)] [seg : SegmentProducer]) #:transparent)

(: fixed-distance (-> Vector2D Vector2D Nonnegative-Real Vector2D))
(define (fixed-distance variant invariant distance)
  (v+ invariant (vscale (v- variant invariant) distance)))

(: root-body (-> Symbol Real Real (Listof Renderer) SegmentProducer))
(define ((root-body name x y renders) seghash)
  (define position (vec x y))
  (define seg (segment (lambda () position)
                           (lambda (vn) (set! position vn))
                           (lambda () (apply r:all renders))))
  (hash-set! seghash name seg)
  (list seg))

(: root (-> Symbol Real Real Renderer * slotted-segment))
(define (root name x y . renders)
  (slotted-segment (set name) (set) (root-body name x y renders)))

(: bone-body (-> Symbol Symbol Vector2D (Listof Renderer) SegmentProducer))
(define ((bone-body variant invariant delta extra-renders) seghash)
  (: distance Nonnegative-Real)
  (define distance (vlen delta))
  (: position Vector2D)
  (define position (v+ ((segment-get (hash-ref seghash invariant))) delta)) ; TODO: make this less brittle in the face of different orders
  (define seg (segment (lambda () position)
                           (lambda (vn) (set! position vn))
                           (lambda ()
                             (set! position (fixed-distance position ((segment-get (hash-ref seghash invariant))) distance)) ; TODO: make this nicer
                             (apply r:all extra-renders))))
  (hash-set! seghash variant seg)
  (list seg))

(: bone (-> Symbol Symbol Vector2D Renderer * slotted-segment))
(define (bone variant invariant delta . renders)
  (slotted-segment (set variant) (set invariant) (bone-body variant invariant delta renders)))

(: construct (-> slotted-segment * (Listof segment)))
(define (construct . elements)
  (: current-segments (HashTable Symbol segment))
  (define current-segments (make-hash))
  (: segments-list (Listof segment))
  (define segments-list empty)
  (define available-names (set-union* (map slotted-segment-provides elements)))
  (define required-names (set-union* (map slotted-segment-requires elements)))
  (define missing-names (set-subtract required-names available-names))
  (unless (set-empty? missing-names)
    (error "could not find segments:" missing-names))

  (for ((element elements))
    (unless (void? element)
      (set! segments-list (append ((slotted-segment-seg element) current-segments) segments-list))))

  (reverse segments-list))
