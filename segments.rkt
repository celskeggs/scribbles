#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")
(require "body.rkt")

(require racket/stxparam)
(require racket/splicing)

(provide fixed-distance construct root bone RenderSegment Vector2Ds Element)

(define-type ElementTree (U Element (Listof ElementTree)))

(define-type Vector2Ds (-> Symbol Vector2D))
(define-type GetVector (-> Vector2D))
(define-type SetVector (-> Vector2D Void))
(define-type UpdateSegment (-> Vector2Ds Void))
(define-type RenderSegment (-> Vector2Ds Renderer))
(define-type Element element)

(struct element ([name : Symbol]
                 [get : GetVector]
                 [set : SetVector]
                 [init : UpdateSegment]
                 [update : UpdateSegment]
                 [render : RenderSegment]
                 [requires : (Setof Symbol)]) #:transparent)

(: fixed-distance (-> Vector2D Vector2D Nonnegative-Real Vector2D))
(define (fixed-distance variant invariant distance)
  (v+ invariant (vscale (v- variant invariant) distance)))

(: root (->* (Symbol Real Real) ((Listof RenderSegment)) Element))
(define (root name x y [renders empty])
  (define position (vec x y))
  (element name
           (lambda () position)
           (lambda (new) (set! position new))
           void ; no init necessary
           void ; no updating necessary
           (lambda (lookup) (apply r:all (apply-map renders lookup)))
           (set)))

(: bone (->* (Symbol Symbol Real Real) ((Listof RenderSegment)) Element))
(define (bone variant invariant dx dy [renders empty])
  (define position (vec dx dy))
  (: distance Nonnegative-Real)
  (define distance (vlen (vec dx dy)))
  (element variant
           (lambda () position)
           (lambda (new) (set! position new))
           (lambda (lookup) (set! position (v+ (vec dx dy) (lookup invariant))))
           (lambda (lookup) (set! position (fixed-distance position (lookup invariant) distance)))
           (lambda (lookup) (apply r:all (apply-map renders lookup)))
           (set invariant)))

(: chop-trees (-> ElementTree (Listof Element)))
(define (chop-trees element-tree)
  (if (list? element-tree)
      (append* (map chop-trees element-tree))
      (list element-tree)))

(: construct (-> ElementTree * Body))
(define (construct . element-trees)
  (: elements (Listof Element))
  (define elements (chop-trees element-trees))
  (: lookup-hash (HashTable Symbol GetVector))
  (define lookup-hash (make-hash (for/list : (Listof (Pairof Symbol GetVector)) ((elem elements))
                                   (cons (element-name elem) (element-get elem)))))
  (unless (= (length elements) (hash-count lookup-hash))
    (error "duplicate names!"))
  (unless (subset? (set-union* (map element-requires elements)) (list->set (hash-keys lookup-hash)))
    (error "Missing requirements:" (set-subtract (set-union* (map element-requires elements)) (list->set (hash-keys lookup-hash)))))
  (: lookup Vector2Ds)
  (define (lookup x)
    ((hash-ref lookup-hash x)))
  (: get-vectors (-> (Listof Vector2D)))
  (define (get-vectors)
    (apply-each (map element-get elements)))
  (: set-vector! (-> Nonnegative-Integer Vector2D Void))
  (define (set-vector! i vec)
    ((element-set (list-ref elements i)) vec))
  (define (update-and-render!)
    (apply-map (map element-update elements) lookup)
    (apply r:all (apply-map (map element-render elements) lookup)))

  ; initialize
  (apply-map (map element-init elements) lookup)

  (body-new get-vectors set-vector! update-and-render!))
