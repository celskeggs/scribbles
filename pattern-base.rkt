#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "skeleton.rkt")
(require "entity.rkt")
(require "functional-graphics.rkt")

(provide PatternDef RendererSnippet
         new-pattern-def attach-renderer!
         new-pattern pattern-constructor)

(define-type PatternDef pattern-def)
(define-type RendererSnippet (-> (Listof Vector2D) Scale Renderer))

(struct pattern-def ([skeleton : SkeletonDef] [style : Style] [renderer-snippets : (Listof RendererSnippet)]) #:mutable)

(: new-pattern-def (-> SkeletonDef Style PatternDef))
(define (new-pattern-def skel style)
  (pattern-def skel style empty))

(: attach-renderer! (-> PatternDef RendererSnippet Void))
(define (attach-renderer! pat render)
  (set-pattern-def-renderer-snippets! pat (cons render (pattern-def-renderer-snippets pat))))

(: new-pattern (-> PatternDef Real Real Entity))
(define (new-pattern def x y)
  (let ((skel (new-skeleton (pattern-def-skeleton def) x y))
        (snippets (pattern-def-renderer-snippets def))
        (style (pattern-def-style def)))
    (entity (list->mutlist (skeleton-joints skel))
            (list (setting-positive-slider "Scale" 1 200 (skeleton-scale skel)))
            (lambda (w h)
              (update-skeleton skel)
              (apply style
                     (for/list : (Listof Renderer) ((rend : RendererSnippet snippets))
                       (rend (map (inst mut-get Vector2D) (skeleton-joints skel))
                             (mut-get (skeleton-scale skel)))))))))

(: pattern-constructor (-> PatternDef (-> Real Real Entity)))
(define (pattern-constructor def)
  (curry new-pattern def))
