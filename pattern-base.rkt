#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "skeleton.rkt")
(require "entity.rkt")
(require "functional-graphics.rkt")
(require "saving.rkt")

(provide PatternDef RendererSnippet
         new-pattern-def
         attach-renderer! attach-line! attach-poly!
         lock-pattern! pattern-load
         new-pattern pattern-constructor)

(define-type PatternDef pattern-def)
(define-type RendererSnippet (-> (Listof Vector2D) Scale Renderer))

(struct pattern-def ([skeleton : SkeletonDef]
                     [style : Style]
                     [rev-renderer-snippets : (Listof RendererSnippet)]
                     [locked-name : (U #f Symbol)]) #:mutable)

(: new-pattern-def (-> SkeletonDef Style PatternDef))
(define (new-pattern-def skel style)
  (pattern-def skel style empty #f))

(: attach-renderer! (-> PatternDef RendererSnippet Void))
(define (attach-renderer! pat render)
  (when (pattern-def-locked-name pat)
    (error "pattern is locked!"))
  (set-pattern-def-rev-renderer-snippets! pat (cons render (pattern-def-rev-renderer-snippets pat))))

(: attach-line! (->* (PatternDef BoneRef) (Style) Void))
(define (attach-line! pat br [style r:all])
  (attach-renderer! pat (lambda ([vecs : (Listof Vector2D)] [scale : Scale])
                          (style (r:line (joint-v-ref scale vecs (car br)) (joint-v-ref scale vecs (cdr br)))))))

(: attach-poly! (->* (PatternDef (Listof JointRef)) (Style) Void))
(define (attach-poly! pat joints [style r:all])
  (attach-renderer! pat (lambda ([vecs : (Listof Vector2D)] [scale : Scale])
                          (style (r:poly (for/list ((joint joints))
                                           (joint-v-ref scale vecs joint)))))))

(define-predicate valid-enc-skel? EncodedSkeleton)

(: pattern-load (-> PatternDef Encoded Entity))
(define (pattern-load pat enc)
  (if (valid-enc-skel? enc)
      (skeleton->pattern pat  (skeleton-load (pattern-def-skeleton pat) enc))
      (error "not a valid saved pattern")))

(: lock-pattern! (-> PatternDef Symbol Void))
(define (lock-pattern! pat unique-name)
  (when (pattern-def-locked-name pat)
    (error "pattern is locked!"))
  (skeleton-lock! (pattern-def-skeleton pat))
  (set-pattern-def-locked-name! pat unique-name)
  (register-entity-brand! unique-name (curry pattern-load pat)))

(: skeleton->pattern (-> PatternDef Skeleton Entity))
(define (skeleton->pattern def skel)
  (define name (pattern-def-locked-name def))
  (unless name
    (error "pattern is not locked!"))
  (entity (list->mutlist (skeleton-joints skel))
          (list (setting-positive-slider "Scale" 1 200 (skeleton-scale skel)))
          (lambda (w h)
              (update-skeleton skel)
              (apply (pattern-def-style def)
                     (for/list : (Listof Renderer) ((rend : RendererSnippet (reverse (pattern-def-rev-renderer-snippets def))))
                       (rend (map (inst mut-get Vector2D) (skeleton-joints skel))
                             (mut-get (skeleton-scale skel))))))
          (lambda ()
            (list name
                  (skeleton-save skel)))))

(: new-pattern (-> PatternDef Real Real Entity))
(define (new-pattern def x y)
  (unless (pattern-def-locked-name def)
    (error "pattern is not locked!"))
  (skeleton->pattern def (new-skeleton (pattern-def-skeleton def) x y)))

(: pattern-constructor (-> PatternDef (-> Real Real Entity)))
(define (pattern-constructor def)
  (curry new-pattern def))
