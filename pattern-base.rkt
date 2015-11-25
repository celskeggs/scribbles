#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "joints.rkt")
(require "skeleton.rkt")
(require "entity.rkt")
(require "functional-graphics.rkt")
(require "saving.rkt")
(require "setting.rkt")
(require "setting-group.rkt")

(provide PatternDef RendererSnippet
         pattern-def-new
         attach-renderer! attach-line! attach-poly! attach-circle! attach-spline! attach-bspline!
         ren-line ren-bone ren-circle ren-conditional ren-nothing
         pattern-lock pattern-load
         pattern-def-skeleton
         pattern-new pattern-constructor)

(define-type PatternDef pattern-def)
(define-type LockedPatternDef locked-pattern-def)
(define-type RendererSnippet (-> Jointset Renderer))

(struct pattern-def ([skeleton : SkeletonDef]
                     [style : Style]
                     [renderer-snippets : (Listof RendererSnippet)]) #:mutable)
(struct locked-pattern-def ([skeleton : LockedSkeletonDef]
                            [style : Style]
                            [renderer-snippets : (Listof RendererSnippet)]
                            [name : Symbol]))

(: pattern-def-new (-> SkeletonDef Style PatternDef))
(define (pattern-def-new skel style)
  (pattern-def skel style empty))

(: attach-renderer! (-> PatternDef RendererSnippet Void))
(define (attach-renderer! pat render)
  (set-pattern-def-renderer-snippets! pat (cons render (pattern-def-renderer-snippets pat))))

(: ren-nothing RendererSnippet)
(define (ren-nothing sk)
  r:nothing)

(: ren-conditional (-> String RendererSnippet RendererSnippet RendererSnippet))
(define ((ren-conditional option-name for-true for-false) sk)
  (if (setting->value (setting-group-option-ref (jointset-settings sk) option-name))
    (for-true sk)
    (for-false sk)))

(: ren-line (->* (JointRef JointRef) (Style) RendererSnippet))
(define ((ren-line a b [style r:all]) sk)
  (style (r:line (joint-ref sk a)
                 (joint-ref sk b))))

(: ren-bone (->* (BoneRef) (Style) RendererSnippet))
(define (ren-bone br [style r:all])
  (ren-line (car br) (cdr br) style))

(: attach-line! (->* (PatternDef BoneRef) (Style) Void))
(define (attach-line! pat br [style r:all])
  (attach-renderer! pat (ren-bone br style)))

(: attach-poly! (->* (PatternDef (Listof JointRef)) (Style) Void))
(define (attach-poly! pat joints [style r:all])
  (attach-renderer! pat (lambda (sk)
                          (style (r:poly (for/list ((joint joints))
                                           (joint-ref sk joint)))))))

(: ren-circle (->* (JointRef Scale) (Style) RendererSnippet))
(define ((ren-circle center size [style r:all]) sk)
  (style (r:circle (joint-ref sk center)
                   (scale* sk size))))

(: attach-circle! (->* (PatternDef JointRef Scale) (Style) Void))
(define (attach-circle! pat center size [style r:all])
  (attach-renderer! pat (ren-circle center size style)))

(: attach-spline! (->* (PatternDef JointRef JointRef JointRef) (Style) Void))
(define (attach-spline! pat v1 v2 v3 [style r:all])
  (attach-renderer! pat (lambda (sk)
                          (style (r:spline (joint-ref sk v1)
                                           (joint-ref sk v2)
                                           (joint-ref sk v3))))))

(: attach-bspline! (->* (PatternDef JointRef JointRef JointRef Float) (Style) Void))
(define (attach-bspline! pat v1 v2 v3 t [style r:all])
  (attach-renderer! pat (lambda (sk)
                          (style (r:bspline (joint-ref sk v1)
                                            (joint-ref sk v2)
                                            (joint-ref sk v3)
                                            t)))))

(define-predicate valid-enc-skel? EncodedSkeleton)

(: pattern-load (-> LockedPatternDef Encoded Entity))
(define (pattern-load pat enc)
  (if (valid-enc-skel? enc)
      (skeleton->pattern pat (skeleton-load (locked-pattern-def-skeleton pat) enc))
      (error "not a valid saved pattern")))

(: pattern-lock (-> PatternDef Symbol LockedPatternDef))
(define (pattern-lock pat unique-name)
  (define locked
    (locked-pattern-def (skeleton-lock (pattern-def-skeleton pat))
                        (pattern-def-style pat)
                        (reverse (pattern-def-renderer-snippets pat))
                        unique-name))
  (register-entity-brand! unique-name (curry pattern-load locked))
  locked)

(: skeleton->pattern (-> LockedPatternDef Skeleton Entity))
(define (skeleton->pattern def skel)
  (define name (locked-pattern-def-name def))
  (entity (list->mutlist (skeleton-handles skel))
          (skeleton-settings skel)
          (lambda (w h)
              (update-skeleton skel)
              (apply (locked-pattern-def-style def)
                     (ann (apply-map (locked-pattern-def-renderer-snippets def) (skeleton-js skel))
                          (Listof Renderer))))
          (lambda ()
            (list name
                  (skeleton-save skel)))))

(: pattern-new (-> LockedPatternDef Vector2D Entity))
(define (pattern-new def base-vec)
  (skeleton->pattern def (skeleton-new (locked-pattern-def-skeleton def) base-vec)))

(: pattern-constructor (-> LockedPatternDef (-> Vector2D Entity)))
(define (pattern-constructor def)
  (curry pattern-new def))
