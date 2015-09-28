#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")

(provide SkeletonDef Skeleton JointRef BoneRef Scale Constraint SimpleConstraint
         new-skeleton-def attach-joint! attach-constraint! attach-simple-constraint! attach-fixed-bone!
         new-skeleton update-skeleton skeleton-joints skeleton-scale
         scale*)

(define-type SkeletonDef skeleton-def)
(define-type Skeleton skeleton)
(define-type JointRef Nonnegative-Integer)
(define-type BoneRef (Pairof Nonnegative-Integer Nonnegative-Integer))
(define-type Scale Positive-Real)
(define-type Constraint (-> Scale (Listof (Mutable Vector2D)) Void))
(define-type SimpleConstraint (-> Scale Vector2D (Listof Vector2D) Vector2D))

(struct skeleton-def ([rev-joint-inits : (Listof Vector2D)] [default-scale : Scale] [rev-constraints : (Listof Constraint)]) #:mutable)
(struct skeleton ([joints : (Listof (Mutable Vector2D))] [scale : (Mutable Scale)] [constraints : Constraint]))

(: scale* (-> Scale Scale Scale))
(define (scale* a b)
  (let ((nnr (* a b)))
    (if (positive? nnr) nnr
        (error "scale dropped to zero"))))

(: new-skeleton-def (-> Scale SkeletonDef))
(define (new-skeleton-def default-scale)
  (skeleton-def empty default-scale empty))

(: merge-constraints (-> (Listof Constraint) Constraint))
(define ((merge-constraints constraints) scale vecs)
  (for ((constraint constraints))
    (constraint scale vecs)))

(: attach-joint! (-> SkeletonDef Real Real JointRef))
(define (attach-joint! skel dx dy)
  (define new-joint-id (length (skeleton-def-rev-joint-inits skel)))
  (set-skeleton-def-rev-joint-inits! skel (cons (vec dx dy) (skeleton-def-rev-joint-inits skel)))
  new-joint-id)

(: assert-valid-joint (-> SkeletonDef JointRef Void))
(define (assert-valid-joint skel joint)
  (unless (< joint (length (skeleton-def-rev-joint-inits skel)))
    (error "Joint is not yet defined: " joint)))

(: attach-constraint! (-> SkeletonDef Constraint Void))
(define (attach-constraint! skel const)
  (set-skeleton-def-rev-constraints! skel (cons const (skeleton-def-rev-constraints skel))))

(: attach-simple-constraint! (-> SkeletonDef JointRef SimpleConstraint Void))
(define (attach-simple-constraint! skel joint constraint)
  (assert-valid-joint skel joint)
  (attach-constraint! skel
                      (lambda ([scale : Scale] [vs : (Listof (Mutable Vector2D))])
                        (mut-set! (list-ref vs joint)
                                  (constraint scale (mut-get (list-ref vs joint)) (map (inst mut-get Vector2D) vs))))))

(: fixed-distance (-> Vector2D Vector2D Scale Vector2D))
(define (fixed-distance variant invariant distance)
  (v+ invariant (vscale (v- variant invariant) distance)))

(: attach-fixed-bone! (-> SkeletonDef JointRef JointRef Scale BoneRef))
(define (attach-fixed-bone! skel variable invariable scale-multiplier)
  (assert-valid-joint skel invariable)
  (attach-simple-constraint! skel variable
                             (lambda ([scale : Scale] [var : Vector2D] [joints : (Listof Vector2D)])
                               (fixed-distance var (list-ref joints invariable) (scale* scale scale-multiplier))))
  (cons variable invariable))


(: new-skeleton (-> SkeletonDef Real Real Skeleton))
(define (new-skeleton skel x y)
  (let ((joints (for/list : (Listof (Mutable Vector2D))
                  ((def-xy (reverse (skeleton-def-rev-joint-inits skel))))
                  (mut-cell (v+ def-xy (vec x y)))))
        (constraints (merge-constraints (reverse (skeleton-def-rev-constraints skel))))
        (scale ((inst mut-cell Scale) (skeleton-def-default-scale skel))))
    (constraints (mut-get scale) joints)
    (skeleton joints scale constraints)))

(: update-skeleton (-> Skeleton Void))
(define (update-skeleton skel)
  ((skeleton-constraints skel) (mut-get (skeleton-scale skel)) (skeleton-joints skel)))
