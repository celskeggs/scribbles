#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "saving.rkt")

(provide SkeletonDef Skeleton
         JointRef RealJointRef DerivedJointRef BoneRef
         Scale
         Constraint SimpleConstraint
         EncodedSkeleton
         new-skeleton-def attach-joint! attach-constraint! attach-simple-constraint! attach-fixed-bone!
         joint-ref dynamic-joint
         new-skeleton update-skeleton skeleton-joints skeleton-scale
         skeleton-load skeleton-save skeleton-lock!
         scale*)

(define-type SkeletonDef skeleton-def)
(define-type Skeleton skeleton)
(define-type RealJointRef Nonnegative-Integer)
(define-type DerivedJointRef (-> Scale (-> JointRef Vector2D) Vector2D))
(define-type JointRef (U RealJointRef DerivedJointRef))
(define-type BoneRef (Pairof JointRef JointRef))
(define-type Scale Positive-Real)
(define-type Constraint (-> Skeleton Void))
(define-type SimpleConstraint (-> Vector2D Skeleton Vector2D))

(struct skeleton-def ([rev-joint-inits : (Listof Vector2D)]
                      [default-scale : Scale]
                      [rev-constraints : (Listof Constraint)]
                      [locked : Boolean]) #:mutable)
(struct skeleton ([joints : (Listof (Mutable Vector2D))] [scale : (Mutable Scale)] [constraints : Constraint]))

(: scale* (-> (U Scale Skeleton) Scale Scale))
(define (scale* a b)
  (if (skeleton? a)
      (scale* (mut-get (skeleton-scale a)) b)
      (let ((nnr (* a b)))
        (if (positive? nnr) nnr
            (error "scale dropped to zero")))))

(: new-skeleton-def (-> Scale SkeletonDef))
(define (new-skeleton-def default-scale)
  (skeleton-def empty default-scale empty #f))

(: skeleton-lock! (-> SkeletonDef Void))
(define (skeleton-lock! skel)
  (set-skeleton-def-locked! skel #t))

(define-type EncodedSkeleton (List (Listof Vector2D) Scale))

(: skeleton-load (-> SkeletonDef EncodedSkeleton Skeleton))
(define (skeleton-load def enc)
  (unless (skeleton-def-locked def)
    (error "skeleton is not locked!"))
  (let* ((joints (map (inst mut-cell Vector2D) (first enc)))
         (constraints (merge-constraints (reverse (skeleton-def-rev-constraints def))))
         (scale ((inst mut-cell Scale) (second enc)))
         (skel (skeleton joints scale constraints)))
    (constraints skel)
    skel))

(: skeleton-save (-> Skeleton EncodedSkeleton))
(define (skeleton-save skel)
  (list (map (inst mut-get Vector2D) (skeleton-joints skel))
        (mut-get (skeleton-scale skel))))

(: joint-ref (-> Skeleton JointRef Vector2D))
(define (joint-ref skel joint)
  (joint-vm-ref (mut-get (skeleton-scale skel)) (skeleton-joints skel) joint))

#|(: joint-v-ref (-> Scale (Listof Vector2D) JointRef Vector2D))
(define (joint-v-ref scale skel joint)
  (let iter ((joint joint))
    (if (procedure? joint)
        (joint scale iter)
        (list-ref skel joint))))|#

(: joint-vm-ref (-> Scale (Listof (Mutable Vector2D)) JointRef Vector2D))
(define (joint-vm-ref scale skel joint)
  (let iter ((joint joint))
    (if (procedure? joint)
        (joint scale iter)
        (mut-get (list-ref skel joint)))))

(: merge-constraints (-> (Listof Constraint) Constraint))
(define ((merge-constraints constraints) sk)
  (apply-map constraints sk)
  (void))

(: attach-joint! (-> SkeletonDef Real Real RealJointRef))
(define (attach-joint! skel dx dy)
  (when (skeleton-def-locked skel)
    (error "skeleton is locked!"))
  (define new-joint-id (length (skeleton-def-rev-joint-inits skel)))
  (set-skeleton-def-rev-joint-inits! skel (cons (vec dx dy) (skeleton-def-rev-joint-inits skel)))
  new-joint-id)

(define-syntax-rule (dynamic-joint scale (base ...) proc)
  (lambda ([scale : Scale] [lookup : (-> JointRef Vector2D)])
    (let ((base (lookup (ann base JointRef))) ...)
      (ann proc Vector2D))))

(: assert-valid-joint (-> SkeletonDef JointRef Void))
(define (assert-valid-joint skel joint)
  (unless (or (procedure? joint) (< joint (length (skeleton-def-rev-joint-inits skel))))
    (error "Joint is not yet defined: " joint)))

(: attach-constraint! (-> SkeletonDef Constraint Void))
(define (attach-constraint! skel const)
  (when (skeleton-def-locked skel)
    (error "skeleton is locked!"))
  (set-skeleton-def-rev-constraints! skel (cons const (skeleton-def-rev-constraints skel))))

(: attach-simple-constraint! (-> SkeletonDef RealJointRef SimpleConstraint Void))
(define (attach-simple-constraint! skel joint constraint)
  (assert-valid-joint skel joint)
  (attach-constraint! skel
                      (lambda ([sk : Skeleton])
                        (mut-set! (list-ref (skeleton-joints sk) joint)
                                  (constraint (joint-ref sk joint) sk)))))

(: fixed-distance (-> Vector2D Vector2D Scale Vector2D))
(define (fixed-distance variant invariant distance)
  (v+ invariant (vscale (v- variant invariant) distance)))

(: attach-fixed-bone! (-> SkeletonDef RealJointRef JointRef Scale BoneRef))
(define (attach-fixed-bone! skel variable invariable scale-multiplier)
  (assert-valid-joint skel invariable)
  (attach-simple-constraint! skel variable
                             (lambda ([var : Vector2D] [sk : Skeleton])
                               (fixed-distance var (joint-ref sk invariable) (scale* sk scale-multiplier))))
  (cons variable invariable))

(: new-skeleton (-> SkeletonDef Real Real Skeleton))
(define (new-skeleton skel x y)
  (unless (skeleton-def-locked skel)
    (error "skeleton is not locked!"))
  (let* ((joints (for/list : (Listof (Mutable Vector2D))
                   ((def-xy (reverse (skeleton-def-rev-joint-inits skel))))
                   (mut-cell (v+ def-xy (vec x y)))))
         (constraints (merge-constraints (reverse (skeleton-def-rev-constraints skel))))
         (scale ((inst mut-cell Scale) (skeleton-def-default-scale skel)))
         (sk (skeleton joints scale constraints)))
    (constraints sk)
    sk))

(: update-skeleton (-> Skeleton Void))
(define (update-skeleton skel)
  ((skeleton-constraints skel) skel))
