#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "geometry.rkt")
(require "setting-group.rkt")
(require "joints.rkt")

(provide SkeletonDef LockedSkeletonDef Skeleton Constraint SimpleConstraint EncodedSkeleton
         skeleton-def-new
         attach-constraint! attach-simple-constraint! attach-simple-bone! attach-fixed-bone! attach-limited-bone!
         skeleton-new update-skeleton skeleton-handles skeleton-settings
         skeleton-js
         skeleton-load skeleton-save skeleton-lock)

(define-type LockedSkeletonDef locked-skeleton-def)
(define-type SkeletonDef skeleton-def)
(define-type Skeleton skeleton)
(define-type Constraint (-> Jointset Void))
(define-type SimpleConstraint (-> Vector2D Jointset Vector2D))
(define-type EncodedSkeleton EncodedJointset)

(struct skeleton-def ([jdef : JointsetDef] [constraints : (Listof Constraint)]) #:mutable)
(struct locked-skeleton-def ([jdef : LockedJointsetDef] [constraints : (Listof Constraint)]))
(struct skeleton ([js : Jointset] [sdef : LockedSkeletonDef]))

(: skeleton-def-new (-> JointsetDef SkeletonDef))
(define (skeleton-def-new jointset-def)
  (skeleton-def jointset-def empty))

(: skeleton-lock (-> SkeletonDef LockedSkeletonDef))
(define (skeleton-lock def)
  (locked-skeleton-def (jointset-lock (skeleton-def-jdef def)) (reverse (skeleton-def-constraints def))))

(: skeleton-save (-> Skeleton EncodedSkeleton))
(define (skeleton-save skel)
  (jointset-save (skeleton-js skel)))

(: attach-constraint! (-> SkeletonDef Constraint Void))
(define (attach-constraint! skel const)
  (set-skeleton-def-constraints! skel (cons const (skeleton-def-constraints skel))))

(: attach-simple-constraint! (-> SkeletonDef RealJointRef SimpleConstraint Void))
(define (attach-simple-constraint! skel joint constraint)
  (assert-valid-joint (skeleton-def-jdef skel) joint)
  (attach-constraint! skel
                      (lambda ([sk : Jointset])
                        (mut-set! (jointset-handle-ref sk joint)
                                  (constraint (joint-ref sk joint) sk)))))

(: attach-simple-bone! (-> SkeletonDef RealJointRef JointRef Scale (-> Vector2D Vector2D Scale Vector2D) BoneRef))
(define (attach-simple-bone! skel variable invariable scale-multiplier processor)
  (assert-valid-joint (skeleton-def-jdef skel) invariable)
  (attach-simple-constraint! skel variable
                             (lambda (var sk)
                               (processor var (joint-ref sk invariable) (scale* sk scale-multiplier))))
  (cons variable invariable))

(: attach-fixed-bone! (-> SkeletonDef RealJointRef JointRef Scale BoneRef))
(define (attach-fixed-bone! skel variable invariable scale-multiplier)
  (attach-simple-bone! skel variable invariable scale-multiplier fixed-distance))

(: attach-limited-bone! (-> SkeletonDef RealJointRef JointRef Scale BoneRef))
(define (attach-limited-bone! skel variable invariable scale-multiplier)
  (attach-simple-bone! skel variable invariable scale-multiplier maximum-distance))

(: skeleton-new (-> LockedSkeletonDef Vector2D Skeleton))
(define (skeleton-new def base-vec)
  (let* ((jset (jointset-new (locked-skeleton-def-jdef def) base-vec))
         (skel (skeleton jset def)))
    (update-skeleton skel)
    skel))

(: skeleton-load (-> LockedSkeletonDef EncodedSkeleton Skeleton))
(define (skeleton-load def enc)
  (let* ((jset (jointset-load (locked-skeleton-def-jdef def) enc))
         (skel (skeleton jset def)))
    (update-skeleton skel)
    skel))

(: update-skeleton (-> Skeleton Void))
(define (update-skeleton skel)
  (apply-map (locked-skeleton-def-constraints (skeleton-sdef skel)) (skeleton-js skel))
  (void))

(: skeleton-handles (-> Skeleton (Listof (Mutable Vector2D))))
(define (skeleton-handles skel)
  (jointset-handles (skeleton-js skel)))

(: skeleton-settings (-> Skeleton SettingGroup))
(define (skeleton-settings skel)
  (jointset-settings (skeleton-js skel)))
