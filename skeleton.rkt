#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "saving.rkt")

(provide SkeletonDef Skeleton
         JointRef RealJointRef DerivedJointRef BoneRef
         Scale
         Constraint SimpleConstraint
         EncodedSkeleton
         new-skeleton-def attach-joint! attach-joint-rel! attach-constraint! attach-simple-constraint! attach-fixed-bone!
         joint-ref dynamic-joint
         new-skeleton update-skeleton skeleton-handles skeleton-scale
         skeleton-load skeleton-save skeleton-lock!
         scale*)

(define-type SkeletonDef skeleton-def)
(define-type Skeleton skeleton)
(define-type RealJointRef Nonnegative-Integer)
(define-type DerivedJointRef (-> Skeleton Vector2D))
(define-type JointRef (U RealJointRef DerivedJointRef))
(define-type BoneRef (Pairof JointRef JointRef))
(define-type Scale Positive-Real)
(define-type Constraint (-> Skeleton Void))
(define-type SimpleConstraint (-> Vector2D Skeleton Vector2D))
(define-type HandleTX (-> (Mutable Vector2D) Skeleton (Mutable Vector2D)))

(struct skeleton-def ([rev-joint-inits : (Listof (Pairof Vector2D HandleTX))]
                      [default-scale : Scale]
                      [rev-constraints : (Listof Constraint)]
                      [locked : Boolean]) #:mutable)
(struct skeleton ([vecs : (Listof (Mutable Vector2D))]
                  [scale : (Mutable Scale)]
                  [sdef : SkeletonDef]))

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

(: skeleton-save (-> Skeleton EncodedSkeleton))
(define (skeleton-save skel)
  (list (map (inst mut-get Vector2D) (skeleton-vecs skel))
        (mut-get (skeleton-scale skel))))

(: joint-ref (-> Skeleton JointRef Vector2D))
(define (joint-ref skel joint)
  (if (procedure? joint)
      (joint skel)
      (mut-get (skeleton-handle-ref skel joint))))

(: base-htx HandleTX)
(define (base-htx v sk)
  v)

(: attach-joint! (->* (SkeletonDef Real Real) (HandleTX) RealJointRef))
(define (attach-joint! skel dx dy [htx base-htx])
  (when (skeleton-def-locked skel)
    (error "skeleton is locked!"))
  (define new-joint-id (length (skeleton-def-rev-joint-inits skel)))
  (set-skeleton-def-rev-joint-inits! skel (cons (cons (vec dx dy) htx)
                                                (skeleton-def-rev-joint-inits skel)))
  new-joint-id)

(: skeleton-handles (-> Skeleton (Listof (Mutable Vector2D))))
(define (skeleton-handles skel)
  (for/list ((vec (skeleton-vecs skel)) (init (reverse (skeleton-def-rev-joint-inits (skeleton-sdef skel)))))
    ((cdr init) vec skel)))

(: skeleton-handle-ref (-> Skeleton Nonnegative-Integer (Mutable Vector2D)))
(define (skeleton-handle-ref skel i)
  (let ((vec (list-ref (skeleton-vecs skel) i)) (init (list-ref (reverse (skeleton-def-rev-joint-inits (skeleton-sdef skel))) i)))
    ((cdr init) vec skel)))

(: attach-joint-rel! (-> SkeletonDef Real Real JointRef RealJointRef))
(define (attach-joint-rel! skel dx dy base-joint)
  (attach-joint! skel dx dy (lambda ([base : (Mutable Vector2D)] [skel : Skeleton])
                              (mut-make (lambda () (v+ (joint-ref skel base-joint) (mut-get base)))
                                        (lambda ([v : Vector2D])
                                          (mut-set! base (v- v (joint-ref skel base-joint))))))))

(define-syntax-rule (dynamic-joint scale (base ...) proc)
  (lambda ([skel : Skeleton])
    (let ((scale (mut-get (skeleton-scale skel)))
          (base (joint-ref skel (ann base JointRef))) ...)
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
                        (mut-set! (skeleton-handle-ref sk joint)
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
(define (new-skeleton def x y)
  (unless (skeleton-def-locked def)
    (error "skeleton is not locked!"))
  (let* ((joint-inits (reverse (skeleton-def-rev-joint-inits def)))
         (vecs (many/list : (Mutable Vector2D) (length joint-inits)
                          (mut-cell (vec 0 0))))
         (scale ((inst mut-cell Scale) (skeleton-def-default-scale def)))
         (sk (skeleton vecs scale def)))
    (for ((prx joint-inits) (handle (skeleton-handles sk)))
      (mut-set! handle (v+ (vec x y) (car prx))))
    (update-skeleton sk)
    sk))

(: skeleton-load (-> SkeletonDef EncodedSkeleton Skeleton))
(define (skeleton-load def enc)
  (unless (skeleton-def-locked def)
    (error "skeleton is not locked!"))
  (let* ((joint-inits (reverse (skeleton-def-rev-joint-inits def)))
         (vecs (map (inst mut-cell Vector2D) (first enc)))
         (scale ((inst mut-cell Scale) (second enc)))
         (skel (skeleton vecs scale def)))
    (update-skeleton skel)
    skel))

(: update-skeleton (-> Skeleton Void))
(define (update-skeleton skel)
  (apply-map (reverse (skeleton-def-rev-constraints (skeleton-sdef skel))) skel)
  (void))
