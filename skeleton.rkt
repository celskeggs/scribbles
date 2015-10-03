#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "geometry.rkt")
(require "saving.rkt")

(provide SkeletonDef Skeleton
         JointRef RealJointRef DerivedJointRef BoneRef
         Scale
         Constraint SimpleConstraint
         EncodedSkeleton
         new-skeleton-def attach-joint! attach-joint-rel!
         attach-constraint! attach-simple-constraint! attach-simple-bone! attach-fixed-bone! attach-limited-bone!
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

(struct skeleton-def ([joint-inits : (Listof (Pairof Vector2D HandleTX))]
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
  (unless (skeleton-def-locked skel)
    (set-skeleton-def-joint-inits! skel (reverse (skeleton-def-joint-inits skel)))
    (set-skeleton-def-locked! skel #t)))

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
  (define new-joint-id (length (skeleton-def-joint-inits skel)))
  (set-skeleton-def-joint-inits! skel (cons (cons (vec dx dy) htx)
                                            (skeleton-def-joint-inits skel)))
  new-joint-id)

(: skeleton-handles (-> Skeleton (Listof (Mutable Vector2D))))
(define (skeleton-handles def)
  (for/list ((vec (skeleton-vecs def)) (init (skeleton-def-joint-inits (skeleton-sdef def))))
    ((cdr init) vec def)))

(: skeleton-handle-ref (-> Skeleton Nonnegative-Integer (Mutable Vector2D)))
(define (skeleton-handle-ref skel i)
  (let* ((vec (list-ref (skeleton-vecs skel) i))
         (inits (skeleton-def-joint-inits (skeleton-sdef skel)))
         (proc (cdr (list-ref inits i))))
    (proc vec skel)))

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
  (unless (or (procedure? joint) (< joint (length (skeleton-def-joint-inits skel))))
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

(: attach-simple-bone! (-> SkeletonDef RealJointRef JointRef Scale (-> Vector2D Vector2D Scale Vector2D) BoneRef))
(define (attach-simple-bone! skel variable invariable scale-multiplier processor)
  (assert-valid-joint skel invariable)
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

(: new-skeleton (-> SkeletonDef Real Real Skeleton))
(define (new-skeleton def x y)
  (unless (skeleton-def-locked def)
    (error "skeleton is not locked!"))
  (let* ((joint-inits (skeleton-def-joint-inits def))
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
  (let* ((joint-inits (skeleton-def-joint-inits def))
         (vecs (map (inst mut-cell Vector2D) (first enc)))
         (scale ((inst mut-cell Scale) (second enc)))
         (skel (skeleton vecs scale def)))
    (update-skeleton skel)
    skel))

(: update-skeleton (-> Skeleton Void))
(define (update-skeleton skel)
  (apply-map (reverse (skeleton-def-rev-constraints (skeleton-sdef skel))) skel)
  (void))
