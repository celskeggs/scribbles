#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")

(provide JointsetDef LockedJointsetDef Jointset JointRef RealJointRef DerivedJointRef BoneRef Scale EncodedJointset
         jointset-def-new attach-joint! attach-joint-rel!
         joint-ref dynamic-joint
         jointset-new jointset-handles jointset-handle-ref jointset-scale
         jointset-load jointset-save jointset-lock
         assert-valid-joint
         scale*)

(define-type JointsetDef jointset-def)
(define-type LockedJointsetDef locked-jointset-def)
(define-type Jointset jointset)
(define-type RealJointRef Nonnegative-Integer)
(define-type DerivedJointRef (-> Jointset Vector2D))
(define-type JointRef (U RealJointRef DerivedJointRef))
(define-type BoneRef (Pairof JointRef JointRef))
(define-type Scale Positive-Real)
(define-type HandleTX (-> (Mutable Vector2D) Jointset (Mutable Vector2D)))

(lockable-struct jointset-def locked-jointset-def
                 ([joint-inits : (Listof (Pairof Vector2D HandleTX))]
                  [default-scale : Scale]))
(struct jointset ([vecs : (Listof (Mutable Vector2D))]
                  [scale : (Mutable Scale)]
                  [sdef : LockedJointsetDef]))

(: scale* (-> (U Scale Jointset) Scale Scale))
(define (scale* a b)
  (if (jointset? a)
      (scale* (mut-get (jointset-scale a)) b)
      (let ((nnr (* a b)))
        (if (positive? nnr) nnr
            (error "scale dropped to zero")))))

(: jointset-def-new (-> Scale JointsetDef))
(define (jointset-def-new default-scale)
  (jointset-def empty default-scale))

(: jointset-lock (-> JointsetDef LockedJointsetDef))
(define (jointset-lock def)
  (locked-jointset-def (reverse (jointset-def-joint-inits def))
                       (jointset-def-default-scale def)))

(define-type EncodedJointset (List (Listof Vector2D) Scale))

(: joint-ref (-> Jointset JointRef Vector2D))
(define (joint-ref skel joint)
  (if (procedure? joint)
      (joint skel)
      (mut-get (jointset-handle-ref skel joint))))

(: base-htx HandleTX)
(define (base-htx v sk)
  v)

(: attach-joint! (->* (JointsetDef Real Real) (HandleTX) RealJointRef))
(define (attach-joint! skel dx dy [htx base-htx])
  (define new-joint-id (length (jointset-def-joint-inits skel)))
  (set-jointset-def-joint-inits! skel (cons (cons (vec dx dy) htx)
                                            (jointset-def-joint-inits skel)))
  new-joint-id)

(: jointset-handles (-> Jointset (Listof (Mutable Vector2D))))
(define (jointset-handles def)
  (for/list ((vec (jointset-vecs def)) (init (locked-jointset-def-joint-inits (jointset-sdef def))))
    ((cdr init) vec def)))

(: jointset-handle-ref (-> Jointset Nonnegative-Integer (Mutable Vector2D)))
(define (jointset-handle-ref skel i)
  (let* ((vec (list-ref (jointset-vecs skel) i))
         (inits (locked-jointset-def-joint-inits (jointset-sdef skel)))
         (proc (cdr (list-ref inits i))))
    (proc vec skel)))

(: attach-joint-rel! (-> JointsetDef Real Real JointRef RealJointRef))
(define (attach-joint-rel! skel dx dy base-joint)
  (attach-joint! skel dx dy (lambda ([base : (Mutable Vector2D)] [skel : Jointset])
                              (mut-make (lambda () (v+ (joint-ref skel base-joint) (mut-get base)))
                                        (lambda ([v : Vector2D])
                                          (mut-set! base (v- v (joint-ref skel base-joint))))))))

(define-syntax-rule (dynamic-joint scale (base ...) proc)
  (lambda ([skel : Jointset])
    (let ((scale (mut-get (jointset-scale skel)))
          (base (joint-ref skel (ann base JointRef))) ...)
      (ann proc Vector2D))))

(: assert-valid-joint (-> JointsetDef JointRef Void))
(define (assert-valid-joint skel joint)
  (unless (or (procedure? joint) (< joint (length (jointset-def-joint-inits skel))))
    (error "Joint is not yet defined: " joint)))

(: jointset-new (-> LockedJointsetDef Vector2D Jointset))
(define (jointset-new def base-vec)
  (let* ((joint-inits (locked-jointset-def-joint-inits def))
         (vecs (many/list : (Mutable Vector2D) (length joint-inits)
                          (mut-cell (vec 0 0))))
         (scale ((inst mut-cell Scale) (locked-jointset-def-default-scale def)))
         (sk (jointset vecs scale def)))
    (for ((prx joint-inits) (handle (jointset-handles sk)))
      (mut-set! handle (v+ base-vec (car prx))))
    sk))

(: jointset-load (-> LockedJointsetDef EncodedJointset Jointset))
(define (jointset-load def enc)
  (jointset (map (inst mut-cell Vector2D) (first enc))
            ((inst mut-cell Scale) (second enc))
            def))

(: jointset-save (-> Jointset EncodedJointset))
(define (jointset-save skel)
  (list (map (inst mut-get Vector2D) (jointset-vecs skel))
        (mut-get (jointset-scale skel))))
