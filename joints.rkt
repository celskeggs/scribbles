#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "setting.rkt")
(require "setting-group.rkt")

(provide JointsetDef LockedJointsetDef Jointset JointRef RealJointRef DerivedJointRef BoneRef Scale EncodedJointset
         jointset-def-new attach-joint! attach-joint-rel! attach-setting!
         joint-ref dynamic-joint
         jointset-new jointset-handles jointset-handle-ref jointset-scale jointset-settings
         jointset-load jointset-save jointset-lock
         assert-valid-joint
         scale*)

(define-type JointsetDef jointset-def)
(define-type LockedJointsetDef locked-jointset-def)
(define-type Jointset jointset)
(define-type EncodedJointset (List (Listof Vector2D) EncodedSettingGroup))
(define-type RealJointRef Nonnegative-Integer)
(define-type DerivedJointRef (-> Jointset Vector2D))
(define-type JointRef (U RealJointRef DerivedJointRef))
(define-type BoneRef (Pairof JointRef JointRef))
(define-type Scale Positive-Real)
(define-type HandleTX (-> (Mutable Vector2D) Jointset (Mutable Vector2D)))

(lockable-struct jointset-def locked-jointset-def
                 ([joint-inits : (Listof (Pairof Vector2D HandleTX))]
                  [default-settings : SettingPrototypeGroup]))
(struct jointset ([vecs : (Listof (Mutable Vector2D))]
                  [settings : SettingGroup]
                  [sdef : LockedJointsetDef]))

(: jointset-scale (-> Jointset Scale))
(define (jointset-scale js)
  ; TODO: don't cast
  (cast (setting->value (setting-group-slider-ref (jointset-settings js) "scale")) Scale))

(: jointset-option-ref (-> Jointset String Boolean))
(define (jointset-option-ref js name)
  (setting->value (setting-group-option-ref (jointset-settings js) name)))

(: jointset-slider-ref (-> Jointset String Real))
(define (jointset-slider-ref js name)
  (setting->value (setting-group-slider-ref (jointset-settings js) name)))

(: jointset-def-default-scale (-> JointsetDef Scale))
(define (jointset-def-default-scale js)
  (cast (setting-prototype->value (setting-prototype-group-slider-ref (jointset-def-default-settings js) "scale")) Scale))

(: locked-jointset-def-default-scale (-> LockedJointsetDef Scale))
(define (locked-jointset-def-default-scale js)
  (cast (setting-prototype->value (setting-prototype-group-slider-ref (locked-jointset-def-default-settings js) "scale")) Scale))

(: scale* (-> (U Scale Jointset) Scale Scale))
(define (scale* a b)
  (if (jointset? a)
      (scale* (jointset-scale a) b)
      (let ((nnr (* a b)))
        (if (positive? nnr) nnr
            (error "scale dropped to zero")))))

(: jointset-def-new (->* (Scale) (SettingPrototypeGroup) JointsetDef))
(define (jointset-def-new default-scale [default-settings setting-prototype-group-empty])
  (jointset-def empty (setting-prototype-group-add (setting-positive-slider-proto "scale" 1 200 default-scale)
                                                   default-settings)))

(: jointset-lock (-> JointsetDef LockedJointsetDef))
(define (jointset-lock def)
  (locked-jointset-def (reverse (jointset-def-joint-inits def))
                       (jointset-def-default-settings def)))

(: joint-ref (-> Jointset JointRef Vector2D))
(define (joint-ref skel joint)
  (if (procedure? joint)
      (joint skel)
      (mut-get (jointset-handle-ref skel joint))))

(: base-htx HandleTX)
(define (base-htx v sk)
  v)

; TODO: make settings referenced by specific objects, not by names - it would be easier to type properly.
(: attach-setting! (-> JointsetDef SettingPrototype Void))
(define (attach-setting! js proto)
  (set-jointset-def-default-settings! js
                                      (setting-prototype-group-add proto
                                                                   (jointset-def-default-settings js))))

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

(define-syntax-rule (dynamic-joint scale (option ...) (slider ...) (base ...) proc)
  (lambda ([skel : Jointset])
    (let ((scale (jointset-scale skel))
          (option (jointset-option-ref skel (symbol->string 'option))) ...
          (slider (jointset-slider-ref skel (symbol->string 'slider))) ...
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
         (sk (jointset vecs (setting-group-instantiate (locked-jointset-def-default-settings def)) def)))
    (for ((prx joint-inits) (handle (jointset-handles sk)))
      (mut-set! handle (v+ base-vec (car prx))))
    sk))

(: jointset-load (-> LockedJointsetDef EncodedJointset Jointset))
(define (jointset-load def enc)
  (jointset (map (inst mut-cell Vector2D) (first enc))
            (setting-group-load (second enc) (locked-jointset-def-default-settings def))
            def))

(: jointset-save (-> Jointset EncodedJointset))
(define (jointset-save skel)
  (list (map (inst mut-get Vector2D) (jointset-vecs skel))
        (setting-group-save (jointset-settings skel) (locked-jointset-def-default-settings (jointset-sdef skel)))))
