#lang typed/racket
(require "vector.rkt")
(require "functional-graphics.rkt")
(require "geometry.rkt")
(require "setting.rkt")
(require "skeleton.rkt")
(require "joints.rkt")
(require "pattern-base.rkt")

(provide autolimb! parallel-segment rotator-bearing!)

(: autolimb! (->* (PatternDef JointRef Positive-Float String Float Float) (Boolean Style) JointRef))
(define (autolimb! body root length direction-option default-x default-y [flip-direction #f] [style r:all])
  (define skeleton (pattern-def-skeleton body))
  (define joints (skeleton-def-jdef skeleton))

  (define half-length (/ length 2.0))

  (attach-setting! joints (setting-option-proto direction-option #f))

  (define endjoint (attach-joint-rel! joints default-x default-y root))
  (define midjoint (dynamic-joint-by-name scale (direction-option) () (root endjoint)
                                          (hypot-known-legs root endjoint (* scale (if (xor direction-option flip-direction) (- half-length) half-length)))))

  (attach-limited-bone! skeleton endjoint root length)

  (attach-bspline! body root midjoint endjoint 0.5 style)

  endjoint)

(: parallel-point (-> JointRef JointRef Float JointRef))
(define (parallel-point on-line off-line shift)
  (dynamic-joint scale () () (on-line off-line)
                 (v+ on-line (vscale (vrotate-origin-deg (v- off-line on-line) 90.0)
                                     (* shift scale)))))

(: parallel-segment (-> JointRef JointRef Scale (Values JointRef JointRef)))
(define (parallel-segment on-line off-line width)
  (define half-width (/ width 2.0))
  (values (parallel-point on-line off-line half-width)       ; right on screen; left on character
          (parallel-point on-line off-line (- half-width)))) ; left on screen; right on character

(: rotate-joint (-> JointRef JointRef Float JointRef))
(define (rotate-joint outer center degrees)
  (dynamic-joint scale () () (outer center)
                 (vrotate-deg outer center degrees)))

(: rotator-bearing! (-> SkeletonDef JointRef Scale (Values JointRef JointRef JointRef JointRef))) ; values: north east south west
(define (rotator-bearing! skel center radius)
  (define orig-north-joint (dynamic-joint scale () () (center)
                                          (v- center (vec 0.0 (scale* scale radius)))))
  (define north (attach-joint-rel! (skeleton-def-jdef skel) 0.0 -1.0 orig-north-joint))
  (attach-fixed-bone! skel north center radius)
  (values north
          (rotate-joint north center 90.0)
          (rotate-joint north center 180.0)
          (rotate-joint north center 270.0)))
