#lang typed/racket
(require "vector.rkt")
(require "geometry.rkt")
(require "functional-graphics.rkt")
(require "joints.rkt")
(require "skeleton.rkt")
(require "setting.rkt")
(require "pattern-base.rkt")

; TEMP
(require "bezier-fit.rkt")

(provide new-box-figure)

(define jts (jointset-def-new 100.0))
(define skel (skeleton-def-new jts))
(define body-style (r:wrap-style "black" 6 (r:color 0 128 0)))
(define limb-style (r:wrap-style "black" 6 #f))
(define eye-style (r:wrap-style "black" 1 "black"))
(define nose-style (r:wrap-style "gray" 1 "gray"))
(define mouth-style (r:wrap-style "black" 2 "white"))
(define pat (pattern-def-new skel (r:wrap-style "black" 6 "white")))

(attach-setting! jts (setting-option-proto "line-face" #t))

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

(define neck (attach-joint! jts 0.0 -50.0))
(define head (attach-joint-rel! jts 0.0 -100.0 neck))
(define pelvis (dynamic-joint scale () () (head neck)
                              (v+ neck (vscale (v- neck head) scale))))
(define left-shoulder (dynamic-joint scale () () (head neck)
                                     (v+ neck (vscale (vrotate-origin-deg (v- head neck) 90.0)
                                                      (* scale 0.5)))))
(define right-shoulder (dynamic-joint scale () () (head neck)
                                      (v+ neck (vscale (vrotate-origin-deg (v- head neck) -90.0)
                                                       (* scale 0.5)))))
(define left-hip (dynamic-joint scale () () (neck pelvis)
                                (v+ pelvis (vscale (vrotate-origin-deg (v- neck pelvis) 90.0)
                                                   (* scale 0.5)))))
(define right-hip (dynamic-joint scale () () (neck pelvis)
                                 (v+ pelvis (vscale (vrotate-origin-deg (v- neck pelvis) -90.0)
                                                   (* scale 0.5)))))

(define face (attach-joint-rel! jts 0.0 -110.0 head))
(define orig-top-of-head (dynamic-joint scale () () (head)
                                   (v- head (vec 0.0 (scale* scale 0.7)))))
(define top-of-head (attach-joint-rel! jts 0.0 -1.0 orig-top-of-head))
(define right-of-head (dynamic-joint scale () () (head top-of-head)
                                   (vrotate-deg top-of-head head +90.0)))
(define left-of-head (dynamic-joint scale () () (head top-of-head)
                                   (vrotate-deg top-of-head head -90.0)))
(define mouth (dynamic-joint scale () () (face head top-of-head)
                             (translate-along-sphere head top-of-head face (scale* scale 0.4))))
(define left-mouth (dynamic-joint scale () () (mouth head left-of-head)
                                  (translate-along-sphere head left-of-head mouth (scale* scale 0.2))))
(define right-mouth (dynamic-joint scale () () (mouth head right-of-head)
                                   (translate-along-sphere head right-of-head mouth (scale* scale 0.2))))
(define left-eye (dynamic-joint scale () () (face head left-of-head)
                                (translate-along-sphere head left-of-head face (scale* scale 0.2))))
(define right-eye (dynamic-joint scale () () (face head right-of-head)
                                 (translate-along-sphere head right-of-head face (scale* scale 0.2))))

(void (attach-fixed-bone! skel head neck 0.5)
      (attach-fixed-bone! skel top-of-head head 0.7)
      (attach-limited-bone! skel face head 0.6))

(attach-poly! pat (list left-shoulder left-hip right-hip right-shoulder) body-style)

(attach-circle! pat head 0.7)
(attach-circle! pat left-eye 0.07 eye-style)
(attach-circle! pat right-eye 0.07 eye-style)

(attach-renderer! pat (ren-conditional "line-face"
                                       (ren-line left-mouth right-mouth)
                                       (ren-circle mouth 0.1 mouth-style)))

(autolimb! pat left-shoulder 0.8 "left-arm-up" 150.0 150.0 #t limb-style)
(autolimb! pat right-shoulder 0.8 "right-arm-up" -150.0 150.0 #f limb-style)
(autolimb! pat left-hip 0.8 "left-leg-up" 50.0 300.0 #t limb-style)
(autolimb! pat right-hip 0.8 "right-leg-up" -50.0 300.0 #f limb-style)

(define new-box-figure (pattern-constructor (pattern-lock pat 'box-figure-basic)))
