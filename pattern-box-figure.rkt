#lang typed/racket
(require "vector.rkt")
(require "functional-graphics.rkt")
(require "skeleton.rkt")
(require "pattern-base.rkt")

(provide new-box-figure)

(define skel (new-skeleton-def 100))
(define body-style (r:wrap-style "black" 6 'solid (r:color 0 128 0) 'solid))
(define pat (new-pattern-def skel (r:wrap-style "black" 6 'solid "white" 'solid)))

(define head (attach-joint! skel 0 -100))
(define neck (attach-joint! skel 0 -50))
(define pelvis (dynamic-joint scale (head neck)
                              (v+ neck (vscale (v- neck head) scale))))
(define left-shoulder (dynamic-joint scale (head neck)
                                     (v+ neck (vscale (vrotate-origin-deg (v- head neck) -90)
                                                      (* scale 0.5)))))
(define right-shoulder (dynamic-joint scale (head neck)
                                      (v+ neck (vscale (vrotate-origin-deg (v- head neck) 90)
                                                       (* scale 0.5)))))
(define left-hip (dynamic-joint scale (neck pelvis)
                                (v+ pelvis (vscale (vrotate-origin-deg (v- neck pelvis) -90)
                                                   (* scale 0.5)))))
(define right-hip (dynamic-joint scale (neck pelvis)
                                 (v+ pelvis (vscale (vrotate-origin-deg (v- neck pelvis) 90)
                                                   (* scale 0.5)))))
(define left-elbow (attach-joint! skel -100 100))
(define left-hand (attach-joint! skel -150 150))
(define right-elbow (attach-joint! skel 100 100))
(define right-hand (attach-joint! skel 150 150))
(define left-foot (attach-joint! skel -50 300))
(define right-foot (attach-joint! skel 50 300))

(attach-fixed-bone! skel head neck 0.5)

(: bones (Listof BoneRef))
(define bones
  (list
   (attach-fixed-bone! skel left-elbow left-shoulder 0.4)
   (attach-fixed-bone! skel left-hand left-elbow 0.4)
   (attach-fixed-bone! skel right-elbow right-shoulder 0.4)
   (attach-fixed-bone! skel right-hand right-elbow 0.4)
   (attach-fixed-bone! skel left-foot left-hip 0.8)
   (attach-fixed-bone! skel right-foot right-hip 0.8)))

(attach-poly! pat (list left-shoulder left-hip right-hip right-shoulder) body-style)

(attach-circle! pat head 0.7)

(map (curry attach-line! pat) bones)

(lock-pattern! pat 'box-figure-basic)

(define new-box-figure (pattern-constructor pat))
