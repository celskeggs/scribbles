#lang typed/racket
(require "vector.rkt")
(require "functional-graphics.rkt")
(require "joints.rkt")
(require "skeleton.rkt")
(require "pattern-base.rkt")

(provide new-stick-figure)

(define jts (jointset-def-new 100.0))
(define skel (skeleton-def-new jts))
(define pat (pattern-def-new skel (r:wrap-style "black" 6 "white")))

(define head (attach-joint! jts 0.0 -1.0))
(define collar (attach-joint! jts 0.0 0.0))
(define pelvis (attach-joint! jts 0.0 1.0))
(define left-elbow (attach-joint! jts -1.0 1.0))
(define left-hand (attach-joint! jts 0.0 1.0))
(define right-elbow (attach-joint! jts 1.0 1.0))
(define right-hand (attach-joint! jts 0.0 1.0))
(define left-knee (attach-joint! jts -1.0 1.0))
(define left-foot (attach-joint! jts 0.0 1.0))
(define right-knee (attach-joint! jts 1.0 1.0))
(define right-foot (attach-joint! jts 0.0 1.0))

(: bones (Listof BoneRef))
(define bones
  (list
   (attach-fixed-bone! skel head collar 0.7)
   (attach-fixed-bone! skel pelvis collar 1.0)
   (attach-fixed-bone! skel left-elbow collar 0.5)
   (attach-fixed-bone! skel left-hand left-elbow 0.5)
   (attach-fixed-bone! skel right-elbow collar 0.5)
   (attach-fixed-bone! skel right-hand right-elbow 0.5)
   (attach-fixed-bone! skel left-knee pelvis 0.6)
   (attach-fixed-bone! skel left-foot left-knee 0.6)
   (attach-fixed-bone! skel right-knee pelvis 0.6)
   (attach-fixed-bone! skel right-foot right-knee 0.6)))

(attach-circle! pat head 0.5)

(void (map (curry attach-line-bone! pat) bones))

(define new-stick-figure (pattern-constructor (pattern-lock pat 'stick-figure-basic)))
