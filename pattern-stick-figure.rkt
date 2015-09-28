#lang typed/racket
(require "vector.rkt")
(require "functional-graphics.rkt")
(require "skeleton.rkt")
(require "pattern-base.rkt")

(provide new-stick-figure)

(define skel (new-skeleton-def 100))
(define pat (new-pattern-def skel (r:wrap-style "black" 6 'solid "white" 'solid)))

(define head (attach-joint! skel 0 -1))
(define collar (attach-joint! skel 0 0))
(define pelvis (attach-joint! skel 0 1))
(define left-elbow (attach-joint! skel -1 1))
(define left-hand (attach-joint! skel 0 1))
(define right-elbow (attach-joint! skel 1 1))
(define right-hand (attach-joint! skel 0 1))
(define left-knee (attach-joint! skel -1 1))
(define left-foot (attach-joint! skel 0 1))
(define right-knee (attach-joint! skel 1 1))
(define right-foot (attach-joint! skel 0 1))

(: bones (Listof BoneRef))
(define bones
  (list
   (attach-fixed-bone! skel head collar 0.7)
   (attach-fixed-bone! skel pelvis collar 1)
   (attach-fixed-bone! skel left-elbow collar 0.5)
   (attach-fixed-bone! skel left-hand left-elbow 0.5)
   (attach-fixed-bone! skel right-elbow collar 0.5)
   (attach-fixed-bone! skel right-hand right-elbow 0.5)
   (attach-fixed-bone! skel left-knee pelvis 0.6)
   (attach-fixed-bone! skel left-foot left-knee 0.6)
   (attach-fixed-bone! skel right-knee pelvis 0.6)
   (attach-fixed-bone! skel right-foot right-knee 0.6)))

(attach-renderer! pat (lambda ([vecs : (Listof Vector2D)] [scale : Scale])
                        (r:circle (list-ref vecs head) (scale* scale 0.5))))

(: line-renderer (-> BoneRef Void))
(define (line-renderer br)
  (attach-renderer! pat (lambda ([vecs : (Listof Vector2D)] [scale : Scale])
                          (r:line (list-ref vecs (car br)) (list-ref vecs (cdr br))))))

(map line-renderer bones)

(define new-stick-figure (pattern-constructor pat))
