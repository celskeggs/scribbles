#lang typed/racket
(require "utils.rkt")
(require "body.rkt")
(require "functional-graphics.rkt")
(require "composition.rkt")
(require "saving.rkt")

(provide World world-new world-add world-style-ref world-body-ref world-scene ButtonPressFunc)

(define-type World world)
(struct world ([style : Style] [bodies : (Listof Body)]) #:mutable)

(: world-new (-> Style World))
(define (world-new style)
  (world style empty))

(: world-add (-> World Body (-> Void)))
(define (world-add world body)
  (set-world-bodies! world (cons body (world-bodies world)))
  (define already #f)
  (lambda () (unless already (set! already #f) (set-world-bodies! world (without (world-bodies world) body)))))

(: world-style-ref (-> World Style))
(define (world-style-ref world)
  (: app (-> Renderer * Renderer))
  (define (app . renders)
    (apply (world-style world) renders))
  app)

(: world-body-ref (-> World Body))
(define (world-body-ref world)
  (body-merge (lambda ()
                (reverse (world-bodies world)))))

(: world-scene (->* (World Positive-Integer Positive-Integer) ((Listof (Pairof String ButtonPressFunc))) Void))
(define (world-scene world w h [buttons empty])
  (compose-scene (world-style-ref world)
                 (world-body-ref world)
                 buttons
                 w h))

;(: world-save (-> 