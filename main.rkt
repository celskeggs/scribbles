#lang typed/racket
(require "functional-graphics.rkt")
(require "pattern-stick-figure.rkt")
(require "world.rkt")

(define world (world-new (r:wrap-style "black" 6 'solid "white" 'solid)))
(world-add world (new-stick-figure 120 200))
(world-add world (new-stick-figure 380 200))
(world-scene world 500 500
             (list (cons "green" (lambda (w h) (world-add world (new-stick-figure 300 300)) (void)))))
