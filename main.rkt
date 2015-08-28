#lang racket
(require "composition.rkt")
(require "functional-graphics.rkt")

(require "vector.rkt")

(r:define-style simple-style "black" 6 'solid "white" 'solid)

(composer example ((head 150 75) (collar 150 125) (pelvis 150 225))
          (fixed-distance head collar 50)
          (simple-style (r:line collar pelvis)
                        (r:line collar head)
                        (r:circle head 30)))

(example)
