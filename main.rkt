#lang racket
(require "composition.rkt")
(require "functional-graphics.rkt")

(r:define-style simple-style "black" 6 'solid "white" 'solid)

(composer example ((head 150 75) (collar 150 125) (pelvis 150 225))
          (simple-style (r:line collar pelvis)
                        (r:circle head 30)))

(example)
