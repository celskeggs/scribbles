#lang typed/racket
(require "pattern-stick-figure.rkt")
(require "editor.rkt")

(editor-main (lambda (w h)
               (new-stick-figure (/ w 2) (/ h 2))))
