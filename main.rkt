#lang typed/racket
(require "pattern-stick-figure.rkt")
(require "editor.rkt")

(editor-main (lambda (w h)
               (make-stick-figure (/ w 2) (/ h 2))))
