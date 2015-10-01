#lang typed/racket
(require "pattern-stick-figure.rkt")
(require "pattern-box-figure.rkt")
(require "editor.rkt")

(editor-main (lambda (w h)
               (new-box-figure (/ w 2) (/ h 2))))
