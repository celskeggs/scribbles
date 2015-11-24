#lang typed/racket
(require "vector.rkt")
(require "pattern-stick-figure.rkt")
(require "pattern-box-figure.rkt")
(require "editor.rkt")
(require "profile.rkt")
(require racket/gui)

(editor-main (lambda (w h)
               (new-box-figure (vec (/ w 2.0) (/ h 2.0)))))
