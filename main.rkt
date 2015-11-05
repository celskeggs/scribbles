#lang typed/racket
(require "vector.rkt")
(require "pattern-stick-figure.rkt")
(require "pattern-box-figure.rkt")
(require "editor.rkt")
(require "profile.rkt")
(require racket/gui)

(profile-thunk
 (thunk
  (let ([new-es (make-eventspace)])
    (parameterize [(current-eventspace new-es)]
      (editor-main (lambda (w h)
                     (new-box-figure (vec (/ w 2.0) (/ h 2.0)))))
      (sleep 20))))
 #:threads #t)
