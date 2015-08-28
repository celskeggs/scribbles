#lang racket
(require racket/gui)

(require "utils.rkt")

(define-provide (interactive-view draw press drag move release [title "Interactive View"] [width 300] [height 300])
  (define frame (new frame% [label title] [width width] [height height]))
  (define my-canvas%
    (class canvas%
      (inherit get-dc get-width get-height)
      
      (define/override (on-event event)
        ((case (send event get-event-type)
           ((left-down) press)
           ((motion) (if (send event get-left-down) drag move))
           ((left-up) release)
           (else void)) (send event get-x) (send event get-y) (get-width) (get-height))
        (send this refresh))
      
      (define/override (on-paint)
        (draw (get-dc)))
      
      (super-new)))
  (new my-canvas% [parent frame])
  (send frame center 'both)
  (send frame show #t))
