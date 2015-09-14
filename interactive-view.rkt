#lang typed/racket
(require typed/racket/gui)
(require typed/racket/draw)

(provide DrawFunc MouseFunc interactive-view)

(define-type DrawFunc (-> (Instance DC<%>) Void))
(define-type MouseFunc (-> Integer Integer Nonnegative-Integer Nonnegative-Integer Void))

(: interactive-view (->* (DrawFunc MouseFunc MouseFunc MouseFunc MouseFunc) (Positive-Integer Positive-Integer String) Void))
(define (interactive-view draw press drag move release [width 300] [height 300] [title "Interactive View"])
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
