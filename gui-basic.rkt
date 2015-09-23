#lang typed/racket
(require typed/racket/gui)
(require typed/racket/draw)

(require "functional-graphics.rkt")

(provide MouseFunc RendererFunc gui-basic)

(define-type MouseFunc (-> Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Void))

(: gui-basic (-> RendererFunc MouseFunc MouseFunc MouseFunc MouseFunc Positive-Integer Positive-Integer String Void))
(define (gui-basic provide-renderer press drag move release width height title)
  (define frame (new frame% [label title] [width width] [height height]))
  (define my-canvas%
    (class canvas%
      (inherit get-dc get-width get-height)

      (: renderer (U #f Renderer))
      (define renderer #f)
      
      (define/override (on-event event)
        (let ((x (send event get-x))
              (y (send event get-y))
              (w (get-width))
              (h (get-height)))
          (unless (or (zero? w) (zero? h) (not (positive? x)) (not (positive? y)))
            ((case (send event get-event-type)
               ((left-down) press)
               ((motion) (if (send event get-left-down) drag move))
               ((left-up) release)
               (else void)) x y w h)
            (set! renderer (provide-renderer w h))
            (send this refresh))))
      
      (define/override (on-paint)
        (define render renderer)
        (define real-render
          (if render
              render
              (let ((got (provide-renderer (get-width) (get-height))))
                (set! renderer got)
                got)))
        (r:render-to real-render (get-dc)))
      
      (super-new)))
  (new my-canvas% [parent frame])
  (send frame center 'both)
  (send frame show #t))
