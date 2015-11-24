#lang typed/racket
(require/typed "worker.rkt"
               [dropping-worker (All (Item) (-> (-> Item Void) (-> Item Void)))])

(require "functional-graphics.rkt")

(provide MouseFunc RendererFunc gui-basic)

(define-type MouseFunc (-> Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Void))

(: gui-basic (-> RendererFunc MouseFunc MouseFunc MouseFunc MouseFunc Positive-Integer Positive-Integer String Void))
(define (gui-basic provide-renderer press drag move release width height title)
  (: renderer (U #f Renderer))
  (define renderer #f)
  (define-type RenderWorker (-> (List Nonnegative-Integer Nonnegative-Integer) Void))
  (: wrap-event (-> (-> Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Void) (-> RenderWorker Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Void)))
  (define ((wrap-event f) worker x y w h)
    (f x y w h)
    (worker (list w h)))
  ((inst wd:window-and-canvas RenderWorker) title width height
                        (lambda ([refresh : (-> Void)]) : RenderWorker
                          (dropping-worker
                           (lambda ([wh : (List Nonnegative-Integer Nonnegative-Integer)])
                             (set! renderer (provide-renderer (car wh) (cadr wh)))
                             (refresh))))
                        (wrap-event press)
                        (wrap-event release)
                        (wrap-event drag)
                        (wrap-event move)
                        (lambda ([worker : RenderWorker] [context : Context] [w : Positive-Integer] [h : Positive-Integer])
                          (define render renderer)
                          (if render
                              (r:render-to render context)
                              (worker (list w h))))))
