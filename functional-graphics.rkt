#lang typed/racket
(require typed/racket/draw)

(require "vector.rkt")

(provide Renderer Style
         r:pen r:brush r:style r:wrap-style
         r:all r:circle r:line
         r:render-to r:save-to)

(define-type Renderer (-> (Instance DC<%>) Void))
(define-type Style (-> Renderer * Renderer))

(: r:wrap-style (-> String Positive-Integer Pen-Style String Brush-Style Style))
(define ((r:wrap-style pen-color pen-width pen-style brush-color brush-style) . bodies)
  (r:style pen-color pen-width pen-style brush-color brush-style
           (apply r:all bodies)))

(: r:pen (-> String Positive-Integer Pen-Style Renderer * Renderer))
(define ((r:pen color width style . bodies) dc)
  (let ((orig-pen (send dc get-pen)))
    (send dc set-pen color width style)
    (for ((body bodies))
      (body dc))
    (send dc set-pen orig-pen)))

(: r:brush (-> String Brush-Style Renderer * Renderer))
(define ((r:brush color style . bodies) dc)
  (let ((orig-brush (send dc get-brush)))
    (send dc set-brush color style)
    (for ((body bodies))
      (body dc))
    (send dc set-brush orig-brush)))

(: r:style (-> String Positive-Integer Pen-Style String Brush-Style Renderer * Renderer))
(define (r:style pen-color pen-width pen-style brush-color brush-style . bodies)
  (r:pen pen-color pen-width pen-style
         (r:brush brush-color brush-style
                  (apply r:all bodies))))

(: r:line (-> Vector2D Vector2D Renderer))
(define ((r:line v1 v2) dc)
  (send dc draw-line (vec-x v1) (vec-y v1) (vec-x v2) (vec-y v2)))

(: r:circle (-> Vector2D Positive-Integer Renderer))
(define ((r:circle center rad) dc)
  (send dc draw-ellipse (- (vec-x center) rad) (- (vec-y center) rad) (* rad 2) (* rad 2)))

(: r:all (-> Renderer * Renderer))
(define ((r:all . bodies) dc)
  (for ((body bodies))
    (body dc)))

(: r:render-to (-> Renderer (Instance DC<%>) Void))
(define (r:render-to rf dc)
  (let ((orig-brush (send dc get-brush))
        (orig-pen (send dc get-pen))
        (orig-smoothing (send dc get-smoothing))
        (orig-transform (send dc get-transformation)))
    (send dc set-brush "green" 'solid)
    (send dc set-pen "black" 1 'solid)
    (send dc set-rotation 0)
    (send dc set-scale 1 1)
    (send dc set-smoothing 'aligned)
    (rf dc)
    (send dc set-brush orig-brush)
    (send dc set-pen orig-pen)
    (send dc set-smoothing orig-smoothing)
    (send dc set-transformation orig-transform)))

(: r:save-to (->* (Renderer Positive-Integer Positive-Integer String) ((U 'png 'jpeg 'xbm 'xpm 'bmp)) Void))
(define (r:save-to rf w h file [kind 'png])
  (let* ((bmp (make-bitmap w h))
         (dc (send bmp make-dc)))
    (send dc set-brush "white" 'solid)
    (send dc set-pen "white" 1 'solid)
    (send dc draw-rectangle 0 0 w h)
    (r:render-to rf dc)
    (send bmp save-file file kind)
    (void)))
