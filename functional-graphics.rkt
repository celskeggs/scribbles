#lang typed/racket
(require typed/racket/draw)

(require "vector.rkt")

(provide Renderer Style Color RendererFunc
         r:pen r:brush r:style r:wrap-style
         r:all
         r:circle r:line r:rect r:poly
         r:text r:blank
         r:render-to r:save-to r:contains
         [rename-out (make-color r:color)]
         rf:compose)

(define-type RendererFunc (-> Nonnegative-Integer Nonnegative-Integer Renderer))

(define-type Color (U String (Instance Color%)))
(define-type Renderer (Pairof (-> (Instance DC<%>) Void) (-> Real Real Boolean)))
(define-type Style (-> Renderer * Renderer))

(define-syntax-rule (genren (dc x y) render check)
  (cons (lambda ([dc : (Instance DC<%>)])
          render)
        (lambda ([x : Real] [y : Real])
          check)))

(: r:wrap-style (-> Color Positive-Integer Pen-Style Color Brush-Style Style))
(define ((r:wrap-style pen-color pen-width pen-style brush-color brush-style) . bodies)
  (r:style pen-color pen-width pen-style brush-color brush-style
           (apply r:all bodies)))

(: r:pen (-> Color Positive-Integer Pen-Style Renderer * Renderer))
(define (r:pen color width style . bodies)
  (genren (dc x y)
          (let ((orig-pen (send dc get-pen)))
            (send dc set-pen color width style)
            (for ((body : Renderer bodies))
              ((car body) dc))
            (send dc set-pen orig-pen))
          (for/or : Boolean ((body : Renderer bodies))
            ((cdr body) x y))))

(: r:brush (-> Color Brush-Style Renderer * Renderer))
(define (r:brush color style . bodies)
  (genren (dc x y)
          (let ((orig-brush (send dc get-brush)))
            (send dc set-brush color style)
            (for ((body : Renderer bodies))
              ((car body) dc))
            (send dc set-brush orig-brush))
          (for/or : Boolean ((body : Renderer bodies))
            ((cdr body) x y))))

(: r:style (-> Color Positive-Integer Pen-Style Color Brush-Style Renderer * Renderer))
(define (r:style pen-color pen-width pen-style brush-color brush-style . bodies)
  (r:pen pen-color pen-width pen-style
         (r:brush brush-color brush-style
                  (apply r:all bodies))))

(: r:line (-> Vector2D Vector2D Renderer))
(define (r:line v1 v2)
  (genren (dc x y)
          (send dc draw-line (vec-x v1) (vec-y v1) (vec-x v2) (vec-y v2))
          #f)) ; TODO: allow line collision detection?

(: r:circle (-> Vector2D Positive-Real Renderer))
(define (r:circle center rad)
  (genren (dc x y)
          (send dc draw-ellipse (- (vec-x center) rad) (- (vec-y center) rad) (* rad 2) (* rad 2))
          (vin-circle? (vec x y) center rad)))

(: r:rect (-> Vector2D Positive-Real Positive-Real Renderer))
(define (r:rect pos width height)
  (genren (dc x y)
          (send dc draw-rectangle (vec-x pos) (vec-y pos) width height)
          (vin-rectangle? (vec x y) pos (vec width height))))

(: r:poly (-> (Listof Vector2D) Renderer))
(define (r:poly points)
  (genren (dc x y)
          (send dc draw-polygon (map vec->pair points))
          #f)) ; TODO: allow polygon collision detection?

(: r:blank (-> Vector2D Positive-Real Positive-Real Renderer))
(define (r:blank pos width height)
  (genren (dc x y)
          (void)
          (vin-rectangle? (vec x y) pos (vec width height))))

(: r:text (-> Vector2D String Renderer))
(define (r:text pos text)
  (genren (dc x y)
          (send dc draw-text text (vec-x pos) (vec-y pos) #t)
          #f)) ; TODO: allow text collision detection?

(: r:all (-> Renderer * Renderer))
(define (r:all . bodies)
  (genren (dc x y)
          (for ((body : Renderer bodies))
            ((car body) dc))
          (for/or : Boolean ((body : Renderer bodies))
            ((cdr body) x y))))

(: r:contains (-> Renderer Real Real Boolean))
(define (r:contains rf x y)
  ((cdr rf) x y))

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
    ((car rf) dc)
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

(: rf:compose (-> (Listof RendererFunc) RendererFunc))
(define ((rf:compose funcs) w h)
  (apply r:all (for/list : (Listof Renderer) ((func funcs))
                 (func w h))))