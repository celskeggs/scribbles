#lang racket
(require racket/draw)

(require "utils.rkt")
(require "interactive-view.rkt")
(require "vector.rkt")

(define border-pen (new pen% [color "black"]))
(define regular-brush (new brush% [color (make-color 0 0 255)]))
(define hover-brush (new brush% [color (make-color 0 192 192)]))
(define drag-brush (new brush% [color (make-color 255 255 255)]))

(define-provide (handle-view render get-handles update-handle [width 300] [height 300] [handle-radius 6] [sel-box-size 30])
  
  (define handle-diameter (* 2 handle-radius))
  
  (define in-view-mode #f) ; in view mode, we only display the rendering result
  (define active-handle #f) ; the index of the handle that we're currently dragging
  (define drag-rel #f) ; the vector of the relative offset of the handle that we're dragging
  (define mouse-pos #f) ; the vector of the mouse's last position
  
  (define (hovering? handle-pos)
    (and mouse-pos (vin-circle? mouse-pos handle-pos handle-radius)))
  
  (define (try-drag-handle handle-id handle-pos) ; return #t to consume the event and and prevent additional calls
    (if (hovering? handle-pos)
        (begin
          (set! active-handle handle-id)
          (set! drag-rel (v- handle-pos mouse-pos))
          #t)
        #f))
  
  (define (draw-handle dc handle-id handle-pos)
    (send dc set-pen border-pen)
    (send dc set-brush (if (eqv? handle-id active-handle)
                           drag-brush
                           (if (hovering? handle-pos)
                               hover-brush
                               regular-brush)))
    (let-values (((hx hy) (vunpack handle-pos)))
      (send dc draw-ellipse (- hx handle-radius) (- hy handle-radius) handle-diameter handle-diameter)))
  
  (define (draw dc)
    (render dc)
    (unless in-view-mode
      (send dc set-pen border-pen)
      (send dc set-brush regular-brush)
      (let-values (((w h) (send dc get-size)))
        (send dc draw-rectangle (- w sel-box-size) 0 sel-box-size sel-box-size))
      (map (lambda (hpair)
             (draw-handle dc (car hpair) (cadr hpair)))
           (enumerate (get-handles)))))
  
  (define (press x y w h)
    (set! mouse-pos (v x y))
    (if (and (>= x (- w sel-box-size)) (<= y sel-box-size))
        (set! in-view-mode #t)
        (ormap (lambda (hpair) (try-drag-handle (car hpair) (cadr hpair))) (enumerate (get-handles)))))
  
  (define (update-dragging)
    (when active-handle
      (update-handle active-handle (v+ drag-rel mouse-pos))))
  
  (define (drag x y w h)
    (set! mouse-pos (v x y))
    (update-dragging))
  
  (define (move x y w h)
    (set! mouse-pos (v x y)))
  
  (define (release x y w h)
    (set! mouse-pos (v x y))
    (if in-view-mode
        (set! in-view-mode #f)
        (begin
          (update-dragging)
          (set! active-handle #f)
          (set! drag-rel #f))))
  
  (interactive-view draw press drag move release width height))
