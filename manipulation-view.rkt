#lang typed/racket
(require typed/racket/draw)

(require "utils.rkt")
(require "interactive-view.rkt")
(require "vector.rkt")

(provide RenderFunc GetHandleFunc UpdateHandleFunc ButtonPressFunc
         handle-view)

(: border-pen (Instance Pen%))
(define border-pen (new pen% [color "black"]))
(: regular-brush (Instance Brush%))
(define regular-brush (new brush% [color (make-color 0 0 255)]))
(: hover-brush (Instance Brush%))
(define hover-brush (new brush% [color (make-color 0 192 192)]))
(: drag-brush (Instance Brush%))
(define drag-brush (new brush% [color (make-color 255 255 255)]))

(define-type RenderFunc (-> (Instance DC<%>) Void))
(define-type GetHandleFunc (-> (Listof Vector2D)))
(define-type UpdateHandleFunc (-> Nonnegative-Integer Vector2D Void))
(define-type ButtonPressFunc (-> Nonnegative-Integer Nonnegative-Integer Void)) 

(struct active-drag ([index : Nonnegative-Integer] [rel : Vector2D]) #:transparent)

(: handle-view (->* (RenderFunc GetHandleFunc UpdateHandleFunc (Listof (Pairof String ButtonPressFunc))) (Positive-Integer Positive-Integer Positive-Integer Positive-Integer) Void))
(define (handle-view render get-handles update-handle buttons [width 300] [height 300] [handle-radius 6] [sel-box-size 30])

  (: handle-diameter Positive-Integer)
  (define handle-diameter (* 2 handle-radius))

  (: in-view-mode Boolean)
  (define in-view-mode #f) ; in view mode, we only display the rendering result
  (: drag-status (U #f active-drag))
  (define drag-status #f) ; stores for the currently-dragging handle: the index and the relative offset
  (: mouse-pos Vector2D)
  (define mouse-pos (vec 0 0)) ; the vector of the mouse's last position

  (: hovering? (-> Vector2D Boolean))
  (define (hovering? handle-pos)
    (vin-circle? mouse-pos handle-pos handle-radius))

  (: try-drag-handle (-> Nonnegative-Integer Vector2D Boolean))
  (define (try-drag-handle handle-id handle-pos) ; return #t to consume the event and and prevent additional calls
    (if (hovering? handle-pos)
        (begin
          (set! drag-status (active-drag handle-id (v- handle-pos mouse-pos)))
          #t)
        #f))

  (: handle-brush (-> Nonnegative-Integer Vector2D (Instance Brush%)))
  (define (handle-brush id pos)
    (let ((drag drag-status))
      (if (and drag (eqv? id (active-drag-index drag)))
          drag-brush
          (if (hovering? pos)
              hover-brush
              regular-brush))))

  (: draw-handle (-> (Instance DC<%>) Nonnegative-Integer Vector2D Void))
  (define (draw-handle dc handle-id handle-pos)
    (send dc set-pen border-pen)
    (send dc set-brush (handle-brush handle-id handle-pos))
    (let-values (((hx hy) (vunpack handle-pos)))
      (send dc draw-ellipse (- hx handle-radius) (- hy handle-radius) handle-diameter handle-diameter)))

  (: draw DrawFunc)
  (define (draw dc)
    (render dc)
    (unless in-view-mode
      (send dc set-pen border-pen)
      (let-values (((w h) (send dc get-size)))
        (send dc set-brush regular-brush)
        (send dc draw-rectangle (- w sel-box-size) 0 sel-box-size sel-box-size)
        (for ((pair (enumerate buttons)))
          (send dc set-brush (cadr pair) 'solid)
          (send dc draw-rectangle (- w (* sel-box-size (+ 2 (car pair)))) 0 sel-box-size sel-box-size)))
      (send dc set-brush regular-brush)
      (map (lambda ([hpair : (Pairof Nonnegative-Integer Vector2D)])
             (draw-handle dc (car hpair) (cdr hpair)))
           (enumerate (get-handles)))
      (void)))

  (: press MouseFunc)
  (define (press x y w h)
    (set! mouse-pos (vec x y))
    (if (and (>= x (- w (* sel-box-size (+ 1 (length buttons))))) (<= y sel-box-size))
        (let ((index (exact-floor (/ (- w x) sel-box-size))))
          (if (= index 0)
              (set! in-view-mode #t)
              ((cdr (list-ref buttons (- index 1))) w h)))
        (ormap
         (lambda ([hpair : (Pairof Nonnegative-Integer Vector2D)])
           (try-drag-handle (car hpair) (cdr hpair)))
         (enumerate (get-handles))))
    (void)) ; void because we don't care about any results

  (: update-dragging (-> Void))
  (define (update-dragging)
    (let ((drag drag-status))
      (when drag
        (update-handle (active-drag-index drag) (v+ (active-drag-rel drag) mouse-pos)))))

  (: drag MouseFunc)
  (define (drag x y w h)
    (set! mouse-pos (vec x y))
    (update-dragging))

  (: move MouseFunc)
  (define (move x y w h)
    (set! mouse-pos (vec x y)))

  (: release MouseFunc)
  (define (release x y w h)
    (set! mouse-pos (vec x y))
    (if in-view-mode
        (set! in-view-mode #f)
        (begin
          (update-dragging)
          (set! drag-status #f))))
  
  (interactive-view draw press drag move release width height))
