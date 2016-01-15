#lang typed/racket
(require racket/flonum)
(require "utils.rkt")
(require "vector.rkt")
(require "gui-basic.rkt")
(require "functional-graphics.rkt")

(provide ButtonFunc RendererFunc Control gui-handles control)

(define controls-style (r:wrap-style "black" 1.0 (r:color 0 0 255)))
(define select-style (r:wrap-style "black" 1.0 (r:color 0 128 255)))
(define hover-style (r:wrap-style "black" 1.0 (r:color 0 192 192)))
(define drag-style (r:wrap-style "black" 1.0 "white"))

(define-type ButtonFunc (-> Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Void))
(struct control ([render : RendererFunc] [draggable : Boolean] [press : ButtonFunc] [release : ButtonFunc]))
(define-type Control control)

(struct active-drag ([index : Nonnegative-Integer] [rel : Vector2D]) #:transparent)

(define handle-radius 6.0)

; Handles: pair of boolean (is selected?) and vector (center position)
(: gui-handles (-> RendererFunc (MutListOf (Mutable (Pairof Boolean Vector2D))) (MutListOf Control) (-> Boolean) Positive-Integer Positive-Integer String Void))
(define (gui-handles render-body handles controls in-view-mode? width height title)

  (: drag-status (U #f active-drag))
  (define drag-status #f) ; stores for the currently-dragging handle: the index and the relative offset
  (: mouse-pos Vector2D)
  (define mouse-pos (vec 0.0 0.0)) ; the vector of the mouse's last position

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

  (: handle-style (-> Nonnegative-Integer (Pairof Boolean Vector2D) Style))
  (define (handle-style id pos)
    (let ((drag drag-status))
      (if (and drag (= id (active-drag-index drag)))
          drag-style
          (if (hovering? (cdr pos))
              hover-style
              (if (car pos)
                  select-style
                  controls-style)))))

  (: render-handle (-> Nonnegative-Integer (Pairof Boolean Vector2D) Renderer))
  (define (render-handle handle-id handle-pos)
    ((handle-style handle-id handle-pos) 1.0 (r:circle (cdr handle-pos) handle-radius)))
  
  (: render RendererFunc)
  (define (render w h)
    (define client-render (render-body w h))
    (if (in-view-mode?)
        client-render
        (r:all client-render
               (apply (curry controls-style 1.0) (apply-map2 (map control-render (controls)) w h))
               (let ((handles (map (inst mut-get (Pairof Boolean Vector2D)) (handles))))
                 (apply r:all (map render-handle (range 0 (length handles)) handles))))))

  (: try-control-click (-> Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Boolean Boolean))
  (define (try-control-click x y w h is-drag?)
    (for/or : Boolean ((control : Control (controls)))
      (if (r:contains ((ann (control-render control) RendererFunc) w h) (->fl x) (->fl y))
          (begin
            (when (or (not is-drag?) (control-draggable control))
              ((control-press control) x y w h))
            #t)
          #f)))
  
  (: press MouseFunc)
  (define (press x y w h)
    (set! mouse-pos (vec (->fl x) (->fl y)))
    (or (let ((handles (map-cdrs (map (inst mut-get (Pairof Boolean Vector2D)) (handles)))))
          (ormap try-drag-handle (range 0 (length handles)) handles))
        (try-control-click x y w h #f))
    (void)) ; void because we don't care about any results

  (: update-dragging (-> Void))
  (define (update-dragging)
    (let ((drag drag-status))
      (when drag
        (let ((mut (list-ref (handles) (active-drag-index drag))))
          (mut-set! mut
                    (cons (car (mut-get mut)) (v+ (active-drag-rel drag) mouse-pos)))))))

  (: drag MouseFunc)
  (define (drag x y w h)
    (unless drag-status
      (try-control-click x y w h #t))
    (set! mouse-pos (vec (->fl x) (->fl y)))
    (update-dragging))

  (: move MouseFunc)
  (define (move x y w h)
    (set! mouse-pos (vec (->fl x) (->fl y))))

  (: release MouseFunc)
  (define (release x y w h)
    (set! mouse-pos (vec (->fl x) (->fl y)))
    (unless (in-view-mode?)
      (update-dragging)
      (set! drag-status #f))
    (for ((control : Control (controls)))
      ((control-release control) x y w h)))
  
  (gui-basic render press drag move release width height title))
