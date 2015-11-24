#lang racket
(require racket/gui)
(require racket/draw)
; This file wraps classes with expensive types in Typed Racket into structures that are usable with less typechecking.

(provide wd:color? wd:pen? wd:brush? wd:context?
         wd:def-color wd:find-color wd:find-pen wd:find-brush
         wd:render-to-file wd:with-pen wd:with-brush
         wd:line wd:ellipse wd:rectangle wd:polygon wd:text
         wd:window-and-canvas)

(struct wd:color (v))
(struct wd:pen (v))
(struct wd:brush (v))
(struct wd:context (v))

(define (wd:def-color r g b)
  (wd:color (make-color r g b)))

(define (wd:find-color name)
  (if (wd:color? name)
      name
      (wd:color (or (send the-color-database find-color name)
                    (error "No such color:" name)))))

(define (wd:find-pen color width)
  (wd:pen (send the-pen-list find-or-create-pen (wd:color-v (wd:find-color color)) width 'solid)))

(define (wd:find-brush color)
  (wd:brush (send the-brush-list find-or-create-brush (wd:color-v (wd:find-color color)) 'solid)))

(define (wd:render-to-file renderer w h file [kind 'png])
  (let* ((bmp (make-bitmap w h))
         (dc (send bmp make-dc)))
    (send dc set-smoothing 'aligned)
    (send dc set-brush "white" 'solid)
    (send dc set-pen "white" 1 'solid)
    (send dc draw-rectangle 0 0 w h)
    (renderer (wd:context dc))
    (send bmp save-file file kind)
    (void)))

(define (wd:with-pen context pen cb)
  (let* ((dc (wd:context-v context))
         (orig-pen (send dc get-pen)))
    (send dc set-pen (wd:pen-v pen))
    (cb)
    (send dc set-pen orig-pen)
    (void)))

(define (wd:with-brush context brush cb)
  (let* ((dc (wd:context-v context))
         (orig-brush (send dc get-brush)))
    (send dc set-brush (wd:brush-v brush))
    (cb)
    (send dc set-brush orig-brush)
    (void)))

(define (wd:line context x1 y1 x2 y2)
  (send (wd:context-v context) draw-line x1 y1 x2 y2))

(define (wd:ellipse context x y w h)
  (send (wd:context-v context) draw-ellipse x y w h))

(define (wd:rectangle context x y w h)
  (send (wd:context-v context) draw-rectangle x y w h))

(define (wd:polygon context points)
  (send (wd:context-v context) draw-polygon points))

(define (wd:text context text x y)
  (send (wd:context-v context) draw-text text x y #t))

(define (wd:window-and-canvas title w h cb on-press on-release on-drag on-move on-render)
  (define frame (new frame% [label title] [width w] [height h]))
  (new (class canvas%
         (inherit get-dc get-width get-height)

         (define state (cb (thunk (send this refresh))))

         (define/override (on-event event)
           (let ((x (send event get-x))
                 (y (send event get-y))
                 (w (get-width))
                 (h (get-height)))
             (unless (or (zero? w) (zero? h) (not (positive? x)) (not (positive? y)))
               ((case (send event get-event-type)
                  ((left-down) on-press)
                  ((motion) (if (send event get-left-down) on-drag on-move))
                  ((left-up) on-release)
                  (else void)) state x y w h))))

         (define/override (on-paint)
           (define dc (get-dc))
           (send dc set-smoothing 'aligned)
           (send dc set-brush "white" 'solid)
           (send dc set-pen "white" 1 'solid)
           (on-render state (wd:context dc) (get-width) (get-height)))

         (super-new))
       [parent frame])
  (send frame center 'both)
  (send frame show #t))
