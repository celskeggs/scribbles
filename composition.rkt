#lang typed/racket
(require (only-in typed/racket/gui
                  put-file))

(require "vector.rkt")
(require "manipulation-view.rkt")
(require "functional-graphics.rkt")
(require "body.rkt")
(require "saving.rkt")

(provide compose-scene ButtonPressFunc)

(: compose-scene (->* (Style Body) ((Listof (Pairof String ButtonPressFunc)) Positive-Integer Positive-Integer) Void))
(define (compose-scene style body [buttons empty] [width 300] [height 300])
  (define calculated-view (style (body-update-and-render! body)))

  (: render-local RenderFunc)
  (define (render-local dc)
    (r:render-to calculated-view dc))

  (: get-handles-local GetHandleFunc)
  (define (get-handles-local)
    (body-get body))

  (: update-handle-local! UpdateHandleFunc)
  (define (update-handle-local! i vec)
    (body-set! body i vec)
    (set! calculated-view (style (body-update-and-render! body))))

  (: save-rendering ButtonPressFunc)
  (define (save-rendering w h)
    (unless (or (= w 0) (= h 0))
      (displayln "Saving image...")
      (let ((path (put-file "Choose where to save a snapshot" #f #f "scribble.png"
                            ".png" empty (list (list "PNG Images" "*.png") (list "Any" "*.*")))))
        (when path
          (r:save-to calculated-view w h (path->string path))))))

  (: save-project ButtonPressFunc)
  (define (save-project w h)
    (unless (or (= w 0) (= h 0))
      (displayln "Saving project...")
      (let ((path (put-file "Choose where to save your project" #f #f "scribble.scribble"
                            ".scribble" empty (list (list "Scribbles Projects" "*.scribble") (list "Any" "*.*")))))
        (when path
          (save-to (body-save body) (path->string path))))))

  (: wrap-button (-> ButtonPressFunc ButtonPressFunc))
  (define (wrap-button btn)
    (lambda (w h)
      (btn w h)
      (set! calculated-view (style (body-update-and-render! body)))))

  (: fixed-buttons (Listof (Pairof String ButtonPressFunc)))
  (define fixed-buttons (map (inst cons String ButtonPressFunc)
                             (map (inst car String ButtonPressFunc) buttons)
                             (map wrap-button
                                  (map (inst cdr String ButtonPressFunc) buttons))))

  (handle-view render-local get-handles-local update-handle-local! (cons (cons "yellow" save-rendering) fixed-buttons) width height))
