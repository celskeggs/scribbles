#lang typed/racket
(require (only-in typed/racket/gui
                  put-file))

(require "manipulation-view.rkt")
(require "functional-graphics.rkt")
(require "segments.rkt")

(provide compose fixed-distance root bone compose-segs)

(: compose-segs (->* ((-> Renderer * Renderer) (Listof segment)) (Positive-Integer Positive-Integer) Void))
(define (compose-segs style segs [width 300] [height 300])
  (define (calculate)
    (apply style (map (lambda ([seg : segment]) ((segment-update-and-render seg))) segs)))

  (define calculated-view (calculate))

  (: render-local RenderFunc)
  (define (render-local dc)
    (r:render-to calculated-view dc))

  (: get-handles-local GetHandleFunc)
  (define (get-handles-local)
    (map (lambda ([seg : segment]) ((segment-get seg))) segs))

  (: update-handle-local! UpdateHandleFunc)
  (define (update-handle-local! i vec)
    ((segment-set (list-ref segs i)) vec)
    (set! calculated-view (calculate)))

  (: save-rendering ButtonPressFunc)
  (define (save-rendering w h)
    (unless (or (= w 0) (= h 0))
      (displayln "Saving...")
      (let ((path (put-file "Choose where to save a snapshot" #f #f "scribble.png"
                            ".png" empty (list (list "PNG Images" "*.png") (list "Any" "*.*")))))
        (when path
          (r:save-to calculated-view w h (path->string path))))))

  (handle-view render-local get-handles-local update-handle-local! (list (cons "yellow" save-rendering)) width height))

(: compose (-> (-> Renderer * Renderer) Positive-Integer Positive-Integer slotted-segment * Void))
(define (compose style width height . elements)
  (compose-segs style (apply construct elements) width height))
