#lang typed/racket
(require (only-in typed/racket/gui
                  put-file))

(require "utils.rkt")
(require "vector.rkt")
(require "gui-controls.rkt")
(require "functional-graphics.rkt")
(require "world.rkt")
(require "entity.rkt")

(provide editor-main)

(define title "Scribbles Editor")

(: world->settings (-> World (MutListOf Setting)))
(define (world->settings world)
  (lambda ()
    (let ((sel (world-selected-entity world)))
      (if sel
          (entity-settings sel)
          empty))))

(: world->renderer (-> World RendererFunc))
(define (world->renderer world)
  (let ((funcs (mutlist-map entity-renderer (world->mutlist world))))
    (lambda (w h)
      (apply r:all
             (for/list : (Listof Renderer) ((func : RendererFunc (funcs)))
               (func w h))))))

(: editor-gui (-> World ButtonPress Positive-Integer Positive-Integer Void))
(define (editor-gui world add-entity width height)
  (: renderer RendererFunc)
  (define renderer (world->renderer world))
  
  (: save-rendering ButtonPress)
  (define (save-rendering w h)
    (displayln "Saving image...")
    (let ((path (put-file "Choose where to save a snapshot" #f #f "scribble.png"
                          ".png" empty (list (list "PNG Images" "*.png") (list "Any" "*.*")))))
      (when path
        (r:save-to (renderer w h) w h (path->string path)))))

  (: remove-entity ButtonPress)
  (define (remove-entity w h)
    (unless (world-delete-selected world)
      (displayln "Nothing selected!")))

  (print (list "GOT" ((world->settings world))))
  
  (gui-controls (world->handles world) (world->settings world) renderer
                (list (button save-rendering "orange")
                      (button add-entity "green")
                      (button remove-entity "red"))
                width height title))

(: editor-main (->* ((-> Positive-Integer Positive-Integer Entity)) (Positive-Integer Positive-Integer) Void))
(define (editor-main new-entity [width 500] [height 500])
  (define world (world-new))
  
  (world-add world (new-entity width height))

  (: add-entity ButtonPress)
  (define (add-entity w h)
    (world-add world (new-entity w h)))
  
  (editor-gui world add-entity width height))

#||#

  #|(: save-project ButtonPressFunc)
  (define (save-project w h)
    (unless (or (= w 0) (= h 0))
      (displayln "Saving project...")
      (let ((path (put-file "Choose where to save your project" #f #f "scribble.scribble"
                            ".scribble" empty (list (list "Scribbles Projects" "*.scribble") (list "Any" "*.*")))))
        (when path
          (save-to (body-save body) (path->string path))))))|#
