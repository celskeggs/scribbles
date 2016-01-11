#lang typed/racket
(require (only-in typed/racket/gui
                  get-file put-file))

(require "utils.rkt")
(require "vector.rkt")
(require "gui-controls.rkt")
(require "functional-graphics.rkt")
(require "world.rkt")
(require "setting.rkt")
(require "entity.rkt")
(require "saving.rkt")

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

  (: save-world ButtonPress)
  (define (save-world w h)
    (displayln "Saving project...")
    (let ((path (put-file "Choose where to save your project" #f #f "scribble.scp"
                          ".scp" empty (list (list "Scribble Projects" "*.scp") (list "Any" "*.*")))))
      (when path
        (save-to (world-save world) (path->string path)))))

  (: load-world ButtonPress)
  (define (load-world w h)
    (displayln "Loading project...")
    (let ((path (get-file "Select your project" #f #f #f
                          ".scp" empty (list (list "Scribble Projects" "*.scp") (list "Any" "*.*")))))
      (when path
        (world-load! world (load-from (path->string path))))))

  (: remove-entity ButtonPress)
  (define (remove-entity w h)
    (unless (world-delete-selected world)
      (displayln "Nothing selected!")))
  
  (gui-controls (world->handles world) (world->settings world) renderer
                (list (button save-rendering "orange" "export")
                      (button add-entity "green" "add")
                      (button remove-entity "red" "remove")
                      (button save-world "cyan" "save")
                      (button load-world "white" "load"))
                width height title))

(: editor-main (->* ((-> Positive-Integer Positive-Integer Entity)) (Positive-Integer Positive-Integer) Void))
(define (editor-main new-entity [width 500] [height 500])
  (define world (world-new))
  
  (world-add world (new-entity width height))

  (: add-entity ButtonPress)
  (define (add-entity w h)
    (world-add world (new-entity w h)))
  
  (editor-gui world add-entity width height))
