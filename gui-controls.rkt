#lang typed/racket
(require (only-in typed/racket/gui
                  put-file))

(require "utils.rkt")
(require "vector.rkt")
(require "gui-handles.rkt")
(require "functional-graphics.rkt")

(provide gui-controls Setting Button button setting-slider setting-option)

(define button-size 40)
(define control-height 30)
(define control-spacing (+ control-height 10))
(define slider-width 150)

(struct btn ([press : (-> Void)] [color : Color]))
(define-type Button btn)
(: button (-> (-> Void) Color Button))
(define (button press color)
  (btn press color))

(: button->control (-> Vector2D Button Control))
(define (button->control pos button)
  (list (lambda (w h) (r:brush (btn-color button) 'solid (r:rect pos button-size button-size)))
        (lambda (x y w h) ((btn-press button)))
        void))

(define-type Setting (U (List String 'slider Real Real (Boxof Real))
                        (List String 'option (Boxof Boolean))))

(: setting-slider (-> String Real Real (Boxof Real) Setting))
(define (setting-slider name min max value-box)
  (list name 'slider min max value-box))

(: setting-option (-> String (Boxof Boolean) Setting))
(define (setting-option name value-box)
  (list name 'option value-box))

(: setting->control (-> Vector2D Setting Control))
(define (setting->control pos setting)
  (if (eq? (cadr setting) 'slider)
      (let-values (((name _ min max value-box) (apply values setting)))
        (let ((line-start (v+ pos (vec 0 (/ control-height 2))))
              (line-end (v+ pos (vec slider-width (/ control-height 2))))
              (text-pos (v+ pos (vec (+ slider-width 15) 0))))
          (list (lambda (w h) (r:all (r:line line-start line-end)
                                     (r:blank pos slider-width control-height)
                                     (r:circle (vinterpolate line-start line-end
                                                             (normalize min max (unbox value-box)))
                                               (exact-ceiling (/ control-height 2)))
                                     (r:text text-pos name)))
                (lambda (x y w h) (set-box! value-box (denormalize min max
                                                                   (normalize (vec-x line-start)
                                                                              (vec-x line-end)
                                                                              x))))
                void)))
      (let-values (((name _ value-box) (apply values setting)))
        (define box-size control-height)
        (let ((text-pos (v+ pos (vec (+ box-size 10) 0))))
          (list (lambda (w h) (r:all (r:brush (if (unbox value-box) "green" "white") 'solid (r:rect pos box-size box-size))
                                     (r:text text-pos name)))
                (lambda (x y w h) (set-box! value-box (not (unbox value-box))))
                void)))))

(: gui-controls (-> (DynListOf Vector2D) (MutListOf Setting) RendererFunc (Listof Button) Positive-Integer Positive-Integer String Void))
(define (gui-controls handles settings render buttons width height title)

  (: view-mode Boolean)
  (define view-mode #f)

  (: in-view-mode? (-> Boolean))
  (define (in-view-mode?) view-mode)

  (: view-mode-control Control)
  (define view-mode-control
    (list (lambda (w h) (r:rect (vec (- w button-size) 0) button-size button-size))
          (lambda (x y w h) (set! view-mode #t))
          (lambda (x y w h) (set! view-mode #f))))

  (: controls (MutListOf Control))
  (define controls
    (mutlist-append (list view-mode-control)
                    (for/list : (Listof Control) (((i button) (list->hash buttons)))
                      (button->control (vec (* i button-size) 0) button))
                    (mutlist-enum-map (lambda ([i : Nonnegative-Integer] [x : Setting])
                                        (setting->control (vec 0 (+ 200 (* i control-height))) x))
                                      settings)))
  
  #| (: handles (DynListOf Vector2D))
  (define handles (dynlist-lambda (Vector2D) (body-get body)
                                  (i v) (body-set! body i v))) |#

  (gui-handles render handles controls in-view-mode? width height title))

(define test-vec (vec 30 30))
(define slider-value (box (ann 0 Real)))
(define option-value (box (ann #f Boolean)))
(gui-controls (dynlist-lambda (Vector2D) (list (if (unbox option-value)
                                                   (v+ test-vec (vec (unbox slider-value) 0))
                                                   test-vec))
                              (i v) (if (unbox option-value)
                                        (set! test-vec (v- v (vec (unbox slider-value) 0)))
                                        (set! test-vec v)))
              (list->mutlist (list (setting-slider "Test Slider" -30 30 slider-value)
                                   (setting-option "Test Option" option-value)))
              (lambda (w h) (r:line test-vec (vec (/ w 2) (/ h 2))))
              (list (button (lambda () (displayln "HELLO WORLD")) "red"))
              600 600 "Test of gui-controls")

#|
  (: save-rendering ButtonFunc)
  (define (save-rendering x y w h)
    (displayln "Saving image...")
    (let ((path (put-file "Choose where to save a snapshot" #f #f "scribble.png"
                          ".png" empty (list (list "PNG Images" "*.png") (list "Any" "*.*")))))
      (when path
        (r:save-to (render-scene w h) w h (path->string path)))))|#

  #|(: save-project ButtonPressFunc)
  (define (save-project w h)
    (unless (or (= w 0) (= h 0))
      (displayln "Saving project...")
      (let ((path (put-file "Choose where to save your project" #f #f "scribble.scribble"
                            ".scribble" empty (list (list "Scribbles Projects" "*.scribble") (list "Any" "*.*")))))
        (when path
          (save-to (body-save body) (path->string path))))))|#
