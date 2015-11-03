#lang typed/racket
(require racket/flonum)
(require "utils.rkt")
(require "vector.rkt")
(require "gui-handles.rkt")
(require "functional-graphics.rkt")
(require "setting.rkt")
(require "entity.rkt")

(provide gui-controls Button ButtonPress RendererFunc button)

(define button-size 40.0)
(define control-height 30.0)
(define half-control-height (cast (/ control-height 2.0) Positive-Flonum))
(define control-spacing (+ control-height 10.0))
(define slider-width 150.0)

(define-type ButtonPress (-> Positive-Integer Positive-Integer Void))
(struct btn ([press : ButtonPress] [color : Color]))
(define-type Button btn)
(: button (-> ButtonPress Color Button))
(define (button press color)
  (btn press color))

(: button->control (-> Vector2D Button Control))
(define (button->control pos button)
  (control (lambda (w h) (r:brush (btn-color button) 'solid (r:rect pos button-size button-size)))
           #f
           (lambda (x y w h) ((btn-press button) w h))
           void))

(: setting->control (-> Vector2D Setting Control))
(define (setting->control pos setting)
  (if (setting-slider? setting)
      (let-values (((min max) (setting-bounds setting)))
        (let ((line-start (v+ pos (vec 0.0 (/ control-height 2))))
              (line-end (v+ pos (vec slider-width (/ control-height 2))))
              (text-pos (v+ pos (vec (+ slider-width 15) 0.0))))
          (control (lambda (w h) (r:all (r:line line-start line-end)
                                        (r:blank pos slider-width control-height)
                                        (r:circle (vinterpolate line-start line-end
                                                                (normalize min max (setting->value setting)))
                                                  (ceiling half-control-height))
                                        (r:text text-pos (setting-key setting))))
                   #t
                   (lambda (x y w h) (setting-set-value! setting
                                                         (denormalize min max
                                                                      (normalize (vec-x line-start)
                                                                                 (vec-x line-end)
                                                                                 (->fl x)))))
                   void)))
      (let ((box-size control-height))
        (let ((text-pos (v+ pos (vec (+ box-size 10) 0.0))))
          (control (lambda (w h) (r:all (r:brush (if (setting->value setting) "green" "white") 'solid (r:rect pos box-size box-size))
                                        (r:text text-pos (setting-key setting))))
                   #f
                   (lambda (x y w h) (setting-set-value! setting (not (setting->value setting))))
                   void)))))

(: gui-controls (-> (MutListOf (Mutable Vector2D)) (MutListOf Setting) RendererFunc (Listof Button) Positive-Integer Positive-Integer String Void))
(define (gui-controls handles settings render buttons width height title)

  (: view-mode Boolean)
  (define view-mode #f)

  (: in-view-mode? (-> Boolean))
  (define (in-view-mode?) view-mode)

  (: view-mode-control Control)
  (define view-mode-control
    (control (lambda (w h) (r:rect (vec (- w button-size) 0.0) button-size button-size))
             #f
             (lambda (x y w h) (set! view-mode #t))
             (lambda (x y w h) (set! view-mode #f))))

  (: controls (MutListOf Control))
  (define controls
    (mutlist-append (list view-mode-control)
                    (for/list : (Listof Control) (((i button) (list->hash buttons)))
                      (button->control (vec (* (->fl i) button-size) 0.0) button))
                    (mutlist-enum-map (lambda ([i : Nonnegative-Integer] [x : Setting])
                                        (setting->control (vec 0.0 (+ 200 (* (->fl i) control-height))) x))
                                      settings)))

  (gui-handles render handles controls in-view-mode? width height title))

#|(when #f
  (define test-vec (vec 30 30))
  (define slider-value (mut-cell (ann 0 Real)))
  (define option-value (mut-cell (ann #f Boolean)))
  (gui-controls (list->mutlist (list (mut-make (lambda ()
                                                 (if (mut-get option-value)
                                                     (v+ test-vec (vec (mut-get slider-value) 0))
                                                     test-vec))
                                               (lambda ([v : Vector2D])
                                                 (if (mut-get option-value)
                                                     (set! test-vec (v- v (vec (mut-get slider-value) 0)))
                                                     (set! test-vec v))))))
                (list->mutlist (list (setting-slider "Test Slider" -30 30 slider-value)
                                     (setting-option "Test Option" option-value)))
                (lambda (w h) (r:line test-vec (vec (/ w 2) (/ h 2))))
                (list (button (lambda (w h) (displayln "HELLO WORLD")) "red"))
                600 600 "Test of gui-controls"))|#
