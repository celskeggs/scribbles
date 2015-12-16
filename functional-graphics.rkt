#lang typed/racket
(require "vector.rkt")
(require "bezier-fit.rkt")
(require/typed "wrap-draw.rkt"
               [#:opaque ColorInst wd:color?]
               [#:opaque PenInst wd:pen?]
               [#:opaque BrushInst wd:brush?]
               [#:opaque Context wd:context?]
               [wd:transparent-brush BrushInst]
               [wd:def-color (-> Byte Byte Byte ColorInst)]
               [wd:find-color (-> Color ColorInst)]
               [wd:find-pen (-> Color Positive-Float PenInst)]
               [wd:find-brush (-> Color BrushInst)]
               [wd:render-to-file (-> (-> Context Void) Positive-Integer Positive-Integer String Void)]
               [wd:with-pen (-> Context PenInst (-> Void) Void)]
               [wd:with-brush (-> Context BrushInst (-> Void) Void)]
               [wd:line (-> Context Float Float Float Float Void)]
               [wd:ellipse (-> Context Float Float Float Float Void)]
               [wd:rectangle (-> Context Float Float Float Float Void)]
               [wd:polygon (-> Context (Listof (Pairof Float Float)) Void)]
               [wd:spline (-> Context Float Float Float Float Float Float Void)]
               [wd:bezier (-> Context Float Float Float Float Float Float Float Float Void)]
               [wd:text (-> Context String Float Float Void)]
               [wd:window-and-canvas (All (A) (-> String Positive-Integer Positive-Integer
                                                  (-> (-> Void) A)
                                                  (-> A Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Void)
                                                  (-> A Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Void)
                                                  (-> A Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Void)
                                                  (-> A Nonnegative-Integer Nonnegative-Integer Positive-Integer Positive-Integer Void)
                                                  (-> A Context Positive-Integer Positive-Integer Void)
                                                  Void))])

(provide Renderer Style Color Context RendererFunc
         r:pen r:brush r:style r:wrap-style
         r:all r:nothing no-style
         r:circle r:line r:rect r:poly r:spline r:bezier r:bspline
         r:text r:blank
         r:render-to r:save-to r:contains
         [rename-out (wd:def-color r:color)]
         rf:compose
         wd:window-and-canvas)

(define-type RendererFunc (-> Nonnegative-Integer Nonnegative-Integer Renderer))

(define-type Color (U String ColorInst))
(define-type Renderer (Pairof (-> Context Void) (-> Float Float Boolean)))
(define-type Style (-> Positive-Float Renderer * Renderer))

(define-syntax-rule (genren (dc x y) render check)
  (cons (lambda ([dc : Context])
          render)
        (lambda ([x : Float] [y : Float])
          check)))

(: r:nothing Renderer)
(define r:nothing (cons void (const #f)))

(: p* (-> Positive-Float Positive-Float Positive-Float))
(define (p* x y)
  (let ((m (* x y)))
    (if (= m 0)
        (error "Number scale mismatch.")
        m)))

(: r:wrap-style (-> Color Positive-Float (Option Color) Style))
(define ((r:wrap-style pen-color pen-width brush-color) line-scale . bodies)
;  (r:style pen-color (p* line-scale pen-width) brush-color (apply r:all bodies)))
  (r:style pen-color (p* (if (= line-scale 1.0) 1.0 100.0) pen-width) brush-color (apply r:all bodies)))

(: r:pen (-> Color Positive-Float Renderer * Renderer))
(define (r:pen color width . bodies)
  (define allocated-pen (wd:find-pen color width))
  (genren (dc x y)
          (wd:with-pen dc allocated-pen
                       (thunk
                        (for ((body bodies))
                          ((car body) dc))))
          (for/or : Boolean ((body : Renderer bodies))
            ((cdr body) x y))))

(: r:brush (-> (Option Color) Renderer * Renderer))
(define (r:brush color . bodies)
  (define allocated-brush (if color (wd:find-brush color) wd:transparent-brush))
  (genren (dc x y)
          (wd:with-brush dc allocated-brush
                         (thunk
                          (for ((body bodies))
                            ((car body) dc))))
          (for/or : Boolean ((body : Renderer bodies))
            ((cdr body) x y))))

(: r:style (-> Color Positive-Float (Option Color) Renderer * Renderer))
(define (r:style pen-color pen-width brush-color . bodies)
  (r:pen pen-color pen-width
         (r:brush brush-color
                  (apply r:all bodies))))

(: r:line (-> Vector2D Vector2D Renderer))
(define (r:line v1 v2)
  (genren (dc x y)
          (wd:line dc (vec-x v1) (vec-y v1) (vec-x v2) (vec-y v2))
          #f)) ; TODO: allow line collision detection?

(: r:circle (-> Vector2D Positive-Float Renderer))
(define (r:circle center rad)
  (genren (dc x y)
          (wd:ellipse dc (- (vec-x center) rad) (- (vec-y center) rad) (* rad 2) (* rad 2))
          (vin-circle? (vec x y) center rad)))

(: r:rect (-> Vector2D Positive-Float Positive-Float Renderer))
(define (r:rect pos width height)
  (genren (dc x y)
          (wd:rectangle dc (vec-x pos) (vec-y pos) width height)
          (vin-rectangle? (vec x y) pos (vec width height))))

(: r:poly (-> (Listof Vector2D) Renderer))
(define (r:poly points)
  (genren (dc x y)
          (wd:polygon dc (map vec->pair points))
          #f)) ; TODO: allow polygon collision detection?

(: r:spline (-> Vector2D Vector2D Vector2D Renderer))
(define (r:spline v1 v2 v3)
  (genren (dc x y)
          (wd:spline dc (vec-x v1) (vec-y v1) (vec-x v2) (vec-y v2) (vec-x v3) (vec-y v3))
          #f)) ; TODO: allow spline collision detection?

(: r:bezier (-> Vector2D Vector2D Vector2D Vector2D Renderer))
(define (r:bezier v0 v1 v2 v3)
  (genren (dc x y)
          (wd:bezier dc (vec-x v0) (vec-y v0) (vec-x v1) (vec-y v1) (vec-x v2) (vec-y v2) (vec-x v3) (vec-y v3))
          #f)) ; TODO: allow bezier collision detection?

(: r:bspline (-> Vector2D Vector2D Vector2D Float Renderer))
(define (r:bspline v0 v1 v2 t)
  (let-values (((va0 va1 va2 va3 vb0 vb1 vb2 vb3) (fit-cubics v0 v1 v2 t)))
    (r:all (r:bezier va0 va1 va2 va3)
           (r:bezier vb0 vb1 vb2 vb3))))

(: r:blank (-> Vector2D Positive-Float Positive-Float Renderer))
(define (r:blank pos width height)
  (genren (dc x y)
          (void)
          (vin-rectangle? (vec x y) pos (vec width height))))

(: r:text (-> Vector2D String Renderer))
(define (r:text pos text)
  (genren (dc x y)
          (wd:text dc text (vec-x pos) (vec-y pos))
          #f)) ; TODO: allow text collision detection?

(: r:all (-> Renderer * Renderer))
(define (r:all . bodies)
  (cond ((empty? bodies) r:nothing)
        ((empty? (cdr bodies)) (car bodies))
        (else
         (genren (dc x y)
                 (for ((body : Renderer bodies))
                   ((car body) dc))
                 (for/or : Boolean ((body : Renderer bodies))
                   ((cdr body) x y))))))

(: no-style Style)
(define (no-style size . bodies)
  (apply r:all bodies))

(: r:contains (-> Renderer Float Float Boolean))
(define (r:contains rf x y)
  ((cdr rf) x y))

(define default-pen (wd:find-pen "black" 1.0))
(define default-brush (wd:find-brush "green"))

(: r:render-to (-> Renderer Context Void))
(define (r:render-to rf dc)
  (wd:with-pen dc default-pen
               (thunk (wd:with-brush dc default-brush
                                     (thunk ((car rf) dc))))))

(: r:save-to (-> Renderer Positive-Integer Positive-Integer String Void))
(define (r:save-to rf w h file)
  (wd:render-to-file
   (lambda (context) (r:render-to rf context))
   w h file))

(: rf:compose (-> (Listof RendererFunc) RendererFunc))
(define ((rf:compose funcs) w h)
  (apply r:all (for/list : (Listof Renderer) ((func funcs))
                 (func w h))))
