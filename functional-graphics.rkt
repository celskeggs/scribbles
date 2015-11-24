#lang typed/racket
(require "vector.rkt")
(require/typed "wrap-draw.rkt"
               [#:opaque ColorInst wd:color?]
               [#:opaque PenInst wd:pen?]
               [#:opaque BrushInst wd:brush?]
               [#:opaque Context wd:context?]
               [wd:def-color (-> Byte Byte Byte ColorInst)]
               [wd:find-color (-> Color ColorInst)]
               [wd:find-pen (-> Color Positive-Integer PenInst)]
               [wd:find-brush (-> Color BrushInst)]
               [wd:render-to-file (-> (-> Context Void) Positive-Integer Positive-Integer String Void)]
               [wd:with-pen (-> Context PenInst (-> Void) Void)]
               [wd:with-brush (-> Context BrushInst (-> Void) Void)]
               [wd:line (-> Context Float Float Float Float Void)]
               [wd:ellipse (-> Context Float Float Float Float Void)]
               [wd:rectangle (-> Context Float Float Float Float Void)]
               [wd:polygon (-> Context (Listof (Pairof Float Float)) Void)]
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
         r:all
         r:circle r:line r:rect r:poly
         r:text r:blank
         r:render-to r:save-to r:contains
         [rename-out (wd:def-color r:color)]
         rf:compose
         wd:window-and-canvas)

(define-type RendererFunc (-> Nonnegative-Integer Nonnegative-Integer Renderer))

(define-type Color (U String ColorInst))
(define-type Renderer (Pairof (-> Context Void) (-> Float Float Boolean)))
(define-type Style (-> Renderer * Renderer))

(define-syntax-rule (genren (dc x y) render check)
  (cons (lambda ([dc : Context])
          render)
        (lambda ([x : Float] [y : Float])
          check)))

(: nop-renderer Renderer)
(define nop-renderer (cons void (const #f)))

(: r:wrap-style (-> Color Positive-Integer Color Style))
(define ((r:wrap-style pen-color pen-width brush-color) . bodies)
  (r:style pen-color pen-width brush-color (apply r:all bodies)))

(: r:pen (-> Color Positive-Integer Renderer * Renderer))
(define (r:pen color width . bodies)
  (define allocated-pen (wd:find-pen color width))
  (genren (dc x y)
          (wd:with-pen dc allocated-pen
                       (thunk
                        (for ((body bodies))
                          ((car body) dc))))
          (for/or : Boolean ((body : Renderer bodies))
            ((cdr body) x y))))

(: r:brush (-> Color Renderer * Renderer))
(define (r:brush color . bodies)
  (define allocated-brush (wd:find-brush color))
  (genren (dc x y)
          (wd:with-brush dc allocated-brush
                         (thunk
                          (for ((body bodies))
                            ((car body) dc))))
          (for/or : Boolean ((body : Renderer bodies))
            ((cdr body) x y))))

(: r:style (-> Color Positive-Integer Color Renderer * Renderer))
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
  (cond ((empty? bodies) nop-renderer)
        ((empty? (cdr bodies)) (car bodies))
        (else
         (genren (dc x y)
                 (for ((body : Renderer bodies))
                   ((car body) dc))
                 (for/or : Boolean ((body : Renderer bodies))
                   ((cdr body) x y))))))

(: r:contains (-> Renderer Float Float Boolean))
(define (r:contains rf x y)
  ((cdr rf) x y))

(define default-pen (wd:find-pen "black" 1))
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
