#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")

(provide entity Entity entity-handles entity-settings entity-renderer
         Setting setting-slider setting-option)

; TODO: SaveFunc?
(struct entity ([handles : (MutListOf (Mutable Vector2D))] [settings : (Listof Setting)] [renderer : RendererFunc]))

(define-type Entity entity)

(define-type Setting (U (List String 'slider Real Real (Boxof Real))
                        (List String 'option (Boxof Boolean))))

(: setting-slider (-> String Real Real (Boxof Real) Setting))
(define (setting-slider name min max value-box)
  (list name 'slider min max value-box))

(: setting-option (-> String (Boxof Boolean) Setting))
(define (setting-option name value-box)
  (list name 'option value-box))
