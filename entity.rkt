#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")
(require "saving.rkt")
(require "setting.rkt")

(provide Entity RendererFunc SaveFunc LoadFunc
         entity entity-handles entity-settings entity-renderer
         register-entity-brand! entity-save entity-load)

(struct entity ([handles : (MutListOf (Mutable Vector2D))]
                [settings : (Listof Setting)]
                [renderer : RendererFunc]
                [savef : SaveFunc]))

(define-type Entity entity)
(define-type SaveFunc (-> (List Symbol Encoded)))
(define-type LoadFunc (-> Encoded Entity))

(: entity-save (-> Entity Encoded))
(define (entity-save ent)
  (let ((out ((entity-savef ent))))
    (unless (hash-has-key? entity-brands (car out))
      (error "brand is invalid!"))
    out))

(: entity-brands (HashTable Symbol LoadFunc))
(define entity-brands (make-hash))

(: register-entity-brand! (-> Symbol LoadFunc Void))
(define (register-entity-brand! brand loader)
  (if (hash-has-key? entity-brands brand)
      (error "duplicate brand:" brand)
      (hash-set! entity-brands brand loader)))

(: entity-load (-> Encoded Entity))
(define (entity-load ent)
  (if (and (list? ent) (= 2 (length ent)))
      (let ((head (car ent)))
        (if (symbol? head)
            ((hash-ref entity-brands head) (cadr ent))
            (error "invalid entity to load")))
      (error "invalid entity to load")))
