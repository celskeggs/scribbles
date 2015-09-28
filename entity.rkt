#lang typed/racket
(require "utils.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")
(require "saving.rkt")

(provide Entity RendererFunc Setting SaveFunc LoadFunc
         entity entity-handles entity-settings entity-renderer
         setting-slider setting-positive-slider setting-option
         register-entity-brand! entity-save entity-load)

(struct entity ([handles : (MutListOf (Mutable Vector2D))]
                [settings : (Listof Setting)]
                [renderer : RendererFunc]
                [savef : SaveFunc]))

(define-type Entity entity)
(define-type SaveFunc (-> (List Symbol Encoded)))
(define-type LoadFunc (-> Encoded Entity))

(define-type Setting (U (List String 'slider Real Real (Mutable Real))
                        (List String 'option (Mutable Boolean))))

(: setting-slider (-> String Real Real (Mutable Real) Setting))
(define (setting-slider name min max value-box)
  (list name 'slider min max value-box))

(: setting-positive-slider (-> String Positive-Real Positive-Real (Mutable Positive-Real) Setting))
(define (setting-positive-slider name min max value-box)
  (setting-slider name min max
                  (mut-make (lambda () (mut-get value-box))
                            (lambda ([val : Real]) (if (positive? val)
                                                       (mut-set! value-box val)
                                                       (error "must be positive!")))))) ; should be ensured by the bounds

(: setting-option (-> String (Mutable Boolean) Setting))
(define (setting-option name value-box)
  (list name 'option value-box))

(: setting->value (-> Setting (U Real Boolean)))
(define (setting->value setting)
  (if (eq? (cadr setting) 'slider)
      (mut-get (fifth setting))
      (mut-get (third setting))))

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
