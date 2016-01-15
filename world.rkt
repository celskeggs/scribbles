#lang typed/racket
(require "utils.rkt")
(require "entity.rkt")
(require "vector.rkt")
(require "functional-graphics.rkt")
(require "saving.rkt")

(provide World world-new
         world->mutlist world->handles world-selected world-entities world-selected-entity
         world-add world-delete-selected
         world-save world-load!)

(define-type World world)
(struct world ([entities : (Listof Entity)] [selected : (U #f Nonnegative-Integer)]) #:mutable)

(: world->mutlist (-> World (MutListOf Entity)))
(define (world->mutlist world)
  (lambda () (world-entities world)))

(: world->handles (-> World (MutListOf (Mutable (Pairof Boolean Vector2D)))))
(define (world->handles world)
  (mutlist-append* (mutlist-enum-map (lambda ([i : Nonnegative-Integer] [vecs : (MutListOf (Mutable Vector2D))])
                                       ; This allows us to know which entity was last modified - hence, last selected.
                                       (mutlist-map (lambda ([vcs : (Mutable Vector2D)])
                                                      (mut-make (lambda ()
                                                                  (cons (equal? i (world-selected world)) (mut-get vcs)))
                                                                (lambda ([vp : (Pairof Boolean Vector2D)])
                                                                  (set-world-selected! world i)
                                                                  (mut-set! vcs (cdr vp)))))
                                                    vecs))
                                     (mutlist-map entity-handles (world->mutlist world)))))

(: world-new (-> World))
(define (world-new)
  (world (list) #f))

(: world-selected-entity (-> World (U #f Entity)))
(define (world-selected-entity world)
  (let ((selected (world-selected world)))
    (and selected (< selected (length (world-entities world))) (list-ref (world-entities world) selected))))

(: world-add (-> World Entity Void))
(define (world-add world ent)
  (set-world-entities! world (cons ent (world-entities world))))

(: world-delete-selected (-> World Boolean))
(define (world-delete-selected world)
  (let ((selected (world-selected world)))
    (and selected
         (begin
           (set-world-entities! world (without-i (world-entities world) selected))
           #t))))

(: world-load! (-> World Encoded Void))
(define (world-load! world enc)
  (if (list? enc)
      (set-world-entities! world (map entity-load enc))
      (error "invalid saved world")))

(: world-save (-> World Encoded))
(define (world-save world)
  (map entity-save (world-entities world)))
