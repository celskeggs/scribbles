#lang typed/racket
(require "functional-graphics.rkt")
(require "segments.rkt")
(require "body.rkt")

(provide new-stick-figure)

(: line-bone (->* (Symbol Symbol Real Real) ((Listof RenderSegment)) Element))
(define (line-bone variant invariant dx dy [extra-renders empty])
  (bone variant invariant dx dy (cons 
                                 (lambda ([lookup : Vector2Ds]) (r:line (lookup variant) (lookup invariant)))
                                 extra-renders)))

(: line-bone-unpack (-> (List* Symbol Symbol Real Real (Listof RenderSegment)) Element))
(define (line-bone-unpack l)
  (line-bone (car l) (cadr l) (caddr l) (cadddr l) (cddddr l)))

(: line-bone-chain-i (-> Symbol (Listof (List* Symbol Real Real (Listof RenderSegment))) (Listof Element)))
(define (line-bone-chain-i initial rest)
  (if (empty? rest)
      empty
      (cons (line-bone-unpack (list* (caar rest) initial (cdar rest)))
            (line-bone-chain-i (caar rest) (cdr rest)))))

(: line-bone-chain (-> Symbol (List* Symbol Real Real (Listof RenderSegment)) * (Listof Element)))
(define (line-bone-chain initial . rest)
  (line-bone-chain-i initial rest))


(: new-stick-figure (-> Real Real Body))
(define (new-stick-figure x y)
  (construct
   (root 'collar x y)
   (line-bone 'pelvis 'collar 0 100)
   (line-bone 'head 'collar 0 -70 (list (lambda ([lookup : Vector2Ds]) (r:circle (lookup 'head) 50))))
   (line-bone-chain 'collar (list 'left-elbow -50 0) (list 'left-hand -50 0))
   (line-bone-chain 'collar (list 'right-elbow 50 0) (list 'right-hand 50 0))
   (line-bone-chain 'pelvis (list 'left-knee -42 42) (list 'left-foot 0 60))
   (line-bone-chain 'pelvis (list 'right-knee 42 42) (list 'right-foot 0 60))))
