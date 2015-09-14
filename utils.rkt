#lang typed/racket

(provide sq enumerate enumerate-generic list-update-ref set-union*)

(: sq (-> Real Nonnegative-Real))
(define (sq x)
  (if (positive? x) ; this helps typed racket correctly infer that the result is always nonnegative!
      (* x x)
      (* x x)))

(: enumerate (All (Element) (->* ((Listof Element)) (Nonnegative-Integer) (Listof (Pairof Nonnegative-Integer Element)))))
(define (enumerate seq [start-at 0])
  (enumerate-generic seq (inst cons Nonnegative-Integer Element) start-at))

(: enumerate-generic (All (Element Result) (->* ((Listof Element) (-> Nonnegative-Integer Element Result)) (Nonnegative-Integer) (Listof Result))))
(define (enumerate-generic seq combiner [start-at 0])
  (if (empty? seq)
      empty
      (cons (combiner start-at (car seq))
            (enumerate-generic (ann (cdr seq) (Listof Element)) combiner (ann (+ start-at 1) Nonnegative-Integer)))))

(: list-update-ref (-> (Listof Any) Nonnegative-Integer Any (Listof Any)))
(define (list-update-ref list i val)
  (if (= i 0)
      (cons val (cdr list))
      (cons (car list) (list-update-ref (cdr list) (- i 1) val))))

(: set-union* (All (A) (-> (Listof (Setof A)) (Setof A))))
(define (set-union* sets)
  (cond [(empty? sets) (set)]
        [(empty? (cdr sets)) (car sets)]
        [else (set-union (car sets) (set-union* (cdr sets)))]))
