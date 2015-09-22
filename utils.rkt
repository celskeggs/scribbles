#lang typed/racket

(provide sq enumerate enumerate-generic set-union* apply-map apply-each without)

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

(: set-union* (All (A) (-> (Listof (Setof A)) (Setof A))))
(define (set-union* sets)
  (cond [(empty? sets) (set)]
        [(empty? (cdr sets)) (car sets)]
        [else (set-union (car sets) (set-union* (cdr sets)))]))

(: apply-map (All (In Out) (-> (Listof (-> In Out)) In (Listof Out))))
(define (apply-map fs in)
  (if (empty? fs)
      empty
      (cons ((car fs) in) (apply-map (cdr fs) in))))

(: apply-each (All (Out) (-> (Listof (-> Out)) (Listof Out))))
(define (apply-each fs)
  (if (empty? fs)
      empty
      (cons ((car fs)) (apply-each (cdr fs)))))

(: without (All (E) (-> (Listof E) E (Listof E))))
(define (without l e)
  (filter-not (curry equal? e) l))
