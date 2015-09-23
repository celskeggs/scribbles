#lang typed/racket
(provide sq enumerate enumerate-generic list->hash set-union* apply-map2 apply-map apply-each without via normalize denormalize
         DynListOf dynlist-get dynlist-set! dynlist-construct dynlist-lambda list->mutlist
         MutListOf mutlist-map mutlist-enum-map mutlist-append)

; mutable lists

(define-type (MutListOf E) (-> (Listof E)))

(: mutlist-map (All (In Out) (-> (-> In Out) (MutListOf In) (MutListOf Out))))
(define (mutlist-map f lst)
  (lambda ()
    (map f (lst))))

(: mutlist-enum-map (All (In Out) (-> (-> Nonnegative-Integer In Out) (MutListOf In) (MutListOf Out))))
(define (mutlist-enum-map f lst)
  (lambda ()
    (define got (lst))
    (map f (range (length got)) got)))

(: list->mutlist (All (E) (-> (U (Listof E) (MutListOf E)) (MutListOf E))))
(define (list->mutlist lst)
  (if (procedure? lst)
      lst
      (lambda ()
        lst)))

(: mutlist-append (All (E) (-> (U (Listof E) (MutListOf E)) * (MutListOf E))))
(define (mutlist-append . lsts)
  (let ((lists (map (inst list->mutlist E) lsts)))
    (lambda ()
      (append* (apply-each lists)))))

; dynamic lists

(define-type (DynListOf E) (Pairof (-> (Listof E)) (-> Nonnegative-Integer E Void)))

(define-syntax-rule (dynlist-lambda (E) a (i v) b)
  (cons (lambda () a)
        (lambda ([i : Nonnegative-Integer] [v : E]) b)))

(: dynlist-construct (All (E) (-> (-> (Listof E)) (-> Nonnegative-Integer E Void) (DynListOf E))))
(define (dynlist-construct a b)
  (cons a b))

(: dynlist-get (All (E) (-> (DynListOf E) (Listof E))))
(define (dynlist-get dl)
  ((car dl)))

(: dynlist-set! (All (E) (-> (DynListOf E) Nonnegative-Integer E Void)))
(define (dynlist-set! dl i e)
  ((cdr dl) i e))

; everything else

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

(: list->hash (All (Element) (->* ((Listof Element)) (Nonnegative-Integer) (HashTable Nonnegative-Integer Element))))
(define (list->hash elems [start-at 0])
  (make-immutable-hash (map (inst cons Nonnegative-Integer Element)
                            (range start-at (+ start-at (length elems)))
                            elems)))

(: set-union* (All (A) (-> (Listof (Setof A)) (Setof A))))
(define (set-union* sets)
  (cond [(empty? sets) (set)]
        [(empty? (cdr sets)) (car sets)]
        [else (set-union (car sets) (set-union* (cdr sets)))]))

(: apply-map2 (All (In1 In2 Out) (-> (Listof (-> In1 In2 Out)) In1 In2 (Listof Out))))
(define (apply-map2 fs in1 in2)
  (if (empty? fs)
      empty
      (cons ((car fs) in1 in2) (apply-map2 (cdr fs) in1 in2))))

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

(: via (All (In Out) (-> (Option In) (-> In Out) (Option Out))))
(define (via x f)
  (if x (f x) #f))

(: normalize (-> Real Real Real Real))
(define (normalize min max value) ; convert to [0, 1] range.
  (/ (- value min) (- max min)))

(: denormalize (-> Real Real Real Real))
(define (denormalize min max value) ; convert from [0, 1] range.
  (+ min (* value (- max min))))
