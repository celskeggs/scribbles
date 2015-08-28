#lang racket

(provide define-provide)

(define-syntax define-provide
  (syntax-rules ()
    [(define-provide (name args ...) body ...)
     (begin
       (provide name)
       (define (name args ...) body ...))]
    [(define-provide (name args ... . rest) body ...)
     (begin
       (provide name)
       (define (name args ... . rest) body ...))]))

(define-provide (sq x)
  (* x x))

(define-provide (enumerate seq [start-at 0] [combiner list])
  (if (empty? seq)
      empty
      (cons (combiner start-at (car seq))
            (enumerate (cdr seq) (+ start-at 1) combiner))))

(define-provide (list-update-ref list i val)
  (if (= i 0)
      (cons val (cdr list))
      (cons (car list) (list-update-ref (cdr list) (- i 1) val))))

; UNUSED
(define-syntax (static-length stx)
  (datum->syntax stx (- (length (syntax-e stx)) 1)))
