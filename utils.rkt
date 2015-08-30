#lang racket

(provide define-provide each)

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

(define-for-syntax (recursive-replace from to body)
  (if (and (identifier? body) (free-identifier=? from body))
      to
      (let ((unwrap (syntax-e body)))
        (if (list? unwrap)
            (map (lambda (e) (recursive-replace from to e)) unwrap)
            unwrap))))

(define-syntax (each stx)
  (syntax-protect
   (syntax-case stx ()
     [(_ bind-name (various-names ...) body)
      #'(each bind-name begin (various-names ...) body)]
     [(_ bind-name wrap-name (various-names ...) body)
      (datum->syntax #'body
                     (cons #'wrap-name
                           (map (lambda (active-name)
                                  (recursive-replace #'bind-name active-name #'body))
                                (syntax-e #'(various-names ...)))))])))

; UNUSED
(define-syntax (static-length stx)
  (datum->syntax stx (- (length (syntax-e stx)) 1)))
