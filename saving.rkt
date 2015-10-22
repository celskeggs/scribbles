#lang typed/racket
(require "vector.rkt")

(provide Encoded save-to load-from)

(define-type Encoded (U (Listof Encoded) Real String Symbol Boolean Vector2D))

(struct par-vec ([x : Real] [y : Real]) #:prefab)

(define-type Partial (U (Listof Partial) Real String Symbol Boolean par-vec))

(: partial-enc (-> Encoded Partial))
(define (partial-enc x)
  (cond ((list? x) (map partial-enc x))
        ((vec? x) (par-vec (vec-x x) (vec-y x)))
        (else x)))

(: partial-dec (-> Any Encoded))
(define (partial-dec x)
  (cond ((list? x) (map partial-dec x))
        ((real? x) x)
        ((string? x) x)
        ((symbol? x) x)
        ((boolean? x) x)
        ((par-vec? x) (vec (par-vec-x x) (par-vec-y x)))
        (else (error "invalid encoding"))))

(: save (-> Encoded Output-Port Void))
(define (save x port)
  (write (partial-enc x) port))

(: save-to (-> Encoded String Void))
(define (save-to x str)
  (call-with-output-file str (curry save x)))

(: load (-> Input-Port Encoded))
(define (load port)
  (let ((loaded (partial-dec (read port))))
    loaded))

(: load-from (-> String Encoded))
(define (load-from str)
  (call-with-input-file str load))

(: encoded? (-> Any Encoded))
(define-predicate encoded? Encoded)
