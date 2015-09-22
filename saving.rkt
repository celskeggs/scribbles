#lang typed/racket

(provide Encoded save-to load-from)

(define-type Encoded (U (Listof Encoded) Real String Symbol))
(: save (-> Encoded Output-Port Void))
(define (save x port)
  (write x port))

(: save-to (-> Encoded String Void))
(define (save-to x str)
  (call-with-output-file str (curry save x)))

(: load (-> Input-Port Encoded))
(define (load port)
  (let ((loaded (read port)))
    (if (encoded? loaded)
        loaded
        (error "invalid loaded file"))))

(: load-from (-> String Encoded))
(define (load-from str)
  (call-with-input-file str load))

(: encoded? (-> Any Encoded))
(define-predicate encoded? Encoded)
