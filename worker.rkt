#lang racket
(require racket/async-channel)

(provide dropping-worker)

(define (process-channel func channel)
  (let loop ((value (async-channel-get channel)))
    (let ((v2 (async-channel-try-get channel)))
      (when v2
        (loop v2)))
    (func value)
    (process-channel func channel)))

(define (dropping-worker func)
  (define channel (make-async-channel))
  (thread
   (lambda () (process-channel func channel)))
  (lambda (work)
    (async-channel-put channel work)))
