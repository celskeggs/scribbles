#lang typed/racket
(require "utils.rkt")

(provide Setting
         setting-slider setting-positive-slider setting-option)

(define-type Setting (U (List String 'slider Real Real (Mutable Real))
                        (List String 'option (Mutable Boolean))))

(: setting-slider (-> String Real Real (Mutable Real) Setting))
(define (setting-slider name min max value-box)
  (list name 'slider min max value-box))

(: setting-positive-slider (-> String Positive-Real Positive-Real (Mutable Positive-Real) Setting))
(define (setting-positive-slider name min max value-box)
  (setting-slider name min max
                  (mut-make (lambda () (mut-get value-box))
                            (lambda ([val : Real]) (if (positive? val)
                                                       (mut-set! value-box val)
                                                       (error "must be positive!")))))) ; should be ensured by the bounds

(: setting-option (-> String (Mutable Boolean) Setting))
(define (setting-option name value-box)
  (list name 'option value-box))

(: setting->value (-> Setting (U Real Boolean)))
(define (setting->value setting)
  (if (eq? (cadr setting) 'slider)
      (mut-get (fifth setting))
      (mut-get (third setting))))
