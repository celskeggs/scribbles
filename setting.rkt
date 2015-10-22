#lang typed/racket
(require "utils.rkt")

(provide SliderSetting OptionSetting Setting SliderSettingPrototype OptionSettingPrototype SettingPrototype 
         setting-key setting-type setting-instantiate setting->value setting-prototype->value setting-set-value!
         setting-slider-proto setting-positive-slider-proto setting-option-proto)

(define-type SliderSettingPrototype (List String 'slider Real Real (-> (Mutable Real))))
(define-type OptionSettingPrototype (List String 'option (-> (Mutable Boolean))))
(define-type SettingPrototype (U SliderSettingPrototype OptionSettingPrototype))

(define-type SliderSetting (List String 'slider Real Real (Mutable Real)))
(define-type OptionSetting (List String 'option (Mutable Boolean)))
(define-type Setting (U SliderSetting OptionSetting))

(: setting-key (case-> [-> SettingPrototype String]
                       [-> Setting String]
                       [-> (U SettingPrototype Setting) String]))
(define (setting-key setting)
  (car setting))

(: setting-type (case-> [-> (U SliderSettingPrototype SliderSetting) 'slider]
                        [-> (U OptionSettingPrototype OptionSetting) 'option]
                        [-> (U SettingPrototype Setting) (U 'slider 'option)]))
(define (setting-type setting)
  (second setting))

(: setting-slider-proto (-> String Real Real Real SliderSettingPrototype))
(define (setting-slider-proto name min max default)
  (list name 'slider min max (lambda () (mut-cell default))))

(: setting-positive-slider-proto (-> String Real Real Real SliderSettingPrototype))
(define (setting-positive-slider-proto name min max default)
  (list name 'slider min max (lambda () (mut-wrap-set (lambda ([val : Real]) (if (positive? val)
                                                                                 val
                                                                                 (error "must be positive!")))
                                                      (mut-cell default)))))

(: setting-option-proto (-> String Boolean OptionSettingPrototype))
(define (setting-option-proto name default)
  (list name 'option (lambda () (mut-cell default))))

(: setting-instantiate (case-> [-> SliderSettingPrototype SliderSetting]
                               [-> OptionSettingPrototype OptionSetting]
                               [-> SettingPrototype Setting]))
(define (setting-instantiate prototype)
  (if (eq? (second prototype) 'slider)
      (list (first prototype) 'slider (third prototype) (fourth prototype) ((fifth prototype)))
      (list (first prototype) 'option ((third prototype)))))

#|(: setting-slider (-> String Real Real (Mutable Real) SliderSetting))
(define (setting-slider name min max value-box)
  (list name 'slider min max value-box))

(: setting-positive-slider (-> String Positive-Real Positive-Real (Mutable Positive-Real) SliderSetting))
(define (setting-positive-slider name min max value-box)
  (setting-slider name min max
                  (mut-make (lambda () (mut-get value-box))
                            (lambda ([val : Real]) (if (positive? val)
                                                       (mut-set! value-box val)
                                                       (error "must be positive!")))))) ; should be ensured by the bounds

(: setting-option (-> String (Mutable Boolean) OptionSetting))
(define (setting-option name value-box)
  (list name 'option value-box))|#

(: setting->value (case-> [-> SliderSetting Real]
                          [-> OptionSetting Boolean]
                          [-> Setting (U Real Boolean)]))
(define (setting->value setting)
  (if (eq? (cadr setting) 'slider)
      (mut-get (fifth setting))
      (mut-get (third setting))))

(: setting-set-value! (-> Setting (U Real Boolean) Void))
(define (setting-set-value! setting value)
  (if (eq? (cadr setting) 'slider)
      (mut-set! (fifth setting) (cast value Real)) ; TODO: don't cast!
      (mut-set! (third setting) (cast value Boolean))))

(: setting-prototype->value (case-> [-> SliderSettingPrototype Real]
                                    [-> OptionSettingPrototype Boolean]
                                    [-> SettingPrototype (U Real Boolean)]))
(define (setting-prototype->value setting)
  (if (eq? (cadr setting) 'slider)
      (mut-get ((fifth setting)))
      (mut-get ((third setting)))))
