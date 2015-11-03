#lang typed/racket
(require "utils.rkt")

(provide SliderSetting OptionSetting Setting SliderSettingPrototype OptionSettingPrototype SettingPrototype 
         setting-key setting-prototype-key setting-type setting-instantiate setting->value setting-prototype->value setting-set-value!
         setting-slider-proto setting-positive-slider-proto setting-option-proto
         setting-bounds
         setting-slider? setting-option? setting-prototype-slider? setting-prototype-option?)

(struct setting-prototype [(key : String)])
(struct setting-prototype-slider setting-prototype ([min : Float] [max : Float] [mut : (-> (Mutable Float))]))
(struct setting-prototype-option setting-prototype ([mut : (-> (Mutable Boolean))]))
(define-type SliderSettingPrototype setting-prototype-slider)
(define-type OptionSettingPrototype setting-prototype-option)
(define-type SettingPrototype (U SliderSettingPrototype OptionSettingPrototype))

(struct setting-slider ([type : SliderSettingPrototype] [mut : (Mutable Float)]))
(struct setting-option ([type : OptionSettingPrototype] [mut : (Mutable Boolean)]))
(define-type SliderSetting setting-slider)
(define-type OptionSetting setting-option)
(define-type Setting (U SliderSetting OptionSetting))

(: setting-key (-> Setting String))
(define (setting-key setting)
  (setting-prototype-key
   (if (setting-slider? setting)
       (setting-slider-type setting)
       (setting-option-type setting))))

(: setting-bounds (-> SliderSetting (Values Float Float)))
(define (setting-bounds slider)
  (let ((proto (setting-slider-type slider)))
    (values (setting-prototype-slider-min proto) (setting-prototype-slider-max proto))))

(: setting-type (case-> [-> (U SliderSettingPrototype SliderSetting) 'slider]
                        [-> (U OptionSettingPrototype OptionSetting) 'option]
                        [-> (U SettingPrototype Setting) (U 'slider 'option)]))
(define (setting-type setting)
  (if (or (setting-prototype-slider? setting) (setting-slider? setting))
      'slider
      'option))

(: setting-slider-proto (-> String Float Float Float SliderSettingPrototype))
(define (setting-slider-proto name min max default)
  (setting-prototype-slider name min max (lambda () (mut-cell default))))

(: setting-positive-slider-proto (-> String Float Float Float SliderSettingPrototype))
(define (setting-positive-slider-proto name min max default)
  (define (make-mutable)
    (mut-wrap-set (lambda ([val : Float])
                    (if (positive? val)
                        val
                        (error "must be positive!")))
                  (mut-cell default)))
  (setting-prototype-slider name min max make-mutable))

(: setting-option-proto (-> String Boolean OptionSettingPrototype))
(define (setting-option-proto name default)
  (setting-prototype-option name (lambda () (mut-cell default))))

(: setting-instantiate (case-> [-> SliderSettingPrototype SliderSetting]
                               [-> OptionSettingPrototype OptionSetting]
                               [-> SettingPrototype Setting]))
(define (setting-instantiate prototype)
  (if (setting-prototype-slider? prototype)
      (setting-slider prototype ((setting-prototype-slider-mut prototype)))
      (setting-option prototype ((setting-prototype-option-mut prototype)))))

(: setting->value (case-> [-> SliderSetting Float]
                          [-> OptionSetting Boolean]
                          [-> Setting (U Float Boolean)]))
(define (setting->value setting)
  (if (setting-slider? setting)
      (mut-get (setting-slider-mut setting))
      (mut-get (setting-option-mut setting))))

(: setting-set-value! (-> Setting (U Float Boolean) Void))
(define (setting-set-value! setting value)
  (if (setting-slider? setting)
      (mut-set! (setting-slider-mut setting) (cast value Float)) ; TODO: don't cast!
      (mut-set! (setting-option-mut setting) (cast value Boolean))))

(: setting-prototype->value (case-> [-> SliderSettingPrototype Float]
                                    [-> OptionSettingPrototype Boolean]
                                    [-> SettingPrototype (U Float Boolean)]))
(define (setting-prototype->value setting)
  (if (setting-prototype-slider? setting)
      (mut-get ((setting-prototype-slider-mut setting)))
      (mut-get ((setting-prototype-option-mut setting)))))
