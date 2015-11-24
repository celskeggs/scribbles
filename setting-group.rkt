#lang typed/racket
(require "utils.rkt")
(require "setting.rkt")

(provide SettingGroup SettingPrototypeGroup EncodedSettingGroup
         setting-prototype-group-add setting-prototype-group-empty setting-prototype-group-ref setting-prototype-group-slider-ref setting-prototype-group-option-ref
         setting-group-instantiate
         setting-group-slider-ref setting-group-option-ref setting-group-ref
         setting-group-load setting-group-save)

(define-type SettingPrototypeGroup (Listof SettingPrototype))
(define-type SettingGroup (Listof Setting))
(define-type EncodedSettingGroup (List String (Listof (U Float Boolean))))

(: setting-prototype-group-empty SettingPrototypeGroup)
(define setting-prototype-group-empty empty)

(: setting-prototype-group-add (-> SettingPrototype SettingPrototypeGroup SettingPrototypeGroup))
(define (setting-prototype-group-add setting group)
  (for ((old-setting group))
    (when (equal? (setting-prototype-key old-setting) (setting-prototype-key setting))
      (error "Name already exists in setting group:" (setting-prototype-key setting))))
  (cons setting group))

; ============

(: setting-prototype-group-option-ref (-> SettingPrototypeGroup String OptionSettingPrototype))
(define (setting-prototype-group-option-ref group name)
  (let ((ref (setting-prototype-group-ref group name)))
    (if (eq? 'option (setting-type ref))
        (cast ref OptionSettingPrototype)
        (error "Setting is not an option setting:" name))))

(: setting-prototype-group-slider-ref (-> SettingPrototypeGroup String SliderSettingPrototype))
(define (setting-prototype-group-slider-ref group name)
  (let ((ref (setting-prototype-group-ref group name)))
    (if (eq? 'slider (setting-type ref))
        (cast ref SliderSettingPrototype) ; TODO: shouldn't cast...
        (error "Setting is not a slider setting:" name))))

(: setting-prototype-group-ref (-> SettingPrototypeGroup String SettingPrototype))
(define (setting-prototype-group-ref group name)
  (cond ((empty? group) (error "No such setting in setting group:" name))
        ((equal? name (setting-prototype-key (car group))) (car group))
        (else (setting-prototype-group-ref (cdr group) name))))

; ============

(: setting-group-instantiate (-> SettingPrototypeGroup SettingGroup))
(define (setting-group-instantiate group)
  (map setting-instantiate group))

; ============

(: setting-group-option-ref (-> SettingGroup String OptionSetting))
(define (setting-group-option-ref group name)
  (let ((ref (setting-group-ref group name)))
    (if (eq? 'option (setting-type ref))
        (cast ref OptionSetting)
        (error "Setting is not an option setting:" name))))

(: setting-group-slider-ref (-> SettingGroup String SliderSetting))
(define (setting-group-slider-ref group name)
  (let ((ref (setting-group-ref group name)))
    (if (eq? 'slider (setting-type ref))
        (cast ref SliderSetting) ; TODO: shouldn't cast...
        (error "Setting is not a slider setting:" name))))

(: setting-group-ref (-> SettingGroup String Setting))
(define (setting-group-ref group name)
  (cond ((empty? group) (error "No such setting in setting group:" name))
        ((equal? name (setting-key (car group))) (car group))
        (else (setting-group-ref (cdr group) name))))

; ============

(: setting-group->pairs (-> SettingGroup (Listof (Pairof String (U Float Boolean)))))
(define (setting-group->pairs group)
  (sort
   (for/list : (Listof (Pairof String (U Float Boolean))) ((setting group))
     (cons (setting-key setting) (setting->value setting)))
   (lambda ([a : (Pairof String Any)] [b : (Pairof String Any)])
     (string<? (car a) (car b)))))

(: setting-prototype-group->pairs (-> SettingPrototypeGroup (Listof String)))
(define (setting-prototype-group->pairs proto)
  (sort (map setting-prototype-key proto)
        string<?))

(: setting-prototype-group-signature (-> SettingPrototypeGroup String))
(define (setting-prototype-group-signature proto)
  (when (member #\newline (string->list (string-append* (setting-prototype-group->pairs proto))))
    (error "Newlines are not valid in setting names!")) ; TODO: check this earlier?
  (string-join (setting-prototype-group->pairs proto) "\n"))

(: setting-group-save (-> SettingGroup SettingPrototypeGroup EncodedSettingGroup))
(define (setting-group-save group proto)
  (let ((pairs (setting-group->pairs group)) (expected (setting-prototype-group->pairs proto)) (signature (setting-prototype-group-signature proto)))
    (unless (equal? (map-cars pairs) expected)
      (error "Mismatch between setting group and prototype:" pairs expected))
    (list signature (map-cdrs pairs))))

(: setting-group-load (-> EncodedSettingGroup SettingPrototypeGroup SettingGroup))
(define (setting-group-load enc proto)
  (let ((signature-loaded (car enc))
        (vals (car (cdr enc)))
        (signature-expected (setting-prototype-group-signature proto))
        (names (setting-prototype-group->pairs proto))
        (group (setting-group-instantiate proto)))
    (unless (equal? signature-loaded signature-expected)
      (error "Mismatch between saved setting group signature and prototype!"))
    (for ((name names)
          (val vals))
      (setting-set-value! (setting-group-ref group name) val))
    group))
