#lang typed/racket

(require/typed profile [profile-thunk (-> (-> Void) #:threads Boolean Void)])

(provide profile-thunk)
