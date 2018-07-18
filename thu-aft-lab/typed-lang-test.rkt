#lang s-exp "typed-lang.rkt"

(require turnstile/rackunit-typechecking)
 
(check-type 5 : Int)
(check-type "five" : String)
 
;(typecheck-fail #f #:with-msg "Unsupported literal")
(typecheck-fail 1.1 #:with-msg "Unsupported literal")
 
(check-type + : (-> Int Int Int))
 
(check-type (+ 1 2) : Int -> 3)
 
(typecheck-fail (+ 1))

(check-type #t : Bool)
(typecheck-fail (check-type (if #t 1 "2") : Int))
(check-type (lambda ([x : Int][y : String]) #f) : (-> Int String Bool))
(check-type (lambda ([x : Int]) #f) : (-> Int Bool))
(check-type ((lambda ([x : Int]) #f) 1) : Bool)
