#lang turnstile/quicklang

(provide Int Bool String ->
         (rename-out [typed-app #%app]
                     [typed-+ +]
                     [typed-lambda lambda]
                     [typed-if if]
                     [typed-datum #%datum]))

(define-base-types Int Bool String)
(define-type-constructor -> #:arity > 0)

(define-primop typed-+ + : (-> Int Int Int))
 
#;(define-typerule (typed-app f e ...) ≫
    [⊢ f ≫ f- ⇒ (~-> τin ... τout)]
    #:fail-unless (stx-length=? #'[τin ...] #'[e ...])
    (format "arity mismatch, expected ~a args, given ~a"
            (stx-length #'[τin ...]) #'[e ...])
    [⊢ e ≫ e- ⇐ τin] ...
    --------------------
    [⊢ (#%plain-app f- e- ...) ⇒ τout])

(define-typerule typed-app
  [(_ f a ...) ⇐ t ≫
   [⊢ a ≫ a- ⇒ at] ...
   [⊢ f ≫ f- ⇐ (-> at ... t)]
   --------------------
   [⊢ (#%app f- a- ...)]]
  [(_ f e ...) ≫
   [⊢ f ≫ f- ⇒ (~-> τin ... τout)]
   [⊢ e ≫ e- ⇐ τin] ...
   --------------------
   [⊢ (#%app f- e- ...) ⇒ τout]])


(define-typerule (typed-if b e1 e2) ≫
  [⊢ b ≫ b- ⇐ Bool]
  [⊢ e1 ≫ e1- ⇒ t1]
  [⊢ e2 ≫ e2- ⇐ t1]
  -----------------
  [⊢ (if- b- e1- e2-) ⇒ t1])


(define-typerule (typed-lambda ([v (~literal :) ty] ...) e) ≫
  [[v ≫ v- : ty] ... ⊢ e ≫ e- ⇒ t]
  ---------
  [⊢ (λ- (v- ...) -e) ⇒ (-> ty ... t)])

(define-typerule typed-datum
  [(_ . n:integer) ≫
   -------------
   [⊢ (#%datum . n) ⇒ Int]]
  [(_ . b:boolean) ≫
   -------------
   [⊢ (#%datum . b) ⇒ Bool]]
  [(_ . s:str) ≫
   -------------
   [⊢ (#%datum . s) ⇒ String]]
  [(_ . x) ≫
   --------
   [#:error (type-error #:src #'x #:msg "Unsupported literal: ~v" #'x)]])
