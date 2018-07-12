#lang racket

(require (for-syntax syntax/parse syntax/stx syntax/id-table))
(provide (rename-out [typechecking-mb #%module-begin])
         + if)
 
; A TyStx is a syntax object representing a type.
; A ExprStx is a syntax object representing an expression.
; A IdStx is an identifier syntax object.
 
(begin-for-syntax
 
  ; compute: ExprStx -> TyStx
  ; computes the type of the given term
  (define (compute e)
    (syntax-parse e
      [:integer #'Int]
      [:string #'String]
      [:boolean #'Bool]
      [((~literal if) e1 e2 e3)
       #:when (check #'e1 #'Bool)
       (let ([e2t (compute #'e2)])
         (when (check #'e3 e2t)
           e2t))]
      [((~literal +) e1 e2)
       #:when (and (check #'e1 #'Int) (check #'e1 #'Int))
       #'Int]
      [(~literal +)
       #'(-> Int Int Int)]
      #;[((~literal lambda) ([arg:id (~literal :) ty:expr] ...) body:expr)]
      #;[((~literal function-application) the-id:id arg:expr ...)
         ]
      #;[((~literal function-application lam:expr arg:expr ...)
          )]
      [e (raise-syntax-error
          'compute
          (format "could not compute type for term: ~a" (syntax->datum #'e)))]))
 
  ; check : ExprStx TyStx -> Bool
  ; checks that the given term has the given type
  (define (check e t-expected)
    (define t (compute e))
    (or (type=? t t-expected)
        (raise-syntax-error
         'check
         (format "error while checking term ~a: expected ~a; got ~a"
                 (syntax->datum e)
                 (syntax->datum t-expected)
                 (syntax->datum t)))))
 
  ; type=? : TyStx TyStx -> Bool
  ; type equality here is is stx equality
  (define (type=? t1 t2)
    (or (and (identifier? t1) (identifier? t2) (free-identifier=? t1 t2))
        (and (stx-pair? t1) (stx-pair? t2)
             (= (length (syntax->list t1))
                (Length (syntax->list t2)))
             (andmap type=? (syntax->list t1) (syntax->list t2))))))
 
(define-syntax typechecking-mb
  (syntax-parser
    [(_ e ...)
     ; prints out each term e and its type, it if has one;
     ; otherwise raises type error
     #:do[(stx-map
           (λ (e)
               (printf "~a : ~a\n"
                       (syntax->datum e)
                       (syntax->datum (compute e))))
           #'(e ...))]
     ; this language only checks types,
     ; it doesn't run anything
     #'(#%module-begin (void))]))

(begin-for-syntax
  (define (mk-empty-env)
    (make-immutable-free-id-table))

  (define (add-to-env env id ty)
    (free-id-table-set env id ty))

  (define (lookup-env env id)
    (free-id-table-ref env id #f)))
