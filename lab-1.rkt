#lang racket

(require syntax/parse)
(require (for-syntax syntax/parse))
(require rackunit)
(require (for-syntax racket/list))
(require (for-syntax racket/stream))
(require racket/generator)

;;;
;; exercise 1
#;(define-syntax (define-world* stx)
  (define size (sub1 (length (syntax->list stx))))
  (define param (range size))
  (syntax-parse stx
    [(_ the-id:id ...)
     #`(define-values (the-id ...) (values #,@param))]))

(define-syntax (define-world* stx)
  (syntax-parse stx
    [(_ the-id:id ...)
     #`(begin (define ix (sequence->generator (in-naturals)))
              (define the-id (ix)) ...)]))

(define-world* x y z)
(check-equal? (list x y z) (list 0 1 2))

;;;
;; exercise 5
(define-syntax (define-rewrite-rule stx)
  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr ...)))
  
  (syntax-parse stx
    [(_ b:binding body:expr)
     #'(define-syntax (b.var stx-)
         (syntax-parse stx-
           [(_ b.rhs ...)
            #'body]))]))

(define-rewrite-rule
  (loop-for-ever exp)
  ; —> 
  (local ((define (for-ever) (begin exp (for-ever)))) (for-ever)))

(define-rewrite-rule
  (zero)
  0)

;;;
;; exercise 2
#;(define-rewrite-rule
    (loop v0 ((v1 e1) ...) e0 ...)
    
    
  )

;;;
;; exercise 3
(define-rewrite-rule
  (all bs ...)
  (if (and bs ...)
      (list bs ...)
      #f))

;;;
;; exercise 4

