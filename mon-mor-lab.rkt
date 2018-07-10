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
(define-rewrite-rule
  (loop v0 ((v1 e1) ...) e0 ...)
  (letrec ([v0 (lambda (v1 ...)
              e0 ...)])
    (v0 e1 ...)))

(check-equal?
 (loop fac ([n 10])
       (if (zero? n)
           1
           (* n (fac (sub1 n)))))
 3628800)
;;;
;; exercise 3
(define-rewrite-rule
  (all bs ...)
  (if (and bs ...)
      (list bs ...)
      #f))

(check-equal? (all #f 1) #f)
(check-equal? (all 0 1 2) (list 0 1 2))
(check-equal? (all 0 1 2 #f) #f)

;;;
;; exercise 4

