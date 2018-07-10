#lang racket

(require (for-syntax syntax/parse))
(require rackunit)

;;;
;; exercise 6
(define-syntax-rule
  (where body [iden val] ...)
  (let ([iden val] ...)
    body))

(check-equal?
 (where (+ my-favorite-number 2)
        [my-favorite-number 8])
 10)

(check-equal?
 (where (op 10 (+ my-favorite-number an-ok-number))
        [my-favorite-number 8]
        [an-ok-number 2]
        [op *])
 100)

(define-syntax (where* stx)  
  (syntax-parse stx
    [(_ body:expr [iden:id val:expr] ...)
     (define bindings (reverse (syntax->list #'([iden val] ...))))
     #`(let* #,bindings
         body)]))

(check-equal?
 (where* (list x y z)
         [x (+ y 4)]
         [y (+ z 2)]
         [z 1])
 (list 7 3 1))

;;;
;; exercise 7
(define-syntax (and/v stx)
  (syntax-parse stx
    #:literals (=>)
    [(_ val:expr (~optional (~seq => iden:id)) exps:expr ...)
     #'(~? (let ([iden val])
              (and val exps ...)) (and val exps ...))]))

(check-equal?
 (and/v 1 => x (+ x 1))
 2)
(check-equal?
 (and/v #f => x (+ x 1))
 #f)
(check-equal?
 (and/v (+ 1 1) (- 10 6))
 4)

;;;
;; exercise 8
; mon-aft-lab-8.rkt

;;; exercise 9
(define-syntax-rule
  (else)
  (raise-syntax-error #f "bam"))
