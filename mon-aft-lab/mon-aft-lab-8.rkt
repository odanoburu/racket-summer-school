#lang racket
 
(require (for-syntax syntax/parse))
 
(begin-for-syntax
  (define-syntax-class byte
    (pattern b:nat #:fail-unless (< (syntax-e #'b) 256) "not a byte")))
 
; SYNTAX
; (split-ct tags start end [name:id step (~optional convert)] ...)
; computes the values of the fields name... by successively extracting
; bytes from tags, beginning at start to maximally end
(define-syntax (split-ct stx)
  (syntax-parse stx
    [(_ tags start:expr end:expr [name step:byte (~optional convert)] ...)
     ; ———————————
     ; the static error checking 
     #:do [(define end-int  (syntax-e #'end))
           (define step-int (sum #'(step ...)))]
     #:fail-unless (when (and (number? end-int) (number? step-int)) (< step-int end-int)) "index out of range"
     ; ———————————
     #`(let ([i start])
         (let*-values ([(i name) (values (+ i step) (extract tags i (+ i step -1)))]
                       ...)
           (values ((~? convert values) name) ...)))]))
 
; [Listof [Syntax Number]] -> Number
; compute the sum of the numbers hidden in syntax 
(define-for-syntax (sum list-of-syntax-numbers)
  (apply + (map syntax-e (syntax->list list-of-syntax-numbers))))
