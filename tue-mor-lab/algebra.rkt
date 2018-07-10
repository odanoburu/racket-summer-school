#lang racket
(require (for-syntax syntax/parse))
 
(provide #%module-begin
         (rename-out [literal #%datum]
                     [plus +]
                     [minus -]
                     [times *]
                     [divide /]
                     [if-macro if]
                     [complain-app #%app]
                     [unwrap #%top-interaction])
         then
         else
         define-function
         function-application)
 
(define-syntax (literal stx)
  (syntax-parse stx
    [(_ . v:number) #'(#%datum . v)]
    [(_ . v:boolean) #'(#%datum . v)]
    [(_ . s:string) #'(quote s)]
    [(_ . other) (raise-syntax-error #f "not allowed" #'other)]))

(define-syntax (if-macro stx)
  (syntax-parse stx
    #:literals  (then else)
    [(_ guard:expr then e1:expr else e2:expr)
     #'(if guard e1 e2)]))

(define-syntax (then stx)
  (syntax-parse stx
    [(_) (raise-syntax-error #f "not allowed")]))

(define-syntax (else stx)
  (syntax-parse stx
    [(_) (raise-syntax-error #f "not allowed")]))

 
(define-syntax (complain-app stx)
  (define (complain msg src-stx)
    (raise-syntax-error 'parentheses msg src-stx))
  (define without-app-stx
    (syntax-parse stx [(_ e ...) (syntax/loc stx (e ...))]))
  (syntax-parse stx
    [(_)
     (complain "empty parentheses are not allowed" without-app-stx)]
    [(_ n:number)
     (complain "extra parentheses are not allowed around numbers" #'n)]
    [(_ x:id _ ...)
     (complain "unknown operator" #'x)]
    [_
     (complain "something is wrong here" without-app-stx)]))

(define-syntax (unwrap stx)
  (syntax-parse stx
    [(_ . e) #'e]))

(require (for-syntax syntax/parse racket/list))

(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (function-name:id parameter:id ...) body:expr)
     (define arity (length (syntax->list #'(parameter ...))))
     #`(define-syntax function-name
         (cons #,arity #'(lambda (parameter ...) body)))]))

(define-for-syntax (lookup id stx)
  ; -> Empty
  ; EFFECT abort process with syntax error 
  (define (failure)
    (define msg (format "undefined function: ~a" (syntax-e id)))
    (raise-syntax-error #f msg stx))
  (define result (syntax-local-value id failure))
  (values (car result) (cdr result)))

(define-syntax (function-application stx)
  (syntax-parse stx
    [( _ function-name:id argument:expr ...)     
     #:do ((define-values (arity function) (lookup #'function-name stx)))
     #:fail-unless (= arity (length (syntax->list #'(argument ...)))) "wrong number of arguments"
     #`(#,function argument ...)]))

(define-syntax (define-predef-fun stx)
  (syntax-parse stx
    [(_ (the-id:id args:expr ...) body:expr)
     #`(define-syntax (the-id stx)
         (syntax-parse stx
           [(_ args ...)
            #'body]))]))

(define-predef-fun
  (plus x y) (+ x y))

(define-predef-fun
  (minus x y) (- x y))

(define-predef-fun
  (times x y) (* x y))

(define-predef-fun
  (divide x y) (/ x y))
