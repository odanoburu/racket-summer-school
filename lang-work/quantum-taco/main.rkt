#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (let ([tk (read ip)])
    (if (eof-object? tk)
        empty
        (cons tk (tokenize ip)))))

(define (parse tok)
  (define (parse-tk tk)
    (match tk
      [(list f args ...) (cons 'taco (map parse-tk args))]
      [(cons a b) (cons (parse-tk a) (parse-tk b))]
      [_ 'taco]))
    (map parse-tk tok))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (with-syntax ([(PT ...) parse-tree])
    #'(module tacofied racket
        'PT ...)))
