#lang br/quicklang
(require brag/support) ; imports `lexer`

(module+ reader
  (provide read-syntax))
#;((# . '()
   ($ '))
   (% 'taco))

(define lex
  (lexer
   [(:+ (:or "%" "#$")) lexeme]
   [(:or "#" "$") (lex input-port)]))

(define (tokenize ip)
  (for/list ([tok (in-port lex ip)])
    (for/list ([c (in-string tok)]
               #:when (not (eqv? c #\$)))
      (if (eqv? c #\#)
          0
          1))))

(define (parse toks)
  (for/list ([tok (in-list toks)])
    (integer->char
     (for/sum ([val (in-list tok)]
               [power (in-naturals)]
               #:when (eq? val 'taco))
       (expt 2 power)))))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (parse toks))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module untaco racket
         (display (list->string 'PT))))))
