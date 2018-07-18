#lang s-exp "morn-typed-lang.rkt"
; e.g., save this to file "morn-typed-prog.rkt"
5
#f
"five"
;1.1 ;; err
;(if 1 2 3) ;; err
(if #f 2 3)
(if (if #t #f #t) 1 2)
(lambda ([x : Int] [y : Int]) x)
((lambda ([x : Bool]) x) #t)
((lambda ([x : Int] [y : Int]) x) 1 2)
+
(+ 1 2)
;(lambda ([x : Int]) x)
;(+ #f 1) ;; err
 
; Running the program prints:
; 5 : Int
; #f : Bool
; five : String
; (if #f 2 3) : Int
; + : (-> Int Int Int)
; (+ 1 2) : Int
