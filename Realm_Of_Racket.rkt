;; Introduction (Open Paren)

#|

(* 1 1)
(- 8 (* 2 3))
(sqrt 9)
(+ 1 2 3 4 5 6 7 8 9 0)
(sqrt (+ (sqrt 3)(sqrt 4)))

(list 1 2 3 4)
'(1 2 3 4)

(list (list 1 3 4) (list 2 4 6))
'((1 3 5) (2 4 6))

(list (list 'hello 'world)
      (list 'racket))
'((hello world) (racket))

|#


;; Chapter 1 (Getting Started)
#lang racket

;; Chapter 2 (A First Racket Program)
(define lower 1)

(define upper 100)

(define (guess)
  (quotient (+ lower upper) 2))

(define (return-five) 5)

(define (smaller)
  (set! upper (max lower (sub1 (guess))))
  (guess))

(define (bigger)
  (set! lower (min upper (add1 (guess))))
  (guess))

(define (start n m)
  (set! lower (min n m))
  (set! upper (max n m))
  (guess))

;; Chapter 3 (Basics of Racket)

;(zero? 1)
;(zero? (sub1 1))

;(symbol=? 'foo 'Foo)

;(expt 53 53)

;(sqrt -1)
;(* (sqrt -1) (sqrt -1))
;(* 1+1i 2+3i)
;(/ 4 6)
;(/ 4.0 6)

;(string-append "alex" "chonglian")
;(string-append "alex" " " "chonglian")

;(cons 1 2)
;(define cell (cons 'a 'b))
;cell
;(car cell)
;(cdr cell)
;(cons 'chicken empty)
;(cons 'chicken '())
;(cons 'pork '(chicken beef))
;(first '(pork chicken beef))
;(rest '(pork chicken beef))

(struct student (name id# dorm))
(define freshman1 (student 'Joe 1234 'NewHall))
(student-name freshman1)
(student-id# freshman1)






























