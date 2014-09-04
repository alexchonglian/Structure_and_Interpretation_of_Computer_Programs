;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 2
;; Building Abstraction with Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

;; if (a b x y) are not real numbers
;; then define add and mul for other types of data
(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.1 Introduction to Data Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; constructor: make rational with numerator and denominator
;(make-rat <n> <d>)

;; selector
;(numer <x>)
;(denom <x>)



(define (add-rat x y)
  (make-rat (+ (* (numer x)(denom y))
               (* (numer y)(denom x)))
            (* (denom x)(denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x)(denom y))
               (* (numer y)(denom x)))
            (* (denom x)(denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x)(numer y))
            (* (denom x)(denom y))))

(define (div-rat x y)
  (make-rat (* (numer x)(denom y))
            (* (numer y)(denom x))))

(define (equal-rat? x y)
  (= (* (numer x)(denom y))
     (* (numer y)(denom x))))

(define x (cons 1 2))
;(car x)
;(cdr x)
(define y (cons 3 4))
(define z (cons x y))
;(car (car z))
;(car (cdr z))

(define (make-rat n d)(cons n d))
(define (numer x)(car x))
(define (denom x)(cdr x))

;; another way of defining make-rat 
;; instead of make-rat calling cons, make-rat is cons
;; more efficient
;(define make-rat cons)
;(define numer car)
;(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;(print-rat x)
;(print-rat y)

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

;(print-rat (add-rat one-half one-third))
;5/6
;(print-rat (mul-rat one-half one-third))
;1/6
;(print-rat (add-rat one-third one-third))
;6/9

;reduce numbers to lowest term
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)(/ d g))))

;(print-rat (add-rat one-third one-third))
;2/3


'(exercise 2 1)#|
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))

(print-rat (make-rat 2 4))
(print-rat (make-rat -2 4))
(print-rat (make-rat -2 -4))
(print-rat (make-rat 2 -4))
|#

'(exercise 2 2)#|
;; point
(define (make-point x y)(cons x y))
(define (x-point p)(car p))
(define (y-point p)(cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; segment
(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (define (average x y) (/ (+ x y) 2.0))
  (let ((a (start-segment segment))
        (b (end-segment segment)))
    (make-point (average (x-point a)(x-point b))
                (average (y-point a)(y-point b)))))

;; testing
(define seg (make-segment (make-point 2 3)(make-point 8 5))) 

(print-point (midpoint-segment seg)) 
|#

'(exercise 2 3)#|
;; Point 
(define (make-point x y) (cons x y)) 
(define (x-point p) (car p)) 
(define (y-point p) (cdr p)) 

;; Rectangle
;; one possible implementation 
(define (make-rect bottom-left top-right) 
  (cons bottom-left top-right)) 

(define (bottom-left rect) (car rect))
(define (top-right rect) (cdr rect))
(define (bottom-right rect)(make-point (x-point (cdr rect)) 
                                       (y-point (car rect)))) 
(define (top-left rect)(make-point (x-point (car rect)) 
                                   (y-point (cdr rect)))) 


(define (width-rect rect)
  (abs (- (x-point (bottom-left rect))
          (x-point (bottom-right rect))))) 
(define (height-rect rect) 
  (abs (- (y-point (bottom-left rect))
          (y-point (top-left rect))))) 
(define (area-rect rect) 
  (* (width-rect rect) (height-rect rect))) 
(define (perimeter-rect rect) 
  (* (+ (width-rect rect) (height-rect rect)) 2)) 


(define r (make-rect (make-point -1 -2)(make-point 2 1)))
(area-rect r)
(perimeter-rect r)
|#


;; procedural representation of "pairs"
;; this style of programming is called "message passing"
(define (cons! x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (display "invalid input"))))
  dispatch)

(define data (cons! 7 8))
;data ;; #<procedure:dispatch>
;(data 0)
;(data 1)
(define (car! z)(z 0))
(define (cdr! z)(z 1))
;(car! data)
;(cdr! data)

'(exercise 2 4)#|
;; another procedural rep of "pairs"
(define (cons! x y)
  (lambda (m)(m x y)));apply m to x y

(define data (cons! 7 8))
data ;; #<procedure:dispatch>
(data (lambda (x y) x));pass in a proc to choose 1st param
(data (lambda (x y) y));pass in a proc to choose 2nd param

;
(define (car! z)
  (z (lambda (p q) p)))
(define (cdr! z)
  (z (lambda (p q) q)))

(car! data)
(cdr! data)

;lets walk thru the derivation
;(car (cons x y)) 
;((cons x y) (lambda (p q) p)) 
;((lambda (m) (m x y)) (lambda (p q) p)) 
;((lambda (p q) p) x y) 
;x
|#


'(exercise 2 5)#|
(define (cons! a b)
  (* (expt 2 a)(expt 3 b)))

(define data (cons! 7 8))
data

;helper function to extract max exponent of m
;m can be 2 or 3
(define (div-iter n m result)
  (if (= 0 (remainder n m))
      (div-iter (/ n m) m (+ 1 result))
      result))

(define (div-recur n base)
  (if (= 0 (remainder n base))
      (+ 1 (div-recur (/ n base) base))
      0))
;(div-recur 72 2);3
;(div-recur 72 3);2
;(div-iter 72 2 0);3
;(div-iter 72 3 0);2

(define (car! z)(div-iter z 2 0))
(define (cdr! z)(div-iter z 3 0))
(car! data);7
(cdr! data);8
|#

'(exercise 2 6)#|
;; Church numerals
(define zero (lambda (f)(lambda (x) x)))
(define (add-1 n)(lambda (x)(f ((n f) x))))

;(add-1 zero)
;derivation as follows:
;((n f) x)
;((zero f) x)
;(((lambda (f)(lambda (x) x)) f) x)
;((lambda (x) x) x)
;x
(define one (lambda (f)(lambda (x)(f x))))

;(add-1 one)
;derivation as follows:
;((n f) x)
;((one f) x)
;(((lambda (f)(lambda (x)(f x))) f) x)
;((lambda (x)(f x)) x)
;(f x)
(define two (lambda (f)(lambda (x)(f (f x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f)(lambda (x) (f (f (f x)))))) 


(define (add-cn a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; thanks to Ken Dyck
;; my brief explanation:
;; the intuition for (a f)((b f) x) is that
;; 0 <=> x
;; 1 <=> (f x)
;; 2 <=> (f (f x))
;; 3 <=> (f (f (f x)))
;; we find:
;; value(church-numeral)= #[f apply to x] p.s. '#'means num-of-times
;; a maps f => (f(f(f...(f x) )))) [repeat a times]
;; b maps f => (f(f(f...(f x) )))) [repeat b times]
;; joining a&b, we get f => (f(f(f...(f x) )))) [repeat a+b times]
;; plz refer to advanced material for rigorous explanation


;; converter of integer-to-church
;; recursive
(define (int-to-church n)
  (if (= n 0)
      zero
      (add-1 (int-to-church (- n 1)))))

;; converter of integer-to-church
;; iterative
(define (int-to-church n)
  (define (iter i result)
    (if (= i n)
        result
        (iter (+ i 1) (add-1 result))))
  (iter 0 zero))

;; converter of church-to-integer
(define (church-to-int cn)
  ((cn (lambda (n) (+ n 1))) 0))

(church-to-int zero);0
(church-to-int one);1
(church-to-int two);2
(church-to-int three);3
(church-to-int (add-cn two three));5
|#


;; Alyssa's interval arithmetics
(define (make-interval lower upper)(cons lower upper))
(define (lower-bound x)(min (car x)(cdr x)))
(define (upper-bound x)(max (car x)(cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)(lower-bound y))
                 (+ (upper-bound x)(upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)(lower-bound y)))
        (p2 (* (lower-bound x)(upper-bound y)))
        (p3 (* (upper-bound x)(upper-bound y)))
        (p4 (* (upper-bound x)(lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


'(exercise 2 7)#|
(define (make-interval lower upper)(cons lower upper))
(define (lower-bound x)(min (car x)(cdr x)))
(define (upper-bound x)(max (car x)(cdr x)))

(define i1 (make-interval 1 2))
(define i2 (make-interval 2 1))
i1
i2
(lower-bound i1)
(lower-bound i2)
;; to make sure to put 'em in the right place
;; we can check if lower<upper in contructor instead of selector
(define (make-interval x y)(cons (min x y) (max x y)))
(define (lower-bound x)(car x))
(define (upper-bound x)(cdr x))
|#

'(exercise 2 8)#|
(define (sub-interval x y)
  (make-interval (- (lower-bound x)(upper-bound y))
                 (- (upper-bound x)(lower-bound y))))

(define i1 (make-interval 2 7))
(define i2 (make-interval 8 3))

i1
i2
(sub-interval i1 i2)
(sub-interval i2 i1)
|#


'(exercise 2 9)#|
(define (sub-interval x y)
  (make-interval (- (lower-bound x)(upper-bound y))
                 (- (upper-bound x)(lower-bound y))))

(define (width i)(/ (- (upper-bound i) (lower-bound i)) 2.0))

(define i+ (make-interval -10 -20))
(define i- (make-interval 10 20))
(define ic (make-interval 1 -1))

(width i+);5
(width i-);5
(width ic);1
(newline)
(width (add-interval i+ ic));5+1=6
(width (add-interval i- ic));5+1=6
(width (sub-interval i+ ic));5+1=6
(width (sub-interval i- ic));5+1=6
(newline)
(width (mul-interval i+ i+));150
(width (mul-interval i- i-));150
(width (mul-interval ic ic));1
(newline)
(width (mul-interval i+ ic));20
(width (mul-interval i- ic));20
(width (mul-interval i+ i-));150

;; another example
;; [0, 10] * [0, 2] = [0, 20]   (width = 10)
;; [-5, 5] * [-1,1] = [-5, 5]   (width = 5)
|#

'(exercise 2 10)#|
(define (div-interval x y)
  (if (>= 0 (* (lower-bound y)
               (upper-bound y)))
      (display "div-by-zero! interval-0-span") 
      (mul-interval x (make-interval (/ 1. (upper-bound y)) 
                                     (/ 1. (lower-bound y)))))) 
|#

'(exercise 2 11)#|
(define (endpoint-sign i)
  ;; endpoint-sign returns:
  ;;     +1 if both non-negative
  ;;     -1 if both negative
  ;;      0 if opposite sign
  (cond ((and (>= (upper-bound i) 0)
              (>= (lower-bound i) 0))
         1)
        ((and (< (upper-bound i) 0)
              (< (lower-bound i) 0))
         -1)
        (else 0)))

(define (mul-interval x y)
  (let ((xH (upper-bound x))
        (xL (lower-bound x))
        (yH (upper-bound y))
        (yL (lower-bound y))
        (x-es (endpoint-sign x))
        (y-es (endpoint-sign y)))
    (cond ((= x-es 1) ;; both x endpoints (+)
           (cond ((> y-es 0) 
                  (make-interval (* xL yL) (* xH yH))) 
                 ((< y-es 0) 
                  (make-interval (* xH yL) (* xL yH))) 
                 (else 
                  (make-interval (* xH yL) (* xH yH))))) 
          
          ((= x-es -1) ;; both x endpoints (-)
           (cond ((> y-es 0) 
                  (make-interval (* xL yH) (* xH yL))) 
                 ((< y-es 0) 
                  (make-interval (* xH yH) (* xL yL))) 
                 (else 
                  (make-interval (* xL yH) (* xH yL))))) 
          
          (else  ;; x spans 0 
           (cond ((> y-es 0) 
                  (make-interval (* xL yH) (* xH yH))) 
                 ((< y-es 0) 
                  (make-interval (* xH yL) (* xL yL))) 
                 (else ;; both x and y span 0, need to check values 
                  (make-interval (min (* xL yH) (* xH yL)) 
                                 (max (* xL yL) (* xH yH)))))))))
|#

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define i (make-center-width 100 5))
;(center i)
;100
;(width i)
;5

'(exercise 2 12)#|
(define (make-center-percent c p)
  (let ((q (/ p 100.0)))
    (make-interval (* c (- 1 q)) (* c (+ 1 q)))))

(define i (make-center-percent 100 5))
i
;(95.0 . 105.0)

(define (percent x)
  (let ((xL (lower-bound x))
        (xH (upper-bound x)))
    (* 100 (/ (- xH xL) (+ xH xL)))))

(percent i)
;5.0
|#


'(exercise 2 13)#|
; Ca = center of a
; Ta = tolerance of a
; a = [Ca*(1 - 0.5*Ta), Ca*(1 + 0.5*Ta)]
; b = [Cb*(1 - 0.5*Tb), Cb*(1 + 0.5*Tb)]
; if the endpoints are positive
; a*b = [Ca*Cb*(1 - 0.5*(Ta + Tb) + 0.25*Ta*Tb),
;        Ca*Cb*(1 + 0.5*(Ta + Tb) + 0.25*Ta*Tb)]
; Ta*Tb can be ignored
; thus tolerances of a*b is approximately Ta+Tb
|#



;; experiment with parallel
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define i1 (make-interval 59 61));60+e
(define i2 (make-interval 29 31));30+e

;(par1 i1 i2)
;(18.59782608695652 . 21.488636363636363)
;(par2 i1 i2)
;(19.44318181818182 . 20.554347826086953)

;; reason why?


'(exercise 2 14)#|
(define A (make-interval 1 2))
(define B (make-interval 1 2))
A;=[1 2]
B;=[1 2]
(div-interval A B);=[0.5 2.0]
;np, because A B are different interval
;if A=1 B=2 then A/B = 0.5
;if A=2 B=1 then A/B = 2.0

;; now the problem is if we ...
(div-interval A A)
;we all know that no matter which value in the interval A takes 
;A/A should be 1 deterministically
;but the result is [0.5 2.0]
;so Alyssa's system introduces extra uncertainty
;by failing to deal with identity issue when calculating A/A
|#


'(exercise 2 15)
;Eva Lu Ator is right
;(/ (+ r1 r2) (* r1 r2)) introduces extra error

'(exercise 2 16)
;http://wiki.drewhess.com/wiki/SICP_exercise_2.16


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.2 Hierarchical Data and the Closure Property
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cool graphs ^ ^
;; list = seq of pairs formed by nested cons-es

;(cons 1 (cons 2 (cons 3 (cons 4 '()))))
;(list 1 2 3 4)
;'(1 2 3 4)

;(car '(1 2 3 4))
;(cdr '(1 2 3 4))
;(cadr '(1 2 3 4))
;(car (cdr '(1 2 3 4)))

;; list-ref is builtin in DrRacket R5RS
;; add ! to overwrite
(define (list-ref! items n)
  (if (= n 0)
      (car items)
      (list-ref! (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

;(list-ref squares 3)
;(list-ref '(1 4 9) 3);violation, 0 indexed
;(list-ref '(1 4 9 16) 3);
;16

;; length is builtin in DrRacket R5RS
;; add ! to overwrite
;recursive
(define (length! items)
  (if (null? items)
      0
      (+ 1 (length! (cdr items)))))

;(length! '(1 2 3 4))
;4

;iterative
(define (length! items)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ 1 count))))
  (iter items 0))

;(length! '(1 2 3 4))
;4


;; append is builtin in DrRacket R5RS
;; add ! to overwrite
(define (append! l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append! (cdr l1) l2))))

;(append! '(1 2 3) '(4 5 6))
;(1 2 3 4 5 6)

'(exercise 2 17)#|
(define (last-pair list)
  (define (last-pair-not-null l)
    (if (null? (cdr l));next one null?
        l
        (last-pair-not-null (cdr l))))
  ;; if not null then go into recursion
  (if (null? list)
      '()
      (last-pair-not-null list)))

(last-pair '())
(last-pair '(1))
(last-pair '(1 2 3 4 5 6))
|#


'(exercise 2 18)#|
;; reverse is builtin in DrRacket R5RS
;; add ! to overwrite
(define (reverse! list)
  (define (iter rest result)
    (if (null? rest)
        result
        (iter (cdr rest) (cons (car rest) result))))
  (iter list '()))

(reverse! '(1 2 3 4))
;(4 3 2 1)

(define (reverse! list)
  (if (null? list)
      '()
      (append (reverse! (cdr list))(cons (car list) '()))))
(reverse! '(1 2 3 4))
;(4 3 2 1)
|#


'(exercise 2 19)#|
(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))

(define (cc amount coin-val)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0)(no-more? coin-val)) 0)
    ((+ (cc;;not using current coin-value
         amount
         (except-first-denomination coin-val))
        (cc;;using current coin-value
         (- amount (first-denomination coin-val))
         coin-val)))))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(cc 100 us-coins)
|#


'(exercise 2 20)#|
;; experiment with dotted-tail notation
(define (f x y . z) z)
;(f 1)
(f 1 2);()
(f 1 2 3);(3)
(f 1 2 3 4);(3 4)
(f 1 2 3 4 5);(3 4 5)
(newline)

(define (g . x) x)
(g);()
(g 1 2 3);(1 2 3)
(newline)

(define h (lambda x x))
(h);()
(h 1);(1)
(h 1 2);(1 2)
(h 1 2 3);(1 2 3)
(newline)

(define i (lambda (x) x))
(i '())
(i '(1))
(i '(1 2))
(i '(1 2 3))
(newline)

;; what is the difference between (lambda x x) and (lambda (x) x)
(define j1 (lambda x x))
(define j2 (lambda (x) x))
(define j3 (lambda (x y) (cons x (cons y '()))))
(define j4 (lambda (x . y) (cons x y)))

(j1 1 2 3);(1 2 3)
(j1 '(1) '(2 3));((1) (2 3))
;(j2 '(1) '(2 3));arity mismatch
(j3 '(1) '(2 3));((1) (2 3)) ;; can only accept fixed number of params
(j4 1 2 3);(1 2 3) can accept variable number of vars but not null

(define (same-parity head . rest)
  (define (recur filter l)
    (cond ((null? l) '())
          ((filter (car l))(cons (car l)(recur filter (cdr l))))
          (else (recur filter (cdr l)))))
  (cons head (recur (if (even? head) even? odd?) rest)))

(same-parity 0 1 4 0 5 6 8 7 1 0 6 7 8 4 3 0 8 6 2 0 3 4 6 1)
(same-parity 3 1 4 0 5 6 8 7 1 0 6 7 8 4 3 0 8 6 2 0 3 4 6 1)
|#


(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

;(scale-list '(1 2 3 4 5) 10)


;; map is builtin in DrRacket R5RS
;; add exclamation mark to overwrite
(define (map! proc items)
  (if (null? items)
      '()
      (cons (proc (car items))(map! proc (cdr items)))))

;(map! abs '(-10 2.5 -11.6 17))
;(10 2.5 11.6 17)
;(map! (lambda (x)(* x x)) '(1 2 3 4))
;(1 4 9 16)

;builtin map is more powerful
#|
(map + '(1 4 7) '(20 50 80) '(300 600 900))
;(321 654 987)
(map (lambda (x y)(+ x y y)) '(1 2 3) '(100 200 300))
;(201 402 603)
(map (lambda x x) '(1 4 7) '(20 50 80) '(300 600 900))
;((1 20 300) (4 50 600) (7 80 900))
|#

(define (scale-list items factor)
  (map (lambda (x)(* factor x)) items))

;(scale-list '(1 2 3 4 5) 10)
;(10 20 30 40 50)

(define (square x)(expt x 2))

'(exercise 2 21)#|
(define (square-list items)
  (if (null? items)
      '()
      (cons (expt (car items) 2)(square-list (cdr items)))))
(square-list '(1 2 3 4))

(define (square-list items)
  (map square items))
(square-list '(1 2 3 4))
|#


'(exercise 2 22)#|
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items '()))
(square-list '(1 2 3 4))
;(16 9 4 1)
;doesn't work bcz it car was consed first

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items '()))
(square-list '(1 2 3 4))
;((((() . 1) . 4) . 9) . 16)
;doesn't work bcz it cons list into atom

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things);use append instead of cons
              (append answer (cons (square (car things)) '())))))
  (iter items '()))
(square-list '(1 2 3 4))
;(1 4 9 16)
;now it works
|#


'(exercise 2 23)#|
(for-each (lambda (x)(newline)(display x)) '(1 2 3 4))

(define (for-each! proc items)
  (proc (car items))
  (if (null? (cdr items))
      (newline)
      (for-each! proc (cdr items))))

(for-each! (lambda (x)(newline)(display x)) '(1 2 3 4))
|#


;(cons '(1 2) '(3 4))
;((1 2) 3 4)

(define x (cons '(1 2) '(3 4)))
;(length x)
;3

(define y (list x x))
;y
;(((1 2) 3 4) ((1 2) 3 4))
;(length (list x x))
;2

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;(count-leaves x)
;4
;(count-leaves y)
;8



'(exercise 2 24)#|
(list 1 (list 2 (list 3 4)))
'(1 (2 (3 4)))

;; tree structure
(1 (2 (3 4)))
   / \
  1  (2 (3 4))
     / \
    2  (3 4)
       / \
      3   4

;; box-and-pointer structure
[1 *]––[* /]
        !
       [2 *]––[* /]
               !
              [3 *]––[4 /]
|#


'(exercise 2 25)#|
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))

(car (car '((7))))

;(define (cadr x)(car (cdr x))) ;if not exist

(cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))
|#

'(exercise 2 26)#|

(define x '(1 2 3))
(define y '(4 5 6))

(append x y)
;(1 2 3 4 5 6)
(cons x y)
;((1 2 3) 4 5 6)
(list x y)
;((1 2 3) (4 5 6))
|#

'(exercise 2 27)#|
(define x '((1 2) (3 4)))

(define (deep-reverse x)
  (cond
    ((null? x) '())
    ((pair? (car x)) (append (deep-reverse (cdr x))
                             ;(cons (deep-reverse (car x)) '())))
                             (list (deep-reverse (car x)))))
    (else (append (deep-reverse (cdr x))
                  (cons (car x) '())))))
(deep-reverse x);((4 3) (2 1))

;; much more clever one! 
(define (deep-reverse x)
  (if (pair? x)
      (append (deep-reverse (cdr x))(list (deep-reverse (car x))))
      x))
(deep-reverse x);((4 3) (2 1))

;; another clever one that uses reverse and map
(define (deep-reverse x)
  (if (pair? x)
      (reverse (map deep-reverse x))
      x))
(deep-reverse x);((4 3) (2 1))

;; do it without using append, not my work
(define (deep-reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (if (pair? (car items))
            (let ((x (iter (car items) '())))
              (iter (cdr items) (cons x result)))
            (iter (cdr items) (cons (car items) result))))) 
  (iter items '()))
(deep-reverse x);((4 3) (2 1))

;; simplify the above
;; iterative
(define (deep-reverse items)
  (define (iter items result)
    (cond ((null? items) result)
          ((pair? (car items))(iter (cdr items)
                                    (cons (iter (car items) '()) result)))
          (else (iter (cdr items) (cons (car items) result)))))
  (iter items '()))
(deep-reverse x);((4 3) (2 1))

;; refactor further 
(define (deep-reverse items)
  (define (iter L R)
    (cond ((null? L) R)
          ((not (pair? (car L))) (iter (cdr L) (cons (car L) R)))
          (else (iter (cdr L) (cons (iter (car L) '()) R)))))
  (iter items '()))
(deep-reverse x);((4 3) (2 1))

(newline)
(reverse x);((3 4) (1 2))
(deep-reverse x);((4 3) (2 1))
(deep-reverse '(1 (2 3 (4 5) 6) 7 (8 (9 0))))

;(1 (2 3 (4 5) 6) 7 (8 (9 0)))
;   (2 3 (4 5) 6)   (8 (9 0))
;        (4 5)         (9 0)

;(((0 9) 8) 7 (6 (5 4) 3 2) 1)
; ((0 9) 8)   (6 (5 4) 3 2)
;  (0 9)         (5 4)
|#


'(exercise 2 28)#|
(define x '((1 2) (3 4)))
;recursive
(define (fringe x)
  (cond
    ((null? x) '())
    ((pair? (car x)) (append (fringe (car x))(fringe (cdr x))))
    (else (cons (car x)(fringe (cdr x))))))

(fringe x);(1 2 3 4)
(fringe (list x x));(1 2 3 4 1 2 3 4)
(fringe '(() 1 2));(() 1 2) ;;found an insidious bug

;recursive ;debug the above
(define (fringe x)
  (cond
    ((null? x) '())
    ((pair? (car x)) (append (fringe (car x))(fringe (cdr x))))
    ((null? (car x)) (fringe (cdr x)))
    (else (cons (car x)(fringe (cdr x))))))
(newline)
(fringe x);(1 2 3 4)
(fringe (list x x));(1 2 3 4 1 2 3 4)
(fringe '(() 1 2)); (1 2)

;recursive in a cleaner way
(define (fringe x)
  (cond
    ((null? x) '())
    ((not (pair? x)) (list x))
    (else (append (fringe (car x)) (fringe (cdr x))))))
(newline)
(fringe x);(1 2 3 4)
(fringe (list x x));(1 2 3 4 1 2 3 4)
(fringe '(() 1 2)); (1 2)

;recursive with helper function append-items-in-list
(define (append-items-in-list x)
  (cond ((null? x) '())
        (else (append (car x)(append-items-in-list (cdr x))))))
(append-items-in-list '((9 8) (7 6 5) () (4 3 2 1)));(9 8 7 6 5 4 3 2 1)

(define (fringe x)
  (cond
    ((null? x) '())
    ((not (pair? x))  (list x))
    (else (append-items-in-list (map fringe x)))))
(newline)
(fringe x);(1 2 3 4)
(fringe (list x x));(1 2 3 4 1 2 3 4)
(fringe '(() 1 2)); (1 2)

;iterative
(define (fringe tree) 
  (define (build x result) 
    (cond ((null? x) result) 
          ((not (pair? x)) (cons x result)) 
          (else (build (car x)  
                       (build (cdr x) result))))) 
  (build tree '())) 
(newline)
(fringe x);(1 2 3 4)
(fringe (list x x));(1 2 3 4 1 2 3 4)
(fringe '(() 1 2));(1 2)
|#

'(exercise 2 29)#|
;constructor
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))
;structure <= {mobile, weight}
;; think of mobile as hook
;; think of branch as horizontal bar

;selector
(define L-branch car)

(define R-branch cadr)

(define br-length car)

(define br-struct cadr)

;; abreviate contructor to make it cleaner
(define M make-mobile)
(define B make-branch)

;; the rules are:
;; (M)=>(B B)
;; (B)=>(L W)
;; (B)=>(L M)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                  m1
;                  !
;        +-----4---*-----6-----------+
;        !                           !
;       60                   +----3--*--3----+
;                            !               !
;                           20               20
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define m1 
  (M 
   (B 4 60) (B 6 
               (M
                (B 3 20)(B 3 20)))))
(define m2
  (M 
   (B 4 90) (B 6 
               (M
                (B 3 20)(B 3 20)))))

m1; is balanced
;((4 60) (6 ((3 20) (3 20))))
m2; is unbalanced
;((4 90) (6 ((3 20) (3 20))))


(define (total-weight mobile)
  (define (branch-weight branch)
    (if (number? (br-struct branch));br-struct can be weight or mobile
        (br-struct branch);if not number, then must be mobile
        (total-weight (br-struct branch))))
  (+ (branch-weight (L-branch mobile))
     (branch-weight (R-branch mobile))))


(total-weight m1)
;100
(total-weight m2)
;130
(total-weight (M (B 3 20)(B 3 20)))
;40

(define (balanced? mobile)
  (define (torque branch)
    (* (br-length branch) (if (number? (br-struct branch))
                              (br-struct branch)
                              (total-weight (br-struct branch)))))
  (= (torque (L-branch mobile))
     (torque (R-branch mobile))))

(balanced? m1)
;#t
(balanced? m2)
;#f



;suppose we change the representation of mobiles
;so that the constructors uses "CONS" istdof "LIST"
(define (make-mobile left right)(cons left right))
(define (make-branch length structure)(cons length structure))

;abbreviated as
(define M make-mobile)
(define B make-branch)

;we only need to change the selector
(define L-branch car)
(define R-branch cdr)
(define br-length car)
(define br-struct cdr)

;recreate m1 m2 
(define m1 (M (B 4 60) (B 6 (M (B 3 20)(B 3 20)))))
(define m2 (M (B 4 90) (B 6 (M (B 3 20)(B 3 20)))))


;;total-weight and balanced? still work
(total-weight m1)
;100
(total-weight m2)
;130
(total-weight (M (B 3 20)(B 3 20)))
;40
(balanced? m1)
;#t
(balanced? m2)
;#f
|#




(define (scale-tree tree factor)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (* tree factor))
    (else (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))))

;(scale-tree '(1 (2 (3 4) 5) (6 7)) 10)
;(10 (20 (30 40) 50) (60 70))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;(scale-tree '(1 (2 (3 4) 5) (6 7)) 10)
;(10 (20 (30 40) 50) (60 70))
;(scale-tree '(1 2 3 ()));arity mismatch
;extra parenthesis will break it but we ignore it for now


'(exercise 2 30)#|
(define (square x)(expt x 2))
;directly
;without using higher-order procedure
(define (square-tree tree)
  (cond
    ((null? tree) '())
    ((pair? (car tree)) (cons (square-tree (car tree))
                              (square-tree (cdr tree))))
    (else (cons (square (car tree))
                (square-tree (cdr tree))))))
(square-tree '(1 (2 (3 4) 5) (6 7)));(1 (4 (9 16) 25) (36 49))
;(square-tree '(1 (2 (3 4) 5) (6 7) ()))

;follow style of scale-tree
(define (square-tree tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (square tree))
    (else (cons (square-tree (car tree))
                (square-tree (cdr tree))))))
(square-tree '(1 (2 (3 4) 5) (6 7)));(1 (4 (9 16) 25) (36 49))
(square-tree '(1 (2 (3 4) 5) (6 7) ())); breaks


;using map and recursion
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
(square-tree '(1 (2 (3 4) 5) (6 7)));(1 (4 (9 16) 25) (36 49))
;(square-tree '(1 (2 (3 4) 5) (6 7) ())); breaks
|#


'(exercise 2 31)#|
;directly
;without using higher-order procedure
(define (tree-map f tree)
  (cond
    ((null? tree) '())
    ((pair? (car tree)) (cons (tree-map f (car tree))
                              (tree-map f (cdr tree))))
    (else (cons (f (car tree))
                (tree-map f (cdr tree))))))
;(square-tree '(1 (2 (3 4) 5) (6 7)));(1 (4 (9 16) 25) (36 49))

;another
(define (tree-map f tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (f tree))
    (else (cons (tree-map f (car tree))
                (tree-map f (cdr tree))))))

(define (square-tree tree) (tree-map square tree))
(square-tree '(1 (2 (3 4) 5) (6 7)));(1 (4 (9 16) 25) (36 49))


;rewrite the above
(define (tree-map f tree) 
  (cond ((null? tree) '()) 
        ((pair? tree)  
         (cons  
          (tree-map f (car tree))  
          (tree-map f (cdr tree)))) 
        (else (f tree)))) 

(define (square-tree tree) (tree-map square tree))
(square-tree '(1 (2 (3 4) 5) (6 7)));(1 (4 (9 16) 25) (36 49))

;using map nad recursion
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))
(square-tree '(1 (2 (3 4) 5) (6 7)))
;(1 (4 (9 16) 25) (36 49))
|#


'(exercise 2 32)#|
(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x)) rest)))))

;; Intuition!
;the result of ...
(subsets '(1 2 3)); ... is the union of ...
;(1) (subsets (2 3)) and ...
;(2) cons 1 into every (subsets (2 3))

;; Example!
(define S subsets)
(S '());      [1] =>  (nil)
(S '(3));     [2] =>  [1] + (cons 3 [1])  =>  (nil (3))
(S '(2 3));   [3] =>  [2] + (cons 2 [2])  =>  (nil (3) (2) (2 3))
(S '(1 2 3)); [4] =>  [3] + (cons 1 [3])  =>  final output
|#

(define (sum-odd-squares tree)
  (cond
    ((null? tree) 0)
    ((not (pair? tree))
     (if (odd? tree) (square tree) 0))
    (else (+ (sum-odd-squares (car tree))
             (sum-odd-squares (cdr tree))))))
;signal-flow
;enumerate: tree-leaves
;filter: odd?
;map: square
;accumulate: + 0

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))
;(fib 0)(fib 1)(fib 2)(fib 3)(fib 4)(fib 5)(fib 6)

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
;signal-flow
;enumerate: integers
;map: fib
;filter: even?
;accumulate: cons ()

(define (filter predicate sequence)
  (cond
    ((null? sequence) '())
    ((predicate (car sequence))
     (cons (car sequence)
           (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

;(filter odd? '(1 2 3 4 5))
;(1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;(accumulate + 0 '(1 2 3 4 5))
;15
;(accumulate * 1 '(1 2 3 4 5))
;120
;(accumulate cons '() '(1 2 3 4 5))
;(1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))))

;(enumerate-interval 2 7)
;(2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))))

;(enumerate-tree '(1 ((2 ()) 3) 4 ((5))))
;(1 2 3 4 5)

(define (sum-odd-square tree)
  (accumulate +
              0
              (map square 
                   (filter odd? (enumerate-tree tree)))))

;(sum-odd-square '(1 ((2 ()) 3) 4 ((5))))
;35 = 1+9+25

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib (enumerate-interval 0 n)))))

;(even-fibs 10)
;(0 2 8 34)


;construct a list of square of first n+1 Fibonacci numbers
(define (list-fib-squares n)
  (accumulate cons 
              '()
              (map square
                   (map fib (enumerate-interval 0 n)))))
;(list-fib-squares 10)
;(0 1 1 4 9 25 64 169 441 1156 3025)


;computing product of squares of add integers in sequence
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))
;(product-of-squares-of-odd-elements '(1 2 3 4 5))
;225



(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

'(exercise 2 33)#|
;define map append length in terms of accumulate
;original definition of accumulte
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map! p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))
(map! square '(1 2 3))
;(1 4 9)

(define (append! seq1 seq2)
  (accumulate cons seq2 seq1))

(append! '(1 2 3) '(4 5 6))
;(1 2 3 4 5 6)

(define (length! sequence)
  (accumulate (lambda (x y)(+ 1 y))
              0
              sequence))
(length! '(1 2 3 4))
;4
|#


'(exercise 2 34)#|
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficients)
  (accumulate (lambda (this-coeff higher-terms)
                 (+ this-coeff (* x higher-terms)))
               0
               coefficients))

(horner-eval 2 '(1 3 0 5 0 1))
;79

;1x^5 + 0x^4 + 5x^3 + 0x^2 + 3x + 1
;(((((1*x+0)*x +5)*x +0)*x +3)*x + 1)
|#


'(exercise 2 35)#|
(define t1 '(1 2 (3 4) (5 (6 7))))
(define t2 '(1 2 (3 4) (5 (6 7 ()))))

(define (count-leaves t)
  (accumulate + 0 
              (map (lambda (x) (if (pair? x)
                                   (count-leaves x) 
                                   1))
                   t)))
(newline)
(count-leaves t1);7
(count-leaves t2);8 because '() will count as 1

;; try to fix this bug
(define (count-leaves t)
  (accumulate + 0
              (map (lambda (x) (cond ((null? x) 0)
                                     ((pair? x) (count-leaves x))
                                     (else 1)))
                   t)))
(newline)
(count-leaves t1);7
(count-leaves t2);7 bug fixed

;; or making use of enumerate-tree
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree t))))

(newline)
(enumerate-tree t1);(1 2 3 4 5 6 7)
(count-leaves t1);7
(count-leaves t2);7 works for enumerate-tree
|#


'(exercise 2 36)#|
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
s
(accumulate-n + 0 s)
;(22 26 30)
|#


'(exercise 2 37)#| 
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product '(1 2 3) '(1 2 3))
;14


(define (matrix-*-vector m v);enumrate row of m and take product wit v
  (map (lambda (x) (dot-product v x)) m))
(matrix-*-vector '((1 2 3) (4 5 6)) '(1 0 -1))
;(-2 -2)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (transpose m)
  (accumulate-n cons '() m))

(define m1 '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(transpose m1)
;((1 4 6) (2 5 7) (3 6 8) (4 6 9))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (matrix-*-vector cols m-row))
         m)))

(matrix-*-matrix '((1 0) (0 1)) '((1 0) (0 1)))
(matrix-*-matrix '((1 2) (2 4)) '((1 2) (2 4)))
(matrix-*-matrix '((1 2 3 4) (5 6 7 8) (9 10 11 12))
                 '((1 2) (1 2) (1 2) (1 2)))
;((1 0) (0 1))
;((5 10) (10 20))
;((10 20) (26 52) (42 84))
|#

'(exercise 2 38)#| 
;; fold-right recur
(define (fold-right op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (fold-right op init (cdr seq)))))
(fold-right list '() '(1 2 3));(1 (2 (3 ())))

;; fold-right iter
(define (fold-right op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (op (car rest)
            (iter result (cdr rest)))))
  (iter init seq))
(fold-right list '() '(1 2 3));(1 (2 (3 ())))

;; fold-left recur
(define (fold-left op init seq)
  (if (null? seq)
      init
      (op (fold-left op init (cdr seq))
          (car seq))))
(fold-left list '() '(1 2 3));(((() 3) 2) 1)
; wrong

;; fold-left recur
(define (fold-left op init seq)
  (let ((rev-seq (reverse seq)))
  (if (null? seq)
      init
      (op (fold-left op init (reverse (cdr rev-seq)))
          (car rev-seq)))))
(fold-left list '() '(1 2 3));(((() 1) 2) 3)
; works though ugly

;; fold-left iter
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))
(fold-left list '() '(1 2 3));(((() 1) 2) 3)


(newline)
(display "other examples")(newline)
(fold-right + 0 '(1 2 3 4 5));15
(fold-right * 1 '(1 2 3 4 5));120
(fold-right cons '() '(1 2 3 4 5));(1 2 3 4 5)


(fold-right / 1 '(1 2 3))
;(/ 1 (/ 2 (/ 3 1)))
(fold-left / 1 '(1 2 3))
;(/ (/ (/ 1 1) 2) 3)

(fold-right list '() '(1 2 3));(1 (2 (3 ())))
(fold-left list '() '(1 2 3));(((() 1) 2) 3)


;; the op must be associative
; e.g.
;(1 + 2)+ 3 = 6
; 1 +(2 + 3) = 6
|#


'(exercise 2 39)#|
;; define reverse intermsof fold-right and fold-left

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)(fold-right op initial (cdr sequence)))))

;;iterative
(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (op (car rest)
            (iter result (cdr rest)))))
  (iter initial sequence))

(define (reverse! sequence)
  (fold-left (lambda (x y)(cons y x)) '() sequence))
(reverse! '(1 2 3 4))

(define (reverse! sequence)
  (fold-right (lambda (x y)(append y (list x))) '() sequence))
(reverse! '(1 2 3 4))
|#



(define (create-group n)
  (map (lambda (i)
         (map (lambda (j)(list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))
;(create-group 5)
#|
(()
 ((2 1))
 ((3 1) (3 2))
 ((4 1) (4 2) (4 3))
 ((5 1) (5 2) (5 3) (5 4)))
|#

(define (create-pair n)
  (accumulate append
              '()
              (map (lambda (i)
                     (map (lambda (j)(list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
;(create-pair 5)
#|
((2 1)
 (3 1)
 (3 2)
 (4 1)
 (4 2)
 (4 3)
 (5 1)
 (5 2)
 (5 3)
 (5 4))
|#

(define (flatmap proc seq)
  (accumulate append '() (map proc seq))) 
;(map square '(1 2 3 4))
;(1 4 9 16)
;(flatmap (lambda (x)(cons x (list x))) '(1 2 3 4))
;(flatmap (lambda (x)(list x x)) '(1 2 3 4))
;(1 1 2 2 3 3 4 4)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (create-pair n)
  (flatmap (lambda (i)
             (map (lambda (j)(list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
;(create-pair 5)
#|
((2 1)
 (3 1)
 (3 2)
 (4 1)
 (4 2)
 (4 3)
 (5 1)
 (5 2)
 (5 3)
 (5 4))
|#

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (find-divisor n 2))

;(smallest-divisor 49)

(define (prime-sum? pair)
  (define (prime? n)
    (= n (smallest-divisor n)))
  (prime? (+ (car pair)(cadr pair))))

;(prime-sum? '(3 4))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair)(cadr pair))))
;(make-pair-sum '(1 2))
;(1 2 3)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j)
                         (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

;(prime-sum-pairs 5)
;((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))

;; define remove
(define (remove x s)
  (cond
    ((null? s) '())
    ((eq? x (car s))(cdr s))
    (else (cons (car s)(remove x (cdr s))))))
;(remove 2 '(1 2 3))
;(remove 4 '(1 2 3));when target not in the list

;; or use filter
(define (remove x s)
  (filter (lambda (item)(not (= x item))) s))
;(remove 2 '(1 2 3))
;(remove 4 '(1 2 3));when target not in the list

(define (permutations s)
  (if (null? s)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (p)(cons x p))
                      (permutations (remove x s))))
               s)))

;(permutations '(1 2 3))
;((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))


'(exercise 2 40)#||#
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)(list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
;(unique-pairs 4)
;((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))

(define (prime-sum-pairs n)
   (map make-pair-sum
        (filter prime-sum? (unique-pairs n))))

;(prime-sum-pairs 5)
;((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))



'(exercise 2 41)#||#
(define (enumerate-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;(enumerate-triples 5)

(define (triples-with-fixed-sum sum max)
  (filter (lambda (list) (= sum (accumulate + 0 list)))
          (enumerate-triples max)))

;(triples-with-fixed-sum 10 5)
;((5 3 2) (5 4 1))



'(exercise 2 42)#|
;; not mine
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)(safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)(safe? k positions))
         (flatmap (lambda (rest-of-queens)
                    (map (lambda (new-row)
                           (adjoin-position new-row k rest-of-queens))
                         (enumerate-interval 1 board-size)))
                  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

;; constructor
(define (place-queen rank file)
  (list rank file))
;simplify
(define place-queen list)

;; selectors
(define (queen-rank queen)
  (car queen))

(define (queen-file queen)
  (cadr queen))

(define (adjoin-position rank file board)
  (cons (place-queen rank file) board))

(define (find-first pred items)
  (cond ((null? items) '())
        ((pred (car items)) (car items))
        (else (find-first pred (cdr items)))))

(define (safe? file board)
  (define (get-queen-by-file file board)
    (find-first (lambda (queen)(= (queen-file queen) file))
                board))
  (let ((the-queen (get-queen-by-file file board)))
    (let ((other-queens (filter (lambda (q)
                                  (not (and (= (queen-rank the-queen)
                                               (queen-rank q))
                                            (= (queen-file the-queen)
                                               (queen-file q)))))
                                board)))
      (and (not (accumulate (lambda (p q)
                              (or q
                                  (= (queen-rank p)
                                     (queen-rank the-queen))))
                            #f
                            other-queens))
           (not (accumulate (lambda (p q)
                              (or q
                                  (= (abs (- (queen-rank the-queen)
                                             (queen-rank p)))
                                     (abs (- (queen-file the-queen)
                                             (queen-file p))))))
                            #f
                            other-queens))))))

(queens 4)
;(((3 4) (1 3) (4 2) (2 1))
; ((2 4) (4 3) (1 2) (3 1)))

;; another one
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)(safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
    (iter-check (car positions) 
                (cdr positions)
                 1))

(define (iter-check row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens)
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
            (if (or (= row-of-new-queen row-of-current-queen)
                    (= row-of-new-queen (+ i row-of-current-queen))
                    (= row-of-new-queen (- row-of-current-queen i)))
                #f
                (iter-check row-of-new-queen
                            (cdr rest-of-queens)
                            (+ i 1))))))   

(queens 8)
|#


'(exercise 2 43)#|
;; the original program recurs on queen-cols and then enum a new row

;; however louis' program enumerates the new row and it computes
;; queen-cols over and over again, which leads to considerable waste

;; because queen-cols is the most computationally intensive part
;; we just simplify the runtime analysis by focusing on recursive
;; queen-cols calls

;; suppose board-size = N

;; for the original program, queen-cols is called N+1 times, O(N)

;; for Louis' program, queen-cols is called 1+N + N^2 +...+ N^N times
;; = (N^(N+1) - 1)/(N - 1) ~= O(N^N)

|#




'(exercise 2 44)#|

(define wave2 (beside wave (flip-vert wave)))

(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))



(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (besides (below painter top-left)
                   (below (bottom-right corner)))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter)(tr painter)))
          (bottom (beside (bl painter)(br painter))))
      (below bottm top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))


(define flipped-pairs
  (square-of-four identity flip-vert identity flip-vert))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
|#


'(exercise 2 45)#|

(define right-split (split beside below))

(define up-split (split below beside))

(define (split f g)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split f g) painter (- n 1))))
          (f painter (g smaller smaller))))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))

((frame-coord-map a-frame) (make-vect 0 0))

(origin-frame frame)

|#

'(exercise 2 46)#|

(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)(xcor-vect v2))
             (+ (ycor-vect v1)(ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)(xcor-vect v2))
             (- (ycor-vect v1)(ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

|#
'(exercise 2 47)#|

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define frame-origin car)
(define frame-edge1 cadr)
(define frame-edge2 caddr)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define frame-origin car)
(define frame-edge1 cadr)
(define frame-edge2 cddr)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)(start-segment segment))
        ((frame-coord-map frame)(end-segment segment))))
     segment-list)))
|#

'(exercise 2 48)#|

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)

|#
'(exercise 2 49)#|

(define (outline-painter frame)
  (let ((o (frame-origin frame))
        (e1 (add-vect (frame-origin frame)(frame-edge1 frame)))
        (e2 (add-vect (frame-origin frame)(frame-edge2 frame)))
        (e12 (add-vect e1 (frame-edge2 frame))))
    (let ((br (make-segment o e1))
          (bl (make-segment o e2))
          (tr (make-segment e1 e2))
          (tl (make-segment e2 e12)))
      ((segment->painter (list br bl tr tl)) frame))))

(define (xross-painter frame)
  (let ((o (frame-origin frame))
        (e1 (add-vect (frame-origin frame)(frame-edge1 frame)))
        (e2 (add-vect (frame-origin frame)(frame-edge2 frame)))
        (e12 (add-vect e1 (frame-edge2 frame))))
    (let ((vertic (make-segment o e2))
          (horizt (make-segment e1 e2)))
      ((segment->painter (list vertic horizt)) frame))))

(define (mid-painter frame)
  (define (mid v1 v2)
    (make-vect (/ (+ (xcor-vect v1) (xcor-vect v2)) 2.0)
               (/ (+ (ycor-vect v1) (ycor-vect v2)) 2.0)))
  (let ((o (frame-origin frame))
        (e1 (add-vect (frame-origin frame)(frame-edge1 frame)))
        (e2 (add-vect (frame-origin frame)(frame-edge2 frame)))
        (e12 (add-vect e1 (frame-edge2 frame))))
    (let ((br-point (mid o e1))
          (bl-point (mid o e2))
          (tr-point (mid e1 e12))
          (tl-point (mid e2 e12)))
      (let ((bottom
             (make-segment bl-point br-point))
            (left
             (make-segment bl-point tl-point))
            (right
             (make-segment br-point tr-point))
            (top
             (make-segment tl-point tr-point)))
        ((segment->painter (list bottom left right top)) frame)))))

(define (wave-painter frame)((segment-painter '(? ? ? ?)) frame))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (framecoord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

|#
'(exercise 2 50)#|

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

|#
'(exercise 2 51)#|

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-above
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; either
(define (below1 painter1 painter2) 
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2)))) 
;; or
(define (below2 painter1 painter2) 
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1)))) 

|#
'(exercise 2 52)#|
skip
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3 Symbolic Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define a 1)
(define b 2)

;(list a b)
;(1 2)

;(list 'a 'b)
;(a b)

;(list 'a b)
;(a 2)

;(car '(a b c))
;a

;(cdr '(a b c))
;(b c)


(define (memq! item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;(memq 'apple '(pear banana prune))
;#f
;(memq 'apple '(x (apple sauce) y apple pear))
;(apple pear)


'(exercise 2 53)#|
(list 'a 'b 'c)
;(a b c)
(list (list 'george))
;((george))
(cdr '((x1 x2) (y1 y2)))
;((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;(y1 y2)
(pair? (car '(a short list)))
;#f
(memq 'red '((red shoes) (blue socks)))
;#f
(memq 'red '(red shoes blue socks))
;(red shoes blue socks)
|#


'(exercise 2 54)#|
;;equal? is builtin
;;overwrite with !
(define (equal?! l1 l2)
  (cond
    ((and (not (pair? l1))(not (pair? l2))) (eq? l1 l2));null or atom
    ((and (pair? l1)(pair? l2)) (and (equal? (car l1)(car l2))
                                     (equal? (cdr l1) (cdr l2)))) 
    (else #f)));false if different types

(equal?! '(this is a list) '(this is a list))
(equal?! '(this is a list) '(this (is a) list))
|#

'(exercise 2 55)#|
(car ''abracadabra)
;; is the same thing as
(car (quote (quote abracadabra)))
|#

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) var))
                         (make-product (deriv (multiplier exp) var)
                                       (multiplicand exp))))
        (else (display "unknown expression type"))))



(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)(variable? v2))(eq? v1 v2))

(define (make-sum a b) (list '+ a b))

(define (make-product a b) (list '* a b))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)(cadr p))

(define (multiplicand p)(caddr p))

;; Testing

;(deriv '(+ x 3) 'x)
;(+ 1 0)
;(deriv '(* x y) 'x)
;(+ (* x 0) (* 1 y))
;(deriv '(* (* x y)(+ x 3)) 'x)
;(+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))
;; producing result that are correct but complex

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a1 0) a1)
        ((and (number? a1)(number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp)(= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)(=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1)(number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; Te-sting

;(deriv '(+ x 3) 'x)
;1
;(deriv '(* x y) 'x)
;y
;(deriv '(* (* x y)(+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))


'(exercise 2 56)
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) var))
                         (make-product (deriv (multiplier exp) var)
                                       (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp)
                                (if (number? (exponent exp))
                                    (- (exponent exp) 1)
                                    ;;or define substraction outside
                                    (list '- (exponent exp) 1)))
           (deriv  (base exp) var))))
        (else (display "unknown expression type"))))



(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))


;;Testing

;(make-exponentiation 2 5)
;(** 2 5)
;(deriv (make-exponentiation 'x 5) 'x)
;(* 5 (** x 4))
;(deriv (make-exponentiation 'x 'y) 'x)
;(* y (** x (- y 1)))


'(exercise 2 57)
(define (make-sum-list l)
  (if (= (length l) 2)
      (list '+ (car l) (cadr l))
      (make-sum (car l) (make-sum-list (cdr l)))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (make-sum-list (list a1 a2)))))

(define (make-product-list l)
  (if (= (length l) 2)
      (list '* (car l) (cadr l))
      (make-product (car l) (make-product-list (cdr l)))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (make-product-list (list m1 m2)))))

(define (augend s)
  (let ((a (cddr s)))
    (if (= (length a) 1)
        (car a)
        (make-sum-list a))))

(define (multiplicand p)
  (let ((m (cddr p)))
    (if (= (length m) 1)
        (car m)
        (make-product-list m))))

;; Testing
;(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))
;(deriv '(* x y (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))



'(exercise 2 58)

;; a can be achieved by changing (+ a b) to (a + b)

;; b can be achieved by giving * higher precedence than +



(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))))

;(element-of-set? 1 '(4 3 2 1))
;#t

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;(adjoin-set 1 '(1 2 3))
;(1 2 3)
;(adjoin-set 1 '(2 3))
;(1 2 3)

(define (intersection-set s1 s2)
  (cond
    ((or (null? s1)(null? s2)) '())
    ((element-of-set? (car s1) s2)(cons (car s1)
                                        (intersection-set (cdr s1) s2)))
    (else (intersection-set (cdr s1) s2))))

;(intersection-set '(1 2 3 4) '(2 4 6))
;(2 4)



'(exercise 2 59)
;;iterative
(define (union-set s1 s2)
  (define (iter s1-left result)
    (cond 
      ((null? s1-left) result)
      ((element-of-set? (car s1-left) result) (iter (cdr s1-left) result))
      (else (iter (cdr s1-left) (cons (car s1-left) result)))))
  (iter s1 s2))
;(union-set '(1 2 3) '(2 3 4))

;;recursive
(define (union-set s1 s2)
  (cond ((null? s2) s1)
        ((element-of-set? (car s2) s1)
         (union-sets1 (cdr s2)))
        (else (cons (car s2) (union-set s1 (cdr s2))))))
;(union-set '(1 2 3) '(2 3 4))



'(exercise 2 60)#|
(define (adjoin-set x set)(cons x set))

(define (union-set set1 set2)(append set1 set2))

;adjoin and union-set operation is constant time (to be exact, O(1))
;the intersection operation in this verison is linear, which is 
;significantly lower than O(n^2) time 
|#


(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (car set)) #t)
    ((< x (car set)) #f)
    (else (element-of-set? x (cdr set)))))

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (let ((x1 (car s1))
            (x2 (car s2)))
        (cond
          ((= x1 x2) (cons x1 (intersection-set (cdr s1)(cdr s2))))
          ((< x1 x2) (intersection-set (cdr s1) s2))
          ((> x1 x2) (intersection-set s1 (cdr s2)))))))


'(exercise 2 61)
(define (adjoin-set x set) 
  (cond ((null? set) (list x)) 
        ((= x (car set)) set) 
        ((< x (car set)) (cons x set)) 
        (else (cons (car set) (adjoin-set x (cdr set)))))) 
;(adjoin-set 1 '(1 2 3))
;(adjoin-set 1 '(2 3))
;(1 2 3)


'(exercise 2 62)
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1))
               (x2 (car s2)))
           (cond
             ((= x1 x2) (cons x1 (union-set (cdr s1)(cdr s2))))
             ((< x1 x2) (cons x1 (union-set (cdr s1) s2)))
             ((> x1 x2) (cons x2 (union-set s1 (cdr s2)))))))))

;(union-set '(2 4 6 8) '(1 3 5 7 9))
;(1 2 3 4 5 6 7 8 9)


(define (entry tree)(car tree))

(define (left-branch tree)(cadr tree))

(define (right-branch tree)(caddr tree))

(define (make-tree entry left right)(list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry-set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry-set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


'(exercise 2 63)#|
(define (tree-list1 tree)
  (if (null? tree)
      '()
      (append (tree-list1 (left-branch tree))
              (cons (entry tree)
                    (tree-list1 (right-branch tree))))))

(define (tree-list2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result)))))
  (copy-to-list tree '()))


(define t1-7
  (make-tree 4
             (make-tree 2
                        (make-tree 1 '() '())
                        (make-tree 3 '() '()))
             (make-tree 6
                        (make-tree 5 '() '())
                        (make-tree 7 '() '()))))

;(tree-list1 t1-7)
;(1 2 3 4 5 6 7)
;(tree-list2 t1-7)

;for tree-list1, the append operation takes extrs cost of linear time
;T(n) = T(n/2) + O(n/2)
;T(n) = O(nlogn)

;for tree-list2, each recursion only need one cons operation
;T(n) = T(n/2) + O(1)
;T(n) = O(n)
|#

'(exercise 2 64)#|
(define (list-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree
                       this-entry left-tree right-tree)
                      remaining-elts))))))))

(list-tree '(1 3 5 7 9))
;(5 (1 () (3 () ())) (7 () (9 () ())))
;calling >>>

(car (partial-tree '(1 3 5 7 9) 5))
;((5 (1 () (3 () ())) (7 () (9 () ()))))
;left-size = 2
;calling >>>

;(partial-tree '(1 3 5 7 9) 2)
;((1 () (3 () ())) 5 7 9)
;calling >>>

(define Left-Result (car (partial-tree '(1 3 5 7 9) 2)))
Left-Result

(define Right-Result (car (partial-tree '(7 9) 2)))
Right-Result

(define Tree-Result (make-tree 5 Left-Result Right-Result))
Tree-Result
|#


'(exercise 2 65)#|
(define (union-set t1 t2) 
  (list-tree (union-set-list (tree-list t1) 
                             (tree-list t2)))) 

(define (intersection-set t1 t2) 
  (list-tree (intersection-set-list (tree-list t1) 
                                    (tree-list t2)))) 
|#


(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))


'(exercise 2 66)#|
(define (lookup target records)
  (cond
    ((null? records) #f)
    ((= target (key (entry records)))(entry records))
    ((< target (key (entry records)))
     (lookup target (left-branch records)))
    (else
     (lookup target (right-branch records)))))
|#


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)(symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond
    ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (display "bad bit"))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)  ;symbol
                               (cadr pair));frequency
                    (make-leaf-set (cdr pairs))))))


'(exercise 2 67)
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
sample-tree

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(decode sample-message sample-tree)
;(a d a b b c a)


'(exercise 2 68)#|
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
   (define (iter tree bits)
     (cond ((and (leaf? tree) (eq? symbol (symbol-leaf tree)))
            (reverse bits))
           ((memq symbol (symbols (left-branch tree)))
            (iter (left-branch tree) (cons 0 bits)))
           ((memq symbol (symbols (right-branch tree)))
            (iter (right-branch tree) (cons 1 bits)))
           (else (display "not found"))))
   (iter tree '()))
(encode '(a d a b b c a) sample-tree)
|#













