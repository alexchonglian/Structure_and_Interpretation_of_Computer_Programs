;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 1
;; Building Abstraction with Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1 The Elements of Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x)(* x x))
;(square 5);25

(define (sum-of-squares x y)
  (+ (square x) (square y)))
;(sum-of-squares 3 4);25

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
;(f 5);136


;; abs is built-in
;; create abs! instead
(define (abs! x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))))

(define (abs! x)
  (cond
    ((< x 0) (- x))
    (else x)))

(define (abs! x)
  (if (< x 0)(- x) x))


'(exercise 1 1)#|
10
12
8
3
6
;undef
;undef
19
#f
4
16
6
16
|#

'(exercise 1 2)#|
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
|#

'(exercise 1 3)#|
(define (sum-of-squares-of-2-larger x y z)
  (- (+ (square x)(square y)(square z))
     (square (min x y z))))

(sum-of-squares-of-2-larger 1 3 4)
(sum-of-squares-of-2-larger 4 1 3)
(sum-of-squares-of-2-larger 3 4 1)
|#

'(exercise 1 4)#|
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 10 -10)
; 20
; if b is positive then (+ a b)
; if b is negative then (- a b)
; the if clause returns a operator, cool ...
|#

'(exercise 1 5)#|
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

;The line below never terminate on DrRacket
;(test 0 (p))

;Applicative-order
;(test 0 (p)) 
;(test 0 (p)) 
;(test 0 (p)) 
;... never stop

;Normal-order
;(test 0 (p)) 
;(if (= 0 0) 0 (p)) 
;(if #t 0 (p)) 
;0 
|#


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;(sqrt-iter 1.0 2.0)
;(sqrt-iter 1.0 3.0)
;(sqrt-iter 1.0 9.0)

(define (sqrt! x)
  (sqrt-iter 1.0 x))

;(sqrt! 2.0)
;(sqrt! 3.0)
;(sqrt! 9.0)



'(exercise 1 6)#|
(define (new-if predicate then-clause else-clause)
  (cond
    (predicate then-clause)
    (else else-clause)))

(new-if (= 2 3) 0 5)
;5

(new-if (= 1 1) 0 5)
;0


;new-if uses applicative-order evaluation
;the else-clause is always evaluated and lead to infinite recursion

;the original if uses normal-order evaluation
;only one of the two clause get evaluated and no infinite problem

;to summarize, the difference between "new-if" and "if" is that
;when using if, only one of the two consequent exprs get evaluated
;when using new-if, both of the consequent exprs get evaluated
|#



'(exercise 1 7)#|
;; original
(sqrt! 900)
(sqrt! 9)
(sqrt! 0.09)
(sqrt! 0.0009)
;30.00000230911258
;3.00009155413138
;0.3000299673226795
;0.04030062264654547


;; 2nd modification
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) (* 0.001 guess)))

(define (sqrt! x)
  (sqrt-iter 1.0 x))
(sqrt! 900)
(sqrt! 9)
(sqrt! 0.09)
(sqrt! 0.0009)
;30.00000230911258
;3.00009155413138
;0.300009155413138
;0.030027476242325962


;; 3rd modification
(define (sqrt! x)
  (cond
    ((> x 1.0)(sqrt-iter 1.0 x))
    (else (sqrt-iter 0.5 x))))
(sqrt! 900)
(sqrt! 9)
(sqrt! 0.09)
(sqrt! 0.0009)
;30.00000230911258
;3.00009155413138
;0.300009155413138
;0.030027476242325962


;; 4th modification
(define (good-enough-rel? new-guess old-guess x)
  (< (abs (- new-guess old-guess))
     (* new-guess 0.001)))

(define (sqrt-iter new-guess old-guess x)
  (if (good-enough-rel? new-guess old-guess x)
      new-guess
      (sqrt-iter (improve new-guess x) new-guess x)))

(define (sqrt! x) 
   (sqrt-iter (improve 1.0 x) 1.0 x)) 
(sqrt! 900)
(sqrt! 9)
(sqrt! 0.09)
(sqrt! 0.0009)
;30.00000230911258
;3.000000001396984
;0.30000000149658457
;0.030000012746348552
|#


'(exercise 1 8)#|
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube a)(* a a a))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) (* guess 0.001)))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cube 3)
(cbrt 27)
;(cbrt -1000);doesn't work for negative
;will fix it later :(
|#


;; block structure
;; embedded definition
;; solving name-packaging problem
(define (sqrt! x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))
;(sqrt! 4)


;; allow x to be free variable
;; lexical scoping
(define (sqrt! x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
;(sqrt! 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2 Procedures and the Processes They Generate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
#|
(factorial -1)
(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
|#

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product) (+ 1 counter) max-count)))
#|
(factorial -1)
(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
|#

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ 1 counter))))
  (iter 1 1))
#|
(factorial -1)
(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
|#



'(exercise 1 9)#|
(define (inc x)(+ x 1))
(define (dec x)(- x 1))

;The process generated by the first procedure is recursive
(+ 4 5) 
(inc (+ (dec 4) 5)) 
(inc (+ 3 5)) 
(inc (inc (+ (dec 3) 5))) 
(inc (inc (+ 2 5))) 
(inc (inc (inc (+ (dec 2) 5)))) 
(inc (inc (inc (+ 1 5)))) 
(inc (inc (inc (inc (+ (dec 1) 5))))) 
(inc (inc (inc (inc (+ 0 5))))) 
(inc (inc (inc (inc 5)))) 
(inc (inc (inc 6))) 
(inc (inc 7)) 
(inc 8) 
9 
  
;The process generated by the second procedure is iterative
(+ 4 5) 
(+ (dec 4) (inc 5)) 
(+ 3 6) 
(+ (dec 3) (inc 6)) 
(+ 2 7) 
(+ (dec 2) (inc 7)) 
(+ 1 8) 
(+ (dec 1) (inc 8)) 
(+ 0 9) 
9 
|#


'(exercise 1 10)#|
(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1)(A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)

(define (f n)(A 0 n))
(define (g n)(A 1 n))
(define (h n)(A 2 n))
(define (k n)(* 5 n n))

;A(0, n) = f(n) = 2n

;A(1, n) = g(n) = A(0, (g(n-1))) = f(g(n-1))

;g(n) = f(g(n-1)) = 2g(n-1)

;g(1) = 2

;g(n) = 2^n

;h(n) = A(2, n) = A(1, A(2, n-1)) = g(h(n-1))

;h(n) = g(h(n-1)) = 2^h(n-1)

;h(1) = 2

;h(n) = 2^2^2^...^2 (n times)

(h 1) ;= 2
(h 2) ;= 2^h(1)
(h 3) ;= 2^h(2)
(h 4) ;= 2^h(3)
|#



(define (fib n)
  (cond ((< n 1) 0);prevent leakage of negative n
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
#|
(fib -1)
(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
|#
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (< count 1)
      b
      (fib-iter (+ a b) a (- count 1))))
#|
(fib -1)
(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
|#

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)(= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;(count-change 100);292
;(count-change 10);4


'(exercise 1 11)#|
;; recursive
(define (foo n)
  (cond
    ((< n 3) n)
    (else (+ (foo (- n 1))
             (* 2 (foo (- n 2)))
             (* 3 (foo (- n 3)))))))
(foo 0)
(foo 1)
(foo 2)
(foo 3)
(foo 4)
(foo 5)
(foo 6)
(foo 7)

;; iterative
(define (foo n)
  (define (foo-iter a b c count)
    (cond
      ((= count 0) a);you can save time by ((= count 2) c)
      (else (foo-iter b c 
                      (+ c (* 2 b) (* 3 a))
                      (- count 1)))))
  (foo-iter 0 1 2 n))

(foo 0)
(foo 1)
(foo 2)
(foo 3)
(foo 4)
(foo 5)
(foo 6)
(foo 7)
|#



'(exercise 1 12)#|
;(row0) 1
;(row1) 1 1
;(row2) 1 2 1
;(row3) 1 3 3 1
;(row4) 1 4 6 4 1
;(row5) 1 5 10105 1

(define (pascal row col)
  (cond
    ((or (< row 0)
         (< col 0)
         (< row col)) 0);return zero
    ((or (= row 0)
         (= row 1)
         (= col 0)
         (= col row)) 1);return one
    ((or (= col 1)
         (= col (- row 1))) row);return row
    (else (+ (pascal (- row 1) col)
             (pascal (- row 1) (- col 1))))))

(pascal 5 -1);violation: -1, return 0
(pascal 5 0)
(pascal 5 1)
(pascal 5 2)
(pascal 5 3)
(pascal 5 4)
(pascal 5 5)
(pascal 5 6);violation: 6>5, return 0
|#


'(exercise 1 13)
;please resort to other resources for the proof

'(exercise 1 14)#|

          (cc 11 5)
          /        \
      (cc 11 4)     (cc -39 5) = 0
        /      \
   (cc 11 3)    (cc -14 4) = 0
      /     \
 (cc 11 2)   (cc 1 3) - 1
    /     \
(cc 11 1)  (cc 6 2)
  /        /       \
 1     (cc 6 1)    (cc 1 2)
         /         /      \
        1       (cc 1 1)  (cc -4 2) = 0
                 /
                1
|#

'(exercise 1 15)#|
(define (cube x)(* x x x))
(define (p x)(- (* 3 x)(* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15); will be expanded as follows
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))
  
;thus procedure p is applied 5 times
;the space and #steps required are ceil[log3 (a/0.1)]
|#


(define (expt! b n)
  (if (= n 0)
      1
      (* b (expt! b (- n 1)))))

;(expt! 3 3);27
;O(n) steps and O(n) space

(define (expt! b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* product b))))
  (expt-iter b n 1))
;(expt! 3 3)
;O(n) steps and O(1) space

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n)(square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
;(fast-expt 2 11);2048
;O(logn) growth

'(exercise 1 16)#|
(define (fast-expt b n)
  (define (fast-iter a b n)
    (cond ((= n 0) a)
          ((even? n)(fast-iter a (square b) (/ n 2)))
          (else (fast-iter (* a b) b (- n 1)))))
  (fast-iter 1 b n))
  
(fast-expt 2 0)
(fast-expt 2 1)
(fast-expt 2 2)
(fast-expt 2 3)
(fast-expt 2 10)
(fast-expt 2 20)
|#

(define (double x)(* x 2))
(define (halve x)(/ x 2))

'(exercise 1 17)#|
(define (times a b)
  (cond
    ((or (= a 0)(= b 0)) 0)
    ((= b -1) (- a));for negative numbers
    ((= a -1) (- b));for negative numbers
    ((even? b)(double (times a (halve b))))
    (else (+ a (times a (- b 1))))))

(times 0 1)
(times 1 -1)
(times 0 0)
(times 1 2)
(times 4 5)
(times -4 -5)
(times -4 5)
(times -5 4)
|#


'(exercise 1 18)#|
(define (times a b)
  (define (times-iter a b product)
    (cond
      ((or (= a 0)(= b 0)) product)
      ((= b -1)(- product a));for negative numbers
      ((= a -1)(- product b));for negative numbers
      ((even? b)(times-iter (double a) (halve b) product))
      (else (times-iter a (- b 1) (+ a product)))))
  (times-iter a b 0))

(times 0 1)
(times 1 -1)
(times 0 0)
(times 1 2)
(times 4 5)
(times -4 -5)
(times -4 5)
(times -5 4)
|#

'(exercise 1 19)#|
;transformation of {a:=bq+aq+ap, b:=bp+aq} can be denoted
;as matrix form of:
;
;T(pq) = [1 1] = [p+q,  q]
;        [1 0]   [ q ,  p]
;
;in T(p'q') = T(pq)^2, the corresponding elements p' and q' are:
;
;p' = p^2 + q^2
;q' = 2pq + q^2

(define (fib n) 
  (define (fib-iter a b p q count) 
    (cond
      ((= count 0) b) 
      ((even? count)(fib-iter a 
                              b 
                              (+ (square p) (square q)) 
                              (+ (* 2 p q) (square q)) 
                              (/ count 2))) 
      (else (fib-iter (+ (* b q) (* a q) (* a p)) 
                      (+ (* b p) (* a q)) 
                      p 
                      q 
                      (- count 1)))))
  (fib-iter 1 0 0 1 n))

(fib 0) 
(fib 1) 
(fib 2) 
(fib 3) 
(fib 4) 
(fib 5) 
(fib 6) 
(fib 7) 
(fib 8) 
(fib 9) 
(fib 10) 
|#


(define (gcd! a b)
  (if (= b 0)
      a
      (gcd! b (remainder a b))))

;(gcd! 6 4)

'(exercise 1 20)#|
;for normal order evaluation
;14 remainder operations in if clause 
;4 during reduction
;in total 18 remainder operations
(gcd 206 40) 
(if (= 40 0) ...)
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0) ...)
(if (= 6 0) ...)
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0) ...)
(if (= 4 0) ...)
(gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
(if (= (r (r 206 40) (r 40 (r 206 40))) 0) ...)
(if (= 2 0) ...)
(gcd (r (r 206 40)(r 40 (r 206 40)))
     (r (r 40 (r 206 40))(r (r 206 40)(r 40 (r 206 40)))))
(if (= (r (r 40 (r 206 40))(r (r 206 40)(r 40 (r 206 40)))) 0) ...)
(if (= 0 0) ...)
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))


;for applicative order evaluation
;4 remainder operations.
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2
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

(define (prime? n)
  (= n (smallest-divisor n)))

;(prime? 131)

(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)(remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))

;(expmod 3 5 5)
;(expmod 3 7 7)
;(expmod 3 11 11)
;(expmod 3 13 13)
;(expmod 3 19 19)


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))));random not included!!

(define (fast-prime? n times)
  (cond
    ((= times 0) #t)
    ((fermat-test n)(fast-prime? n (- times 1)))
    (else #f)))


'(exercise 1 21)#|
(smallest-divisor 19)
;19
(smallest-divisor 1999)
;1999
(smallest-divisor 19999)
;7
|#

'(exercise 1 22)#|
;; use a scheme version that provides random and runtime
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes first last)
  (define (search-iter current last)
    (if (<= current last) (timed-prime-test current))
    (if (<= current last) (search-iter (+ current 2) last)))
  (search-iter (if (even? first) (+ first 1) first)
               (if (even? last) (- last 1) last)))
|#

'(exercise 1 23)#|
(define (smallest-divisor n)
  (define (find-divisor n test-div)
    (cond
      ((> (square test-div) n) n)
      ((divides? test-div n) test-div)
      (else (find-divisor n (next test-div)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (next n)
    (if (= n 2) 3 (+ 2 n)))
  (find-divisor n 2))

(smallest-divisor 1)
(smallest-divisor 2)
(smallest-divisor 3)
(smallest-divisor 19)
(smallest-divisor 247)
(smallest-divisor 131)
|#

'(exercise 1 24)

'(exercise 1 25)
;; correct

'(exercise 1 26)
;; (expmod base (/ exp 2) m) evaluated twice
;; the running time grows linearly with N

'(exercise 1 27)#|
; Carmichael number
(test-fermat-prime 561 false)
(test-fermat-prime 1105 false)
(test-fermat-prime 1729 false)
(test-fermat-prime 2465 false)
(test-fermat-prime 2821 false)
(test-fermat-prime 6601 false)
|#

'(exercise 1 28)
;; Miller-Rabin test
;; will come back to that later



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.3 Formulating Abstraction with Higher-Order Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube x)(* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
;(sum-integers 1 9)
;45

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)(sum-cubes (+ a 1) b))))
;(sum-cubes 1 4)
;100

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
;(* 8 (pi-sum 1 10000))
;3.141

(define (func a b);term and next are undefined
  (if (> a b)
      0
      (+ (term a)(func (next a) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)(sum term (next a) next b))))

(define (inc n)(+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

;(sum-cubes 1 4)
;100

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

;(sum-integers 1 10)
;55

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;(* 8 (pi-sum 1 1000))
;3.139592655589783

(define (integral f a b dx)
  (define (add-dx x)(+ x dx))
  (* dx (sum f
             (+ a (/ dx 2.0))
             add-dx
             b)))

;(integral cube 0 1 0.01)
;0.24998750000000042


'(exercise 1 29)#|
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (+2h x) (+ x (* 2 h)))
  (* (/ h 3.0) (+ (f a)
                  (* 4.0 (sum f (+ h a) +2h (- b h)))
                  (* 2.0 (sum f (+2h a) +2h (- b h)))  
                  (f b))))

(simpson-integral cube 0 1 100)
;0.25
|#


'(exercise 1 30)#|
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum identity 1 inc 9)
;45
|#

'(exercise 1 31)#|
;;recursive product
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)(product term (next a) next b))))

(product identity 1 inc 5)
;120

;;iterative product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(product identity 1 inc 5)
;120

;;factorial in terms of product can be written as
(define (factorial n)
  (product identity 1 inc n))

(factorial 5)
;120

;another algorithm to approximate pi, written intermsof product
(define (pi-sum n)
  (define (pi-term x)
    (if (even? x)
        (/ (+ x 2)(+ x 1))
        (/ (+ x 1)(+ x 2))))
  (* 4.0 (product pi-term 1 inc n)))

(pi-sum 6)
;3.3436734693877552
(pi-sum 100)
;3.1570301764551676
|#

'(exercise 1 32)#|
;; recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)(accumulate combiner null-value term (next a) next b))))
(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

(sum identity 1 inc 9)
;45
(product identity 1 inc 5)
;120

;; iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)(combiner (term a) result))))
  (iter a null-value))
(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

(sum identity 1 inc 9)
;45
(product identity 1 inc 5)
;120
|#

'(exercise 1 33)#|
(define (filtered-acc combiner nulval term a next b filter) 
  (cond
    ((> a b) nulval)
    ((filter a)
     (combiner (term a)(filtered-acc combiner nulval term (next a) next b filter))) 
    (else
     (filtered-acc combiner nulval term (next a) next b filter))))

(define (sum-square-prime a b)
  (filtered-acc + 0 square a inc b prime?))

(sum-square-prime 2 3);1 is treated as prime anyway
;13

(define (product-of-relative-prime n)
  (define (prime-gcd? i)
    (= 1 (gcd i n)))
  (filtered-acc * 1 identity 2 inc n prime-gcd?))

(product-of-relative-prime 10)
;189 = 3*7*9
|#


(define (pi-sum a b)
  (sum (lambda (x)(/ 1.0 (* x (+ x 2))))
       a
       (lambda (x)(+ x 4))
       b))

;(* 8 (pi-sum 1 100))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x)(+ x dx))
          b)
     dx))

;(integral cube 0 1 0.01)

(define (plus4 x)(+ x 4))
(define plus4 (lambda (x)(+ x 4)))
;these two are equivalent


;((lambda (x y z)(+ x y (square z))) 1 2 3)
;12

;f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
;a = (1+xy)
;b = (1-y)
;f(x,y) = xa^2 + yb + ab


(define (f x y)
  (define (f-helper a b)
    (+ (* x a a)
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))
;(f 1 1);4

(define (f x y)
  ((lambda (a b)
     (+ (* x a a)
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
;(f 1 1);4

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x a a)
       (* y b)
       (* a b))))
;(f 1 1);4

;; experiment with let
#|
(define x 5)
(+ (let ((x 3))(* x 10)) x)
;inner-x = 3 outer-x = 5
x
;verify that x is still 5

(define x 2)
(let ((x 3)
      (y (+ x 2)))
  (* x y))
;12
;verify taht y=x+2 using outer-x = 2
|#


;internal define plays similar role
(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x a a)
     (* y b)
     (* a b)))


'(exercise 1 34)#|
(define (f g)
  (g 2))

(f square)
;4
(f (lambda (z)(* z (+ z 1))))
;6

;(f f)
;(f 2)
;(2 2)
;!! boom
|#


(define (search f neg pos)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((mid (average neg pos)))
    (if (close-enough? neg pos)
        mid
        (let ((test-value (f mid)))
          (cond
            ((positive? test-value)(search f neg mid))
            ((negative? test-value)(search f mid pos))
            (else mid))))))

;(search identity -1 1)
;0
;(search (lambda (x)(+ x 0.25)) -1.0 1.0)
;-0.25

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond
      ((and (negative? a-value)(positive? b-value))(search f a b))
      ((and (negative? b-value)(positive? a-value))(search f b a))
      (else (display "value not opposite")))))

;(half-interval-method sin 2.0 4.0)
;3.14111328125
;(half-interval-method (lambda (x)(- (* x x x) (* 2 x) 3)) 1.0 2.0)
;1.89306640625


(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(fixed-point cos 1.0)
;0.7390822985224023
;(fixed-point (lambda (y)(+ (sin y)(cos y))) 1.0)
;1.2587315962971173

(define (sqrt! x)
  (fixed-point (lambda (y)(average y (/ x y))) 1.0))

;(sqrt! 4.0)
;(sqrt! 9.0)
;(sqrt! 100)

'(exercise 1 35)#|
;; according to definition phi = ( 1 + sqrt(5)) / 2
;; now approximate it with fixed-point procedure
(define (golden-ratio start-point)
  (fixed-point (lambda (x)(+ 1 (/ 1.0 x))) start-point))

(golden-ratio 1.0)
(golden-ratio 10.0)
(golden-ratio 0.1)
;~=1.618

;; using average damping
(fixed-point (lambda (x)(average x (+ 1 (/ 1 x)))) 1.0) 
|#


'(exercise 1 36)#|
(define (fixed-point-print f first-guess)
  (define tolerance 0.0001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; test if fixed-point-print works 
;; with golden-ratio example from (exercise 1 36)
;(fixed-point-print (lambda (x)(average x (+ 1 (/ 1 x)))) 1.0)

#|print the trail ...
1.0
1.5
1.5833333333333333
1.6074561403508771
1.6147785476652068
1.61702925556443
1.617723628348796
1.6179380934832117
1.6180043565683029|#

;; x-to-x-power
;; without average damping
(define (x-to-x-power y guess); x^x = 1000, plz find x
  (fixed-point-print
   (lambda (x)(/ (log y)(log x))) guess))
(x-to-x-power 1000.0 10.0)
;; converges in 28 iterations
#|
10.0
2.9999999999999996
6.2877098228681545
3.7570797902002955
5.218748919675316
4.1807977460633134
4.828902657081293
4.386936895811029
4.671722808746095
4.481109436117821
4.605567315585735
4.522955348093164
4.577201597629606
4.541325786357399
4.564940905198754
4.549347961475409
4.5596228442307565
4.552843114094703
4.55731263660315
4.554364381825887
4.556308401465587
4.555026226620339
4.55587174038325
4.555314115211184
4.555681847896976
4.555439330395129
4.555599264136406
4.555493789937456
4.555563347820309|#


;; x-to-x-power
;; with average damping
(define (x-to-x-power y guess); x^x = 1000, plz find x
  (fixed-point-print
   (lambda (x)(average x (/ (log y)(log x)))) guess))
(x-to-x-power 1000.0 10.0)
;; converges in 8 iterations
#|
10.0
6.5
5.095215099176933
4.668760681281611
4.57585730576714
4.559030116711325
4.55613168520593
4.555637206157649
4.55555298754564|#
|#


'(exercise 1 37)#|
;; recursive
(define (cont-frac n d k)
  (define (recur i)
    (if (= i (+ k 1))
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

;; iterative
(define (cont-frac n d k) 
  (define (iter i result) 
    (if (= i 0) 
        result 
        (iter (- i 1) (/ (n i) (+ (d i) result))))) 
  (iter k 0.0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)
;same output for both procedure
;0.6179775280898876
;0.6180555555555556
;0.6180257510729613
|#



'(exercise 1 38)#|
;; recursive
(define (cont-frac n d k)
  (define (recur i)
    (if (= i (+ k 1))
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

;; iterative
(define (cont-frac n d k) 
  (define (iter i result) 
    (if (= i 0) 
        result 
        (iter (- i 1) (/ (n i) (+ (d i) result))))) 
  (iter k 0.0))
;; you can use either

;;write D that produces 1 2 1 1 4 1 1 8 1 ...
(define (D i)
  (if (= (remainder i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))

;; then apply
(define (euler k)
  (+ 2 (cont-frac (lambda (i) 1.0) D k)))
(euler 7)
(euler 8)
(euler 9)
(euler 10)
|#


'(exercise 1 39)#|
;; recursive
(define (cont-frac n d k)
  (define (recur i)
    (if (= i (+ k 1))
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

;; iterative
(define (cont-frac n d k) 
  (define (iter i result) 
    (if (= i 0) 
        result 
        (iter (- i 1) (/ (n i) (+ (d i) result))))) 
  (iter k 0.0))

;; you can use either

;; (tan x) = x/(1-recursion) = (-x)/(-1+recursion)
;; (N i) = x   if i=1
;; (N i) = x^2 if i=other
;; (D i) = -1 -3 -5 -7 ..
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x (- (* x x))))
             (lambda (i)
               (- (* i 2) 1))
             k))
(define pi 3.141592653589793238462623382)
(tan-cf (/ pi 4) 2);approaching 1.0
(tan-cf (/ pi 4) 3)
(tan-cf (/ pi 4) 4)
(tan-cf (/ pi 4) 5)
(tan-cf (/ pi 4) 6)
(tan-cf (/ pi 4) 7)
(tan-cf (/ pi 4) 8)
(tan-cf (/ pi 4) 9)

|#




(define (average-damp f)
  (lambda (x)(average x (f x))))

;((average-damp square) 10)
;55
;((average-damp square) 5)
;15

(define (sqrt! x)
  (fixed-point (average-damp (lambda (y)(/ x y))) 1.0))
;(sqrt! 4.0)
(define (cbrt! x)
  (fixed-point (average-damp (lambda (y)(/ x (* y y)))) 1.0))
;(cbrt! 27.0)

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx))(g x)) dx)))
;((deriv cube) 5)
;75.00014999664018

(define (newton-transform g)
  (lambda (x)(- x (/ (g x)((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt! x)
  (newton-method (lambda (y)(- (* y y) x)) 1.0))
;(sqrt! 4.0)
;2.0000000000002385

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt! x)
  (fixed-point-of-transform (lambda (y)(/ x y))
                            average-damp
                            1.0))
;(sqrt! 4.0)
;2.000000000000002


(define (sqrt! x)
  (fixed-point-of-transform (lambda (y)(- (square y) x))
                            newton-transform
                            1.0))
;(sqrt! 4.0)
;2.0000000000002385



'(exercise 1 40)#|
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

((cubic 1 1 1) 2);15

(define (find-zero-cubic x)
  (newton-method (cubic 1 1 -3) 1));solve x for (x^3)+(x^2)+x-3 = 1

(find-zero-cubic 0.0)
;1
|#

'(exercise 1 41)#|
;; double is builtin, so it must be overwritten
(define (double! f)
  (lambda (x)(f (f x))))

((double! inc) 4)
;6
((double! square) 3)
;81

(((double! (double! double!)) inc) 5)
;21
;; the expansion is demonstrated as below
;(((double! (lambda (x)(double! (double! x)))) inc) 5)
;(((double! (double! double!)) inc) 5)
;(((double! (lambda (x) (double! (double! x)))) inc) 5)
; ...
;((double! (double! (double! (double! inc)))) 5)
;((double! (double! (double! (lambda (x) (inc (inc x)))))) 5)
; ... 
;(inc (inc (inc...(inc 5))))))))))))))))
;16 incs in total
|#

'(exercise 1 42)#|
(define (compose f g)
  (lambda (x)(f (g x))))

((compose square inc) 6)
;49
|#

'(exercise 1 43)#|
;; recursive(1)
(define (repeat f n)
  (lambda (x)(if (< n 1);instead of (= n 0) to prevent negative
                 x
                 (f ((repeat f (- n 1)) x)))))

((repeat square 2) 5);625

;; recursive(2)
;; stack up f(f(f(...(f(x))...))) and eval once
;; using compose
(define (repeat f n)
  (define (compose f g)(lambda (x)(f (g x))))
  (if (< n 1)
      identity
      (compose f (repeat f (- n 1)))))
;; (lambda (x)(f (repeat f (- n 1)))) will work fine

((repeat square 2) 5);625

;; iterative
(define (repeat f n)
  (define (compose f g)(lambda (x)(f (g x))))
  (define (iter n result)
    (if (< n 1)
        result
        (iter (- n 1) (compose f result))))
  (iter n identity))

((repeat square 2) 5);625
|#


'(exercise 1 44)#|
(define (repeat f n)
  (define (compose f g)(lambda (x)(f (g x))))
  (if (< n 1)
      identity
      (compose f (repeat f (- n 1)))))

(define dx 0.00001) 

(define (smooth f) 
  (lambda (x) 
    (/ (+ (f (- x dx)) 
          (f x) 
          (f (+ x dx))) 
       3))) 

(define (n-fold-smooth f n) 
  ((repeated smooth n) f)) 

(define (tick x)
  (if (> x 0)
      (* 10000 x);abrupt on the positve side
      x));linear on negative

((smooth tick) 0);0.033330
|#

'(exercise 1 45)#|
(define (repeat f n)
  (define (compose f g)(lambda (x)(f (g x))))
  (if (< n 1)
      identity
      (compose f (repeat f (- n 1)))))

(define (cube-root x)
  (fixed-point
   (average-damp (lambda (y)(/ x (* y y))))
   1.0))

(define (4th-root x)
  (fixed-point 
   (repeat (average-damp (lambda (y)(/ x (expt y 3)))) 2)
   1.0))

;(4th-root 16.0);1.98
|#


'(exercise 1 46)#|
;; iterative-improve
(define (iterative-improve good? imprv)
  (lambda (x)
    (if (good? x)
        (imprv x)
        ((iterative-improve good? imprv)(imprv x)))))

;; now apply it to square-root and fixed-point

;; square-root
;; original
(define (sqrt! x)
  (define (improve guess x)(average guess (/ x guess)))
  (define (average x y)(/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))
(sqrt! 4.0)
;2.0000000929222947
;; square-root
;; with iterative-improve
(define (sqrt! x)
  (define (improve guess x)(average guess (/ x guess)))
  (define (average x y)(/ (+ x y) 2))
  (define (close? guess x)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve (lambda (guess)(close? guess x))
                      (lambda (guess)(improve guess x)))
   1.0))
(sqrt! 4.0)
;2.000000000000002
;; end of square root 



;; fixed-point
;; original
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;testing
(fixed-point cos 1.0);0.7390822985224023
;; fixed-point
;; with iterative-improve
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve (lambda (guess)(close? guess (f guess)))
                      (lambda (guess)(f guess)))
   first-guess))
;testing
(fixed-point cos 1.0);0.7390822985224023
|#


