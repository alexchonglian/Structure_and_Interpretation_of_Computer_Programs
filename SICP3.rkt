;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 3
;; Modularity, Object and State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.1 Assignment and Local State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))  balance)
      "Insufficent funds"))

#|
(withdraw 25);75
(withdraw 25);50
(withdraw 60);Insufficient
(withdraw 15);35
|#


(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient"))))

#|
(new-withdraw 25);75
(new-withdraw 25);50
(new-withdraw 60);Insufficient
(new-withdraw 15);35
|#


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insuffi")))


(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

#|
(W1 50);50
(W2 70);30
(W2 40);Insufficient
(W1 40);10
|#


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insuf fund"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (display "error"))))
  dispatch)

(define acc (make-account 100))

#|
((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)
|#

(define acc2 (make-account 100))



'(exercise 3 1)#|
(define (make-accumulator init)
  (lambda (inc)
    (begin (set! init (+ inc init)) init)))

(define A (make-accumulator 5))
(A 10);15
(A 10);25

(define (make-accumulator init)
  (lambda (x)
    (set! init (+ x init))
    init)) 

(define A (make-accumulator 5))
(A 10);15
(A 10);25
|#


'(exercise 3 2)#|
'(single params)
(define (make-monitor f)
  (let ((counter 0))
    (lambda (arg)
      (begin (set! counter (+ 1 counter));begin is not necesary, leave it
             (f arg)))))

(define square-root (make-monitor sqrt))
(square-root 100);10


'(accept multiple parameters)
(define (make-monitor f)
  (let ((counter 0))
    (define (monitor . args)
      (begin (set! counter (+ 1 counter))
             (apply f args)))
    monitor))

(define add (make-monitor +))
(add 1 2 3 4 5 6 7 8 9);45


'(add the how-many-calls? and reset-count!)
(define (make-monitor f)
  (let ((counter 0))
    (define (monitor . args)
      (cond ((eq? (car args) 'how-many-calls?) counter)
            ((eq? (car args) 'reset-count!)(begin (set! counter 0) 
                                                  counter))
            (else (begin (set! counter (+ 1 counter))
                         (apply f args)))))
    monitor))

(define add (make-monitor +))
(add 1 2 3 4 5 6 7 8 9);45
(add 1 2 3 4 5);15
(add 6 7 8 9);30
(add 'how-many-calls?);3
(add 'reset-count!);0
(add 1 2 3 4);10
(add 'how-many-calls?);1

'(remove begin)
(define (make-monitor f)
  (let ((counter 0))
    (define (monitor . args)
      (cond ((eq? (car args) 'how-many-calls?) counter)
            ((eq? (car args) 'reset-count!) (set! counter 0) counter)
            (else (set! counter (+ 1 counter)) (apply f args))))
    monitor))

(define add (make-monitor +))
(add 1 2 3 4 5 6 7 8 9);45
(add 1 2 3 4 5);15
(add 6 7 8 9);30
(add 'how-many-calls?);3
(add 'reset-count!);0
(add 1 2 3 4);10
(add 'how-many-calls?);1

'(saw interesting solution)
(define (make-accumulator acc) 
  (lambda (x)  
    (set! acc (+ acc x)) 
    acc)) 
(define (make-monitored f) 
  (define calls (make-accumulator 0)) 
  (define (reset) (calls (- (calls 0)))) 
  (define (mf a) 
    (cond ((equal? a 'how-many-calls?) (calls 0)) 
          ((equal? a 'reset-count) reset) 
          (else (calls 1) (f a)))) 
  mf) 

(define s (make-monitored sqrt)) 
(s 100)  ;; 10 
(s 144)  ;; 12 
(s 'how-many-calls?) ;; 2 
|#


'(exercise 3 3)#|
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insuf fund"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pswd-attempt m)
    (if (eq? pswd-attempt password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (lambda (x) "wrong dispatcher")))
        (lambda (x) "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret))

((acc 'secret 'withdraw) 40);60
((acc 'secret 'withdraw) 40);20
((acc 'secret 'withdraw) 40);insuf fund
((acc 'guess 'withdraw) 40);incorrect pswd
((acc 'secret 'deposit) 10);30
((acc 'secret 'deposit) 10);40
((acc 'secret 'deposit) 10);50
((acc 'guess 'deposit) 10);"Incorrect password"
((acc 'secret 'crazy-operation) 10);"wrong dispatcher"
|#



'(exercise 3 4)#|
(define (make-account balance password)
  (let ((num-fail-login 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insuf fund"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cop) "call-the-cop")
    (define (dispatch pswd m)
      (if (eq? pswd password)
          ;;successful login, set num-fail-login to 0
          (begin (set! num-fail-login 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (lambda (x) "wrong dispatcher"))))
          ;;failed login, if 7 times=>alarm; else=>inc num-fail-login
          (if (>= num-fail-login 7)
              (lambda (x) (call-the-cop))
              (begin (set! num-fail-login (+ 1 num-fail-login))
                     (lambda (x) (list num-fail-login 'times 'failed))))))
    dispatch))

(define acc (make-account 100 'secret))
((acc 'secret 'withdraw) 40);60
((acc 'secret 'withdraw) 40);20
((acc 'secret 'withdraw) 40);insuf fund
((acc 'guess 'withdraw) 40);incorrect pswd
((acc 'secret 'deposit) 10);30
((acc 'secret 'deposit) 10);40
((acc 'secret 'deposit) 10);50

;wrong password once then try correct one
((acc 'guess 'withdraw) 40);"Incorrect password"
((acc 'secret 'deposit) 10);60

;wrong password for 7 times
((acc 'guess 'withdraw) 40)
((acc 'guess 'withdraw) 40)
((acc 'guess 'withdraw) 40)
((acc 'guess 'withdraw) 40)
((acc 'guess 'withdraw) 40)
((acc 'guess 'withdraw) 40)
((acc 'guess 'withdraw) 40)
((acc 'guess 'withdraw) 40);"call-the-cop"

|#

;just reminder

;syntax for function that gets any number of args
(define (f . args) args)
;(f 1 2 3)

;syntax for lambda that gets any number of args
(define f (lambda args args))
;(f 1 2 3)


(define random-init 0)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment)(iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


; using rand-update directly (no assignment)

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond
          ((= trials-remaining 0) (/ trials-passed trials))
          ((= (gcd x1 x2) 1) (iter (- trials-remaining 1) 
                                   (+ trials-passed 1) x2))
          (else (iter (- trials-remaining 1)
                      trial-passed x2))))))
  (iter trials 0 initial-x))



'(exercise 3 5)#|
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (P x y)
  (< (+ (expt (- x 5) 2) (expt (- y 7) 2))
     (expt 3 2)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (monte-carlo trials experiment))

;(estimate-integral P 2.0 8.0 4.0 10.0 100) 
|#


'(exercise 3 6)#|
(define rand
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate) 
             (begin (set! x (rand-update x)) x))
            ((eq? message 'reset)
             (lambda (new-value)(set! x new-value)))))
    dispatch))
|#


(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))

;(W 20);5
;(W 10);-5

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))

;(D 20);5
;(D 10);15

;((make-decrementer 25) 20)
;((lambda (amount) (- 25 amount)) 20)
;(- 25 20)


;difference between
(define peter-acc (make-account 100))
(define paul-acc (make-account 100))

(define peter-acc (make-account 100))
(define paul-acc peter-acc)

;factorial
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;(factorial 5);120

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

;(factorial 5);120


'(exercise 3 7)#|
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "insufficient fund"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (dispatch pswd-attempt m)
    (if (eq? pswd-attempt password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (lambda (x) "wrong dispatcher")))
        (lambda (x) "incorrect password")))
  dispatch)

(define acc (make-account 100 'secret))

((acc 'secret 'withdraw) 40);60
((acc 'secret 'withdraw) 40);20
((acc 'secret 'withdraw) 40);insuf fund
((acc 'guess 'withdraw) 40);incorrect pswd
((acc 'secret 'deposit) 10);30
((acc 'secret 'deposit) 10);40
((acc 'secret 'deposit) 10);50
((acc 'guess 'deposit) 10);"Incorrect password"
((acc 'secret 'crazy-operation) 10);"wrong dispatcher"

;; end of answer of 3.3 

;; create new account
(define acc (make-account 100 'secret))

;; define make-joint
(define (make-joint account pw-old pw-new)
  (lambda (pw-new-attempt msg)
    (if (eq? pw-new-attempt pw-new)
        (account pw-old msg)
        (lambda (x) "incorrect password"))))

(define jacc (make-joint acc 'secret 'new-secret))

((acc 'secret 'withdraw) 40);60
((jacc 'new-secret 'withdraw) 40);20
((acc 'secret 'deposit) 10);30
((jacc 'new-secret 'withdraw) 40);"insufficient fund"
((jacc 'guess 'withdraw) 40);"incorrect password"
|#


'(exercise 3 8)#|
;; the gist is to introduce local state variable
;; but there are many ways to achieve desired result

;; shyam's version - delayed response
(define (g y)
  (define (f x)
    (let ((z y))
      (set! y x)
      z))
  f)

(define f (g 0))
(+ (f 0) (f 1))
;(+ (f 1) (f 0))
(define f (g 0))
(+ (f 1) (f 0))
;(+ (f 0) (f 1))


;; my version - accumulated product

(define (g)
  (lambda (arg)
    (let ((prod 1))
      (set! prod (* arg prod))
      prod)))

(define (g init)
  (lambda (arg)
    (begin (set! init (* init arg))
           init)))

(define f (g 1))
(+ (f 0) (f 1))
;(+ (f 1) (f 0))
(define f (g 1))
(+ (f 1) (f 0))
;(+ (f 0) (f 1))

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.2 The Environment Model of Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; applying simple procs

'(exercise 3 9)
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
;(factorial 5);120

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                     params: n
;                     body: ...
;                        
;                        ^
;                        |    
;                       [*][*]---+
;                          ^     |
;                          |     V
;            +-------------|------------------------------+
;            |             |                              |
; global --> | factorial --+                              |
; env        |                                            |
;            +--------------------------------------------+
;                 ^              ^                      ^ 
;(factorial 6)    |              |                      |   
;              +-----+        +-----+                +-----+    
;         E1=> | n:6 |    E2=>| n:5 |     ...    E6=>| n:1 |       
;              +-----+        +-----+                +-----+
;              body of        body of                body of
;             factorial      factorial              factorial
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;(factorial 5);120

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                     params: n      params: product,counter,max-counter
;                     body: ...      body: ...
;                        
;                        ^                ^     
;                        |                |          
;                       [*][*]---+       [*][*]---+
;                          ^     |          ^     |
;                          |     V          |     V
;            +-------------|----------------|-------------+
;            |             |                |             |
; global --> | factorial --+                |             |
; env        | fact-iter -------------------+             |
;            |                                            |
;            +--------------------------------------------+
;               ^           ^                        ^ 
;(factorial 6)  |       E2  |                     E7 |   
;            +-----+    +--------+                +----------+    
;       E1=> | n:6 |    | prod:1 |                | prod:720 |      
;            +-----+    | cntr:1 |      ...       | cntr:7   |
;     (fact-iter 1 1 n) | max:6  |                | max:6    | 
;                       +--------+                +----------+ 
;                         body of                    body of
;                        fact-iter                  fact-iter
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; frames as the repo of local state

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds")))

(define W1 (make-withdraw 100))
;(W1 50)


'(exercise 3 10)
;'(let ((var exp)) body)
;'((lambda (var) body) exp)

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "insufficient funds"))))

(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds")))
   initial-amount))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; eval (define W1 (make-withdraw 100))
;
;                     params: initial-amount
;                     body: ((lambda (balance)
;                              (lambda (amount)
;                        ^       (if (>= ...
;                        |    
;                       [*][*]---+
;                          ^     |
;                          |     V
;            +-------------|------------------------------+
;            | mk-wthdw----+                              |
; global --> | W1---+                                     |
; env        |      |                                     |
;            +------|---------------^---------------------+
;                   |               |      
;                   V          +----^----+   
;                [*][*]    E1=>|init:100 |    
;                 |  |         +----^----+
;                 |  |              |
; params:amount<--+  |         +----^----+ 
; body: (if (>= )    |     E2=>|blnc:100 |
;            ...     |         +----^----+
;                    |              |
;                    +--------------+
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; apply W1, create env for {amount: 50}
;
;            +--------------------------------------------+
;            | mk-wthdw: ...                              |
; global --> | W1---+                                     |
; env        |      |                                     |
;            +------|---------------^---------------------+
;                   |               |      
;                   V          +----^----+   
;                [*][*]    E1=>|init:100 |    
;                 |  |         +----^----+
;                 |  |              |
; params:amount<--+  |         +----^----+    +----------+ 
; body: (if (>= )    |     E2=>|blnc:100 |<---|amount:50 |
;            ...     |         +----^----+    +----------+ 
;                    |              |
;                    +--------------+
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; complete calling W1, balance in E2 is modified
;
;            +--------------------------------------------+
;            | mk-wthdw: ...                              |
; global --> | W1---+                                     |
; env        |      |                                     |
;            +------|---------------^---------------------+
;                   |               |      
;                   V          +----^----+   
;                [*][*]    E1=>|init:100 |    
;                 |  |         +----^----+
;                 |  |              |
; params:amount<--+  |         +----^----+
; body: (if (>= )    |     E2=>|blnc:50  |
;            ...     |         +----^----+ 
;                    |              |
;                    +--------------+
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; create new W2 proc
;
;            +---------------------------------------------+
;            | mk-wthdw: ...                               |
; global --> | W1------------------------+                 |
; env        | W2--+                     |                 |
;            +-----|----------^----------|------------^----+
;                  |          |          |            |
;                  V     +----^----+     V       +----^----+
;               [*][*] E1|init:100 |  [*][*]  E3 |init:100 |
;                |  |    +----^----+   |  |      +----^----+
;                |  |         |        |  |           |
;                |  |    +----^----+   |  |      +----^----+
;                |  |  E2|blnc:50  |   |  |   E4 |blnc:100 |
;                |  |    +----^----+   |  |      +----^----+
;                |  |         |        |  |           |
;                |  +---------+        |  +-----------+
;                V                     |
;  params:amount                       |
;  body: (if (>= ) <-------------------+
;             ...
;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; internal definitions

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



'(exercise 3 11)
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (-  balance amount))
               balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (lambda (amount) "wrong message"))))
  dispatch)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (define acc (make-account 50))
;
;                     params: balance
;                     body: (define (withdraw amount) ...
;                        ^  (define (deposit amount) ...
;                        |  (define (dispatch m) ... 
;                        |  dispatch)
;                        |
;                       [*][*]---+
;                          ^     |
;                          |     V
;            +-------------|-------------------------------+
;            | mk-acct:----+                               |
; global --> |                                             |
; env        | acc--+                                      |
;            +------|-----------^--------------------------+
;                   |           |
;                   V     +-----^-------+
;                [*][*] E1| balance: 50 |<-------o--------o--------o
;                 |  |    |             |        |        |        |
;                 |  |    | withdraw:------->[*][*]       |        |
;                 |  |    | deposit:----------------->[*][*]       |
;                 |  |    | dispatch:------------------------->[*][*]
;                 |  +--->|             |
;                 |       +-------------+
;                 |  
;                 |  
;                 V                     
;        params: m                     
;        body: (cond ((eq? m 'withdraw) ... )
;                    ((eq? m 'deposit) ...)
;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ((acc 'deposit) 40)
;
;            +---------------------------------------------+
;            | mk-acct: ...                                |
; global --> |                                             |
; env        | acc--+                                      |
;            +------|-----------^--------------------------+
;                   |           |
;                   V     +-----^-------+
;                [*][*] E1| balance: 50 |<-------o--------o--------o
;                 |  |    |             |        |        |        |
;                 |  |    | withdraw:------->[*][*]       |        |
;                 |  |    | deposit:----------------->[*][*]       |
;                 |  |    | dispatch:------------------------->[*][*]
;                 |  +--->|             |
;                 |       +-------------* <---o--------------------o
;                 |                           |                    |
;                 |                    +----------+         +---------+
;                 |               E2=> |m:'deposit|    E3=> |amount:40|
;                 |                    +----------+         +---------+
;        params: m                     
;        body: (cond ((eq? m 'withdraw) ... )
;                    ((eq? m 'deposit) ...)
;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ((acc 'withdraw) 60)
;
;            +---------------------------------------------+
;            | mk-acct: ...                                |
; global --> |                                             |
; env        | acc--+                                      |
;            +------|-----------^--------------------------+
;                   |           |
;                   V     +-----^-------+
;                [*][*] E1| balance: 90 |<-------o--------o--------o
;                 |  |    |             |        |        |        |
;                 |  |    | withdraw:------->[*][*]       |        |
;                 |  |    | deposit:----------------->[*][*]       |
;                 |  |    | dispatch:------------------------->[*][*]
;                 |  +--->|             |
;                 |       +-------------* <---o--------------------o
;                 |                           |                    |
;                 |                    +----------+         +---------+
;                 |               E4=> |m:'witdraw|    E5=> |amount:60|
;                 |                    +----------+         +---------+
;        params: m                     
;        body: (cond ((eq? m 'withdraw) ... )
;                    ((eq? m 'deposit) ...)
;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.3 Modelling with Mutable Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define x '((a b) c d))
(define y '(e f))

;x
;y
;(set-car! x y)
;x

(define z (cons y (cdr x)))
;z
;x

(set-cdr! x y)
;x

(define (cons! x y)
  (define (get-new-pair) '(()))
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

;(cons! 1 '(2 3 4))



'(exercise 3 12)#|
(define (append! x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(append '(1 2 3) '(4 5 6))
(append! '(1 2 3) '(4 5 6))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(append! '(1 2 3) '(4 5 6))
(define a '(1 2 3))
(define b '(4 5 6))
(append! a b)
a

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z;(a b c d)
(cdr x);(b)
(define w (append! x y))
w;(a b c d)
(cdr x);(b c d)

|#


'(exercise 3 13)#|
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
z;#0=(a b c . #0#)
(last-pair x);(f)
;; will run infinite loop
|#

'(exercise 3 14)#|
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v '(a b c d))
(define w (mystery v))
w
|#


;; part shared among different data objects
(define x (list 'a 'b))
(define z1 (cons x x))

#|
z1;((a b) a b)
(set-car! (car z1) 'wow)
z1;((wow b) wow b)
|#

(define z2 (cons (list 'a 'b) (list 'a 'b)))

#|
z2;((a b) a b)
(set-car! (car z2) 'wow)
z2;((wow b) a b)
|#

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)


'(exercise 3 15)
;; you know :)

'(exercise 3 16)
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define three '(a b c))
;(count-pairs three)

(define four '(a b c))
(set-car! four (cddr four))
;(count-pairs four)

(define level3 '(c))
(define level2 (cons level3 level3))
(define level1 (cons level2 level2))
(define seven level1)
;(count-pairs seven)

(define infinite '(a b c))
(set-cdr! (cddr infinite) infinite)
;(count-pairs infinite)

'(exercise 3 17)
(define (count-pairs x)
  (define visited '()) 
  (define (counter x)
    (if (or (not (pair? x))(memq x visited))
        0
        (begin 
          (set! visited (cons x visited))
          (+ (counter (car x))
             (counter (cdr x))
             1))))
  (counter x))
#|
(count-pairs three);3
(count-pairs four);3
(count-pairs seven);3
(count-pairs infinite);3
|#

'(exercise 3 18)
(define (cycle? x)
  (define visited '())
  (define (iter current)
    (set! visited (cons current visited))
    (cond ((null? (cdr current)) #f)
          ((memq (cdr current) visited) #t)
          (else (iter (cdr current)))))
  (iter x))

#|
(cycle? three);#f
(cycle? four);#f
(cycle? seven);#f
(cycle? infinite);#t
|#

'(exercise 3 19)
;; use a fast pointer and a slow pointer
;; each iteration:
;;     fast pointer cdr'ed twice
;;     slow pointer cdr'ed once
(define (cycle? lst)
  (define (safe-cdr l)
    (if (pair? l)
        (cdr l)
        '()))
  (define (iter a b)
    (cond ((not (pair? a)) #f)
          ((not (pair? b)) #f)
          ((eq? a b) #t)
          ((eq? a (safe-cdr b)) #t)
          (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))

#|
(cycle? three);#f
(cycle? four);#f
(cycle? seven);#f
(cycle? infinite);#t
|#


'(exercise 3 20)
(define (cons! x y)
  (define (set-x! v)(set! x v))
  (define (set-y! v)(set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else "wrong")))
  dispatch)

(define (car! z) (z 'car))
(define (cdr! z) (z 'cdr))
(define (set-car!! z new)
  ((z 'set-car!) new)
  z)
(define (set-cdr!! z new)
  ((z 'set-cdr!) new)
  z)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (define x (cons 1 2))
; (define z (cons x x))
;
;            +-------------------------------------------------+
;            | cons    : ...                                   |
;            | car     : ...            set-car!: ...          |
;            | cdr     : ...            set-car!: ...          |
; global --> |                                                 |
; env        |    x <----------------------------------------------+-+
;            |    |                      z                     |   | |
;            +----|------------^---------|-------------^-------+   | |
;                 |            |         |             |           | |
;                 |       +----^----+    |        +----^----+      | |
;                 V   E1=>|x:1      |    V    E1=>|x:x-------------+ |
;              [*][*]     |y:2      |  [*][*]     |y:x---------------+
;               |  |      |set-x! ..|   |  |      |set-x! ..|
; params:m <----+  +----->|set-y! ..|   |  +----->|set-y! ..|
; body:                   |dispatch |   |         |dispatch |     
;   dispatch              +---------+  dispatch   +---------+
;                                 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (set-car! (cdr z) 17)
;
;            +-------------------------------------------------+
;            | cons    : ...                                   |
;            | car     : ...            set-car!: ...          |<----+
;            | cdr     : ...            set-car!: ...          |     |
; global --> |                                                 |     |
; env        |    x <--------------------------------------------+-+ |
;            |    |                      z                     | | | |
;            +----|------------^---------|-------------^-------+ | | |
;                 |            |         |             |         | | |
;                 |       +----^----+    |        +----^----+    | | |
;                 V   E1=>|x:1      |    V    E2=>|x:x-----------+ | |
;              [*][*]     |y:2      |  [*][*]     |y:x-------------+ |
;               |  |      |set-x! ..|   |  |      |set-x! ..|        |
; params:m <----+  +----->|set-y! ..|   |  +----->|set-y! ..|        |
; body:                   |dispatch |   |         |dispatch |        |
;   dispatch              +^-------^+  dispatch   +----^----+        |
;                    E5    |     E6|                E3 |             |
;                 +--------^--+ +--^--+           +----^----+        |
;                 |m:'set-car!| |v:17 |           | m: 'cdr |        |
;                 +-----------+ +-----+           +---------+        |
;                 call dispatch call set-x       call dispatch       |
;                                                                    |  
;                                                 +---------+        |
;                                             E4=>|z:x      |--------+
;                                                 |new:17   |
;                                                 +---------+
;                                                call set-car!
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (car z)
;
;            +-------------------------------------------------+
;            | cons    : ...                                   |
;            | car     : ...            set-car!: ...          |
;            | cdr     : ...            set-car!: ...          |
; global --> |                                                 |
; env        |    x <----------------------------------------------+-+
;            |    |                      z                     |   | |
;            +----|------------^---------|-------------^-------+   | |
;                 |            |         |             |           | |
;                 |       +----^----+    |        +----^----+      | |
;                 V   E1=>|x:17     |    V    E1=>|x:x-------------+ |
;              [*][*]     |y:2      |  [*][*]     |y:x---------------+
;               |  |      |set-x! ..|   |  |      |set-x! ..|
; params:m <----+  +----->|set-y! ..|   |  +----->|set-y! ..|
; body:                   |dispatch |   |         |dispatch |     
;   dispatch              +----^----+ dispatch    +---------+
;                              |   
;                           +--^--+
;                       E3=>|m:car|
;                           +-----+
;                          call dispatch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; representing queues

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      "empty already"
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         "empty already")
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


'(exercise 3 21)#|
(define q1 (make-queue))

(insert-queue! q1 'a)
;((a) a)
(insert-queue! q1 'b)
;((a b) b)
(delete-queue! q1)
;((b) b)
(delete-queue! q1)
;(() b)
(delete-queue! q1)

(empty-queue? q1)

(define (print-queue queue) (car queue))

(define q1 (make-queue))
(insert-queue! q1 'a)
;((a) a)
(print-queue q1)
;(a)
(insert-queue! q1 'b)
;((a b) b)
(print-queue q1)
;(a b)
(delete-queue! q1)
;((b) b)
(print-queue q1)
;(b)
(delete-queue! q1)
;(() b)
(print-queue q1)
;()
(delete-queue! q1)

(empty-queue? q1)
|#


'(exercise 3 22)#|
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          "empty already"
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             "empty already")
            (else (set-front-ptr! (cdr front-ptr)))))
    (define (print-queue) front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'empty-queue) emtpy-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else "wrong message")))
    dispatch))
|#

'(exercise 3 23)
;; incomplete
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          "empty already"
          (car front-ptr)))
    (define (rear-queue)
      (if (empty-queue?)
          "empty already"
          (car rear-ptr)))
    (define (insert-queue-front! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))
    (define (insert-queue-rear! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))
    (define (delete-queue-front!)
      (cond ((empty-queue?)
             "empty already")
            (else (set-front-ptr! (cdr front-ptr)))))
    (define (delete-queue-rear!)
      (cond ((empty-queue?)
             "empty already")
            (else (set-rear-ptr! (cdr rear-ptr)))))
    (define (print-queue) front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'empty-queue) emtpy-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'rear-queue) rear-queue)
            ((eq? m 'insert-queue-front!) insert-queue-front!)
            ((eq? m 'insert-queue-rear!) insert-queue-rear!)
            ((eq? m 'delete-queue-front!) delete-queue-front!)
            ((eq? m 'delete-queue-rear!) delete-queue-rear!)
            ((eq? m 'print-queue) print-queue)
            (else "wrong message")))
    dispatch))

(define t1 '(*table* (a . 1) (b . 2) (c . 3)));
;t1

;; assoc in builtin, use asso
(define (lookup key table)
  (let ((record (asso key (cdr table)))) ;; ignore the *table* label
    (if record
        (cdr record)
        #f)))
(define (asso key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (asso key (cdr records)))))

;(lookup 'a t1)

(define (insert! key value table)
  (let ((record (asso key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

;(insert! 'd 4 t1)
;t1

(define (make-table)
  (list '*table*))

;; two dimensional tables

(define t2 '(*table* (math (+ . 43) (- . 45) (* . 42)) 
                     (letters (a . 97) (b . 98))))

(define (lookup key-1 key-2 table)
  (let ((subtable (asso key-1 (cdr table))))
    (if subtable
        (let ((record (asso key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

#|
(lookup 'math '+ t2)
(lookup 'math '* t2)
(lookup 'math '/ t2)
(lookup 'letters 'a t2)
(lookup 'string 'a t2)
|#

(define (insert! key-1 key-2 value table)
  (let ((subtable (asso key-1 (cdr table))))
    (if subtable
        (let ((record (asso key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key-2 value)
                                       (cdr subtable)))))
        (set-cdr! table (cons (list key-1 (cons key-2 value))
                              (cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (asso key-1 (cdr local-table))))
        (if subtable
            (let ((record (asso key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (asso key-1 (cdr local-table))))
        (if subtable
            (let ((record (asso key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1 (cons key-2 value))
                                        (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else "wrong message")))
    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


'(exercise 3 24)#|
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1
                                              (cons key-2 value))
                                        (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else "wrong message")))
    dispatch))

(define (same-key? k1 k2)
  (define tolerance 0.0001)
  (< tolerance (abs (- k1 k2))))
(define operation-table (make-table same-key?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
|#

'(exercise 3 25)#|
;; insert incomplete
(define (make-table)
  
  ;(let ((local-table (list '*table*)))
  (let ((local-table '(*table* (math (+ . 43) (- . 45) (* . 42)) 
                               (letters (a . 97) (b . 98)))))
    
    (define (asso key records)
      (cond ((null? records) #f)
            ((equal? key (caar records)) (car records))
            (else (asso key (cdr records)))))
    
    (define (lookup all-keys)
      (define (iter keys current-table)
        (if (null? keys)
            current-table
            (let ((next-table (asso (car keys) current-table)))
              (if next-table
                  (iter (cdr keys)(cdr next-table))
                  #f))))
      (iter all-keys (cdr local-table)))
    
    (define (insert! all-keys value)
      (define (iter keys current-table)
        (if (null? keys)
            (set-cdr! current-table value)
            (let ((next-table (asso (car keys) current-table)))
              (if next-table
                  (iter (cdr keys)(cdr next-table))
                  (set-cdr! current-table (cdr keys))))))
      (iter all-keys (cdr local-table)))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (lambda (x) "wrong message"))))
    dispatch))

(define t1 (make-table))

((t1 'lookup) '(math +));43
((t1 'lookup) '(math -));45
((t1 'lookup) '(letters a));97
|#



'(exercise 3 26)#|
;; node: ( (key value) left right )

(define (entry tree) (car tree))

(define (left tree) (cadr tree))

(define (right tree) (caddr tree))

(define (make-tree entry left right) (list entry left right))

(define (adjoin-set x set)
  (cond ((null? set)(make-tree x '() '()))
        ((= (car x)(car (entry set))) set)
        ((< (car x)(car (entry set))) 
         (make-tree (entry set)
                    (adjoin-set x (left set))
                    (right set)))
        ((> (car x)(car (entry set)))
         (make-tree (entry set)
                    (left set)
                    (adjoin-set x (right set))))
        (else "wrong")))

(define (make-table)
  (let ((local-table '()))
    
    (define (lookup key records)
      (cond ((null? records) #f)
            ((= key (car (entry records))) (entry records))
            ((< key (car (entry records))) (lookup key (left records)))
            ((> key (car (entry records))) (lookup key (right records)))
            (else "wrong")))
    
    (define (insert! key value)
      (let ((record (lookup key local-table)))
        (if record
            (set-cdr record value)
            (set! local-table 
                  (adjoin-set (cons key value) local-table)))))
    
    (define (get key)
      (lookup key local-table))
    
    (define (dispatch m)
      (cond ((eq? m 'get-proc) get)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'print) local-table)
            (else "wrong")))
    dispatch))

(define table (make-table))
(define get (table 'get-proc))
(define put (table 'insert-proc))

;; Solution for multi-dimensional tables


; helper methods 

(define (make-record key value)
  (list (cons key value) nil nil))
(define (get-key record) (caar record))
(define (get-value record) (cdar record))
(define (set-key! record new-key) (set-car! (car record) new-key))
(define (set-value! record new-value) (set-cdr! (car record) new-value))
(define (get-left record) (cadr record)
(define (get-right record) (caddr record))
(define (set-left! record new-left) (set-car! (cdr record) new-left))
(define (set-right! record new-right) (set-car! (cddr record) new-right))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (get-key records)) (get-value records))
        ((< key (get-key records)) (assoc key (get-left records)))
        (else (assoc key (get-right records)))))
  
(define (add-record key value table)
  (define (iter record parent set-action)
    (cond ((null? record)
           (let ((new (make-record key value)))
             (set-action parent new)
             (car new)))
          ((equal? key (get-key record))
           (set-value! record value)(car record))
          ((< key (get-key record))
           (iter (get-left record) record set-left!))
          (else (iter (get-right record) record set-right!))))
  (iter (cdr table) table set-cdr!))

; the procedure 
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter keys records)
        (if (null? keys) records
            (let ((found (assoc (car keys) records)))
              (if found (iter (cdr keys) found)
                  #f))))
      (iter keys (cdr local-table)))
    (define (insert! keys value)
      (define (iter keys subtable)
        (cond ((null? (cdr keys)) (add-record (car keys) value subtable))
              (else (let ((new (add-record (car keys) nil subtable)))
                      (iter (cdr keys) new)))))
      (iter keys local-table)
      'ok)
    (define (print) (display local-table) (newline))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) print)
            (else (lambda (x) "wrong message"))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print-table (operation-table 'print))
|#


'(exercise 3 27)
(define (lookup key table)
  (let ((record (asso key (cdr table)))) ;; ignore the *table* label
    (if record
        (cdr record)
        #f)))
(define (asso key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (asso key (cdr records)))))
(define (insert! key value table)
  (let ((record (asso key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))


(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(set! fib (memoize fib))
;(fib 100)














