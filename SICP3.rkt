
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
      (begin (set! balance (- balance amount)) balance)
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

#|
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) 
                 (list balance initial-amount))
          "Insu"))))

(define W1 (make-withdraw 100))
(W1 10)
(W1 50)
(W1 30)
|#

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

; API
;(make-queue)
;(empty-queue? <queue>)
;(front-queue <queue>)
;(insert-queue! <queue> <item>)
;(delete-queue! <queue>)

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
;; proc \w local state
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
          (display "empty already") ;; "error" VS. (display "error")
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

'(exercise 3 23)#|
; use 2 lists as 1 data block
; first = (cons data second)
; second = (cons prev next)
;
;         --------   --------   --------
;        /        \ /        \ /        \ /
;       /          X          X          X
;      /          / \        / \        / \  
;     V          V  |       V  |       V  |  
; => [**|**] => [**|**] => [**|**] => [**|**] => 
;     |          |          |          |        
;     V          V          V          V      
;     (D1)       (D1)       (D1)       (D1)

;wqzhang
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (make-deque) (cons '() '()))
(define (empty-deque? deque) 
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))
(define (print-deque deque)
  (define (collect q)
    (if (null? q)
        '()
        (cons (car q) (collect (cddr q)))))
  (display (collect (front-ptr deque)))
  (newline))
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-car! (cdr new-pair) (rear-ptr deque))
           (set-cdr! (cdr (rear-ptr deque)) new-pair)
           (set-rear-ptr! deque new-pair)))))
(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! (cdr new-pair) (front-ptr deque))
           (set-car! (cdr (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)))))
(define (front-deque deque)
  (if (empty-deque? deque)
      (display "error")
      (car (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (display "error")
      (car (rear-ptr deque))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (display "error"))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-front-ptr! deque (cddr (front-ptr deque)))
         (set-car! (cdr (front-ptr deque)) '()))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (display "error"))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-rear-ptr! deque (cadr (rear-ptr deque)))
         (set-cdr! (cdr (rear-ptr deque)) '()))))


(define q1 (make-deque))
(front-insert-deque! q1 'a)
(print-deque q1)
; (a)
(front-insert-deque! q1 'b)
(print-deque q1)
; (b a)
(rear-insert-deque! q1 'x)
(print-deque q1)
; (b a x)
(rear-insert-deque! q1 'y)
(print-deque q1)
; (b a x y)
(rear-delete-deque! q1)
(print-deque q1)
; (b a x)
(front-delete-deque! q1)
(print-deque q1)
; (a x)
(front-delete-deque! q1)
(print-deque q1)
; (x)
(front-delete-deque! q1)
(print-deque q1)
; ()
(empty-deque? q1)
;Value: #t

(define q2 (make-deque))
(rear-insert-deque! q2 1)
(print-deque q2)
; (1)
(front-insert-deque! q2 3)
(print-deque q2)
; (3 1)
(front-insert-deque! q2 5)
(print-deque q2)
; (5 3 1)
(front-deque q2)
;Value: 5
(rear-deque q2)
;Value: 1
(front-delete-deque! q2)
(print-deque q2)
; (3 1)
(front-deque q2)
;Value: 3
(rear-deque q2)
;Value: 1
(rear-delete-deque! q2)
(print-deque q2)
; (3)
(front-deque q2)
;Value: 3
(rear-deque q2)
;Value: 3
(empty-deque? q2)
;Value: #f
(rear-delete-deque! q2)
(print-deque q2)
; ()
(empty-deque? q2)
;Value: #t


;
; another implementation
;
; => [*][*] => [*][*] => [*][*] => [*][*] => [*][/]
;     |         |         |         |         |
;     V         V         V         V         V
;     (D1)      (D1)      (D1)      (D1)      (D1)
;        ^         ^         ^         ^         ^
;        |         |         |         |         |
;    [/][*] <= [/][*] <= [/][*] <= [/][*] <= [/][*] <= 
;
; NO that doesn't work :(

|#


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
#|
(insert! 'math '+ 10 t2)
t2
(insert! 'letters 'a 79 t2)
t2
(insert! 'letters 'c 99 t2)
t2
(insert! 'symbol '& 'and t2)
t2
|#


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
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (print-table)
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-table) print-table); for debugging
            (else (display "error"))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print-table (operation-table 'print-table))

#|
(put 'math '+ 43)
(put 'math '- 45)
(put 'math '* 42)
(put 'letters 'a 97)
(put 'letters 'b 98)
(put 'letters 'c 99)
(print-table)
(get 'math '+)
(get 'math '-)
(get 'math '*)
(get 'letters 'a)
(get 'letters 'b)
(get 'letters 'c)
|#


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

;; finally came up \w a solution :)
(define (make-table)
  (let ((local-table (list '*table*)))

    (define (asso key records)
      (cond 
        ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (asso key (cdr records)))))

    (define (lookup key-list)
      (define (lookup-recur keys table)
        (let ((subtable (asso (car keys) (cdr table))))
          (if subtable
              (if (null? (cdr keys))
                  (cdr subtable)
                  (lookup-recur (cdr keys) subtable))
              #f)))
      (lookup-recur key-list local-table))

    (define (insert! key-list value)
      (define (make-entry keys)
        (if (null? (cdr keys))
            (cons (car keys) value)
            (list (car keys) (make-entry (cdr keys)))))
      (define (insert-recur keys table)
        (let ((subtable (asso (car keys) (cdr table))))
          (cond ((eq? subtable #f)
                 (set-cdr! table
                           (cons (make-entry keys) (cdr table))))
                ((not (list? (cdr subtable)))
                 (set-cdr! subtable (cdr (make-entry keys))))
                (else 
                 (if (null? (cdr keys))
                     (set-cdr! subtable value)
                     (insert-recur (cdr keys) subtable))))))

      (insert-recur key-list local-table)
      'ok)

    (define (print-table)
      local-table)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-table) print-table)
            (else (display "error"))))
    dispatch))

#|
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print-table (operation-table 'print-table))


(put '(letters lower a) 97)
(print-table);(*table* (letters (lower (a . 97))))


(put '(letters) 'letters);; overwrite 3D with 1D
(print-table);(*table* (letters . letters))

(put '(letters lower a) 97);; overwrite 1D with 3D
(print-table);(*table* (letters (lower (a . 97))))


(put '(letters) 'letters);; overwrite 3D with 1D
(print-table);(*table* (letters . letters))

(put '(letters lower a) 97);; overwrite 1D with 3D
(print-table);(*table* (letters (lower (a . 97))))



(get '(letters));((lower (a . 97)))
(get '(letters lower));((a . 97))
(get '(letters lower a));97

(put '(letters lower a) 97)
(put '(letters lower b) 98)
(put '(letters lower c) 99)
(put '(letters upper A) 63)
(put '(letters upper B) 64)
(put '(letters upper C) 65)
(put '(math +) 43)
(put '(math -) 45)
(put '(math *) 42)
(print-table)
;(*table* (math (* . 42) (- . 45) (+ . 43))
;         (letters (upper (c . 65) (b . 64) (a . 63))
;                  (lower (c . 99) (b . 98) (a . 97))))
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

(define (asso key records)
  (cond ((null? records) false)
        ((equal? key (get-key records)) (get-value records))
        ((< key (get-key records)) (asso key (get-left records)))
        (else (asso key (get-right records)))))
  
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
            (let ((found (asso (car keys) records)))
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


'(exercise 3 27)#|
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
|#



;; 3.3.4 simulator for digital circuits

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; code in this section are reorganized to make it work
; so the order is different from that in the book
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; wires
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (display "unknown operation - wires"))))
    dispatch))


(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin 
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


;; logical relationship
(define (logical-not s)(cond ((= s 0) 1)((= s 1) 0)(else (display "error"))))
(define (logical-and s1 s2) (and s1 s2))
(define (logical-or s1 s2) (or s1 s2))

;; agenda - i think of it as (clock with seq of funcs)
;(make-agenda)
;(empty-agenda? <agenda>)
;(first-agenda-item <agenda>)
;(remove-first-agenda-item! <agenda>)
;(add-to-agenda! <time> <action> <agenda>)
;(current-time <agenda>)

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda)(cdr agenda))

(define (set-segments! agenda segments) (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action)
                                       (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action) 
                                    segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (display "Agenda empty - first-agenda-item")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

; behavior; add action to agenda
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))


;; run funcs in queue of agenda till its empty
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; 
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))




; a behavior that connect wires
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

; a behavior that connect wires
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; a behavior that connect wires
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; a behavior that connect wires
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; a behavior that connect wires
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;(probe 'sum sum)
; sum 0 New-value = 0

;(probe 'carry carry)
; carry 0 New-value = 0

;(half-adder input-1 input-2 sum carry)
;ok

;(set-signal! input-1 1)
;done

;(propagate)
;sum 8 New-value = 1
;done

;(set-signal! input-2 1)
;done

;(propagate)
;carry 11 New-value = 1
;sum 16 New-value = 0
;done


(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

;(or-gate a b d)
;(and-gate a b c)
;(inverter c e)
;(and-gate d e s)



;(get-signal <wire>)

;(set-signal! <wire> <new value>)

;(add-action! <wire> <proc>)

;(after-delay <delay> <proc>)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)


(define (logical-not s)
  (cond
    ((= s 0) 1)
    ((= s 1) 0)
    (else (display "error"))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)



'(exercise 3 28)
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


'(exercise 3 29)
;DeMorgan Law? a|b = !( (!a) & (!b) ) look up what it is later
;
;             +---+
; a -->INV--> |   |
;             |AND|-->INV--> output
; b -->INV--> |   |
;             +---+
;
; time delay is 2 inv + 1 and
;
#|
(define (or-gate a1 a2 output)
  (let ((w1 (wire))
        (w2 (wire))
        (o (wire)))
    (inverter a1 w1)
    (inverter a2 w2)
    (and-gate w1 w2 o)
    (inverter o output)))
|#

'(exercise 3 30)
(define (ripple-carry-adder a b s c)
  (let ((c-in (make-wire)))
    (if (null? (cdr a))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))
    (full-adder (car a) (car b) c-in (car s) c)))

;; delay proportional to n


'(exercise 3 31)#|
;Explain why accept-action-procedure is written like this.
(define (accept-action-procedure! proc)
  (set! action-procedures
        (cons proc action-procedures))
  (proc))

;instead of 
(define (accept-action-procedure! proc)
  (set! action-procedures
        (cons proc action-procedures)))

bcs, i have to trigger it to let it cascade. 
or it will be in equilibrium (all zeros)

|#


'(exercise 3 32)
; obviously temperal order matters
; use and-gate as example
; (and-gate a1 a2 output)
; changing from {a1=0, a2=1} to {a1=1, a2=0} 
; changing two input a1 a2 simultaneously
; the transition might be (1) {a1=0, a2=1} to {a1=1, a2=1} to {a1=1, a2=0}
;                      or (2) {a1=0, a2=1} to {a1=0, a2=0} to {a1=1, a2=0}
; depending on which wire (a1 a2) add-action-procedure first

; in the first case (a1 add-action-procedures first)
; {a1=0, a2=1} to {a1=1, a2=1} register callback1 that set output to 1
; {a1=1, a2=1} to {a1=1, a2=0} register callback2 that set output to 0
; execute callback1 first, then callback2, we got output 0, correct
; execute callback2 first, then callback1, we got output 1, wrong
; therefore order matters and we need queue (fifo buffer) to maintain them



;; 3.3.5 propagation of constraints

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; code in this section are reorganized to make it work
; so the order is different from that in the book
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









;(has-value? <connector>)
;(get-value  <connector>)
;(set-value! <connector> <new-value> <informant>)
;(forget-value! <connector> <retractor>)
;(connect <connector> <new-constraint>)




(define (for-each-except exception proc list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (proc (car items))
                (loop (cdr items)))))
  (loop list))


(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond
        ((not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except setter
                          inform-about-value
                          constraints))
        ((not (= value newval))
         (display "contradiction"))
        (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond
        ((eq? request 'has-value?) (if informant #t #f))
        ((eq? request 'value) value)
        ((eq? request 'set-value!) set-my-value)
        ((eq? request 'forget) forget-my-value)
        ((eq? request 'connect) connect)
        (else (display "unknown operation"))))
    me))


(define (has-value? connector) (connector 'has-value?))

(define (get-value connector) (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
       (set-value! sum
                   (+ (get-value a1) (get-value a2))
                   me))
      ((and (has-value? a1) (has-value? sum))
       (set-value! a2
                   (- (get-value sum) (get-value a1))
                   me))
      ((and (has-value? a2) (has-value? sum))
       (set-value! a2
                   (- (get-value sum) (get-value a2))
                   me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
       (display "unknown request"))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)


(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      ((or (and (has-value? m1) (= (get-value m1) 0))
           (and (has-value? m2) (= (get-value m2) 0)))
       (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2))
       (set-value! product
                   (* (get-value m1) (get-value m2))
                   me))
      ((and (has-value? product) (has-value? m1))
       (set-value! m2
                   (/ (get-value product)
                      (get-value m1))
                   me))
      ((and (has-value? product) (has-value? m2))
       (set-value! m1
                   (/ (get-value product)
                      (get-value m2))
                   me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
       (display "unknown request"))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)


(define (constant value connector)
  (define (me request)
    (display "unknown request"))
  (connect connector me)
  (set-value! connector value me)
  me)


(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
       (display "unknown request"))))
  (connect connector me)
  me)


(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))


(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))


(define C (make-connector))
(define F (make-connector))

;(celsius-fahrenheit-converter C F)

;(probe "Celsius temp" C)
;(probe "Fahrenheit temp" F)


;; testing

;(set-value! C 25 'user)
;Probe: Celsius temp = 25
;Probe: Fahrenheit temp = 77
;done

;(set-value! F 212 'user)
;Error! Contradiction (77 212)

;(forget-value! C 'user)
;Probe: Celcius temp = ?
;Probe: Fahrenheit temp = ?
;done

;(set-value! F 212 'user)
;Probe: Celcius temp = 212
;Probe: Fahrenheit temp = 100
;done


'(exercise 3 33)

;; reuse adder and multiplier
(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)
    'ok))

;; create new one
(define (averager a b c)
  (define (process-new-value)
    (cond
      ((and (has-value? a) (has-value? b))
       (set-value! c
                   (+ (get-value a) (get-value b))
                   me))
      ((and (has-value? a) (has-value? c))
       (set-value! b
                   (- (* 2 (get-value c)) (get-value a))
                   me))
      ((and (has-value? a2) (has-value? sum))
       (set-value! a
                   (- (* 2 (get-value c)) (get-value b))
                   me))))
  (define (process-forget-value)
    (forget-value! c me)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
       (display "unknown request"))))
  (connect a me)
  (connect b me)
  (connect c me)
  me)


'(exercise 3 34)
; the square constraint is unaware that a & a are identical
; if set b to 4
; nothing happens


'(exercise 3 35)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 - SQUARER"
                   (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (square (get-value a))
                        me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value)
       (process-new-value))
      ((eq? request 'I-lost-my-value)
       (process-forget-value))
      (else
       (display "unknown request"))))
  (connect a me)
  (connect b me)
  me)



'(exercise 3 36)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;           +------------------------------------------------------------+
;           | for-each-except: ...        inform-about-value:...         |
; global--> | b:----------------------------+                            |
; env       | a:--+                         |                            |
;           +-----|-------------^-----------|-------------^-----------^--+
;                 |             |           |             |           |
;                 V     +-------^-------+   V     +-------^-------+   |
;              [*][*]   |value:10       | [*][*]  |init:100       |   |
;               |  |    |informant:'user|  |  |   |informant:false|   |
;               |  |    |cstrnts:'()    |  |  |   |cstrnts:'()    |   |
;               |  |    |set-my-val:... |  |  |   |set-my-val:... |   |
;               |  |    |               |  |  |   |               |   |
;               |  |    +--^----^-------+  |  |   +-------^-------+   |
;               |  |       |    |          |  |           |           |
;               |  +-------+    |          |  +-----------+           |
;               V               |          V                          |
;  params:request               |       params:request   +------------^--+
;  body:body of me     +--------^---+   body:body of me  |connector: a   |
;                      |newval:10   |                    |new-value:10   |
;                      |setter:'user|                    |informant:'user|
;                      +--------^---+                    +---------------+
;                               |call to set-my-value   call to set-value!
;                               | 
;                  +------------^---+
;                  |exception:setter|                                 
;                  |proc:ifm-abt-val|   
;                  |list:constraints|
;                  +----------------+
;                 call to for-each-except
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


'(exercise 3 37)

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.4 Concurrency: Time Is of the Essence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(exercise 3 38)

; Peter +10
; Paul  -20
; Mary  -1/2

;(a)
; Peter Paul  Mary  45
; Paul  Peter Mary  45
; Peter Mary  Paul  35
; Paul  Mary  Peter 50
; Mary  Peter Paul  40
; Mary  Paul  Peter 40

;(b)
; $30 $55 $60 $80 $90 $110

; $30
; Peter R 100  Mary  R 100
; Peter W 110
; Mary  W 50
; Paul  R 50
; Paul  W 30

; $55
; Peter R 100
; Peter W 110
; Mary  R 110  Paul  R 110
; Paul  W 90
; Mary  W 55

; $60
; Mary  R 100
; Mary  W 50
; Peter R 50   Paul  R 50
; Paul  W 30
; Peter W 60

; $80
; Peter R 100  Mary  R 100  Paul  R 100
; Peter W 110
; Paul  W 50
; Mary  W 80

; $90
; Peter R 100
; Peter W 110
; Mary  R 110  Paul  R 110
; Mary  W 55
; Paul  W 90

; $110
; Peter R 100  Mary  R 100  Paul  R 100
; Paul  W 50
; Mary  W 80
; Peter W 110

(define (parallel-execute . args) (for-each 1thread args))

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))


(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (display "unknown request"))))
    dispatch))



'(exercise 3 39)

(define x 10)

(define s (make-serializer))

(parallel-execute
 (lambda () (set! x ((s (lambda () (* x x))))))
 (s (lambda () (set! x (+ x 1)))))

; 100 is possible

; P1 read x=10
; P2 rw x=11
; P1 write x=100;



'(exercise 3 40)

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))


;1000000
;P[1] sets x to 100 and then P[2] sets x to 1000000
;P[2] sets x to 1000 and then P[1] sets x to 1000000

;100000
;P[2] accesses x once before P[1] set x to 100
;    then P[2] accesses x twice, then P[2] sets x

;10000
;P[2] accesses x twice before P[1] set x to 100 
;    then P[2] accesses x once, then P[2] sets x
;P[2] sets x to 1000 between the two times that P[1]
;    accesses x, then P[1] sets x.

;1000
;P[1] and P[2] access x two and three times respectively, 
;    then P[1] sets x, then P[2] sets x

;100
;P[1] and P[2] access x two and three times respectively
;    then P[2] sets x, then P[1] sets x


'(exercise 3 41)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond
        ((eq? m 'withdraw) (protected withdraw))
        ((eq? m 'deposit) (protected deposit))
        ((eq? m 'balance) ((protected (lambda () balance))))
        (else (display "unknown request"))))
    dispatch))

;; unnecesary move; reading balance is already atomic

'(exercise 3 42)#|
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond
          ((eq? m 'withdraw) protected-withdraw)
          ((eq? m 'deposit) protected-deposit)
          ((eq? m 'balance) balance)
          (else (display "unknown request"))))
      dispatch)))


; no difference in terms of concurrency control
; but the environment model is different
; in that
; for the old version (protected withdraw) is eval'ed every time its called
;   so there are many copies of (protected withdraw) floating around
; for the new version (protected withdraw) is eval'ed only once 
;   so there is only one copy of (protected withdraw)

|#


(define (exchange account1 account2)
  (let ((difference (- (account1 'balance) (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)))
        (display "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (maker-serializer)))
    (define (dispatch m)
      (cond
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        ((eq? m 'balance) balance)
        ((eq? m 'serializer) balance-serializer)
        (else (display "unknown request"))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))



'(exercise 3 43)

; because its exchange operation
; imagine swapping these 3 accounts

;; both can happen
;; exchange a1 a2 first, then a1 a3
; a1 a2 a3
; 10 20 30
; 20 10 30
; 30 10 20

;; exchange a1 a3 first, then a1 a2
; a1 a2 a3
; 10 20 30
; 30 20 10
; 20 30 10


'(exercise 3 44)
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

; Louis Reasoner is wrong. Ben's version will work.
; exchange need to be exclusive(start exchange after prev exchange finishes)
; transfer is not, because transfer commutative
; receive 10 from A then receive 20 from B is equivalent to
; receive 20 from B then receive 10 from A
; start a transfer to account1 while it's in another transfer is totally ok


'(exercise 3 45)
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (display "unknown request"))))
    dispatch))

(define (deposit account amount)
  ((account 'deposit) amount))

; serialized-change will call (account1 'withdraw) and (account2 'deposit)
; locks of account1 and account2 re already acquired by serialized-exchange
; when (account1 'withdraw) and (account2 'deposit) try to acquire them again
; they will keep busy waiting and never stops












