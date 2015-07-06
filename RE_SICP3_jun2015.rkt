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
      "Insufficient funds"))
#|
(withdraw 25);75
(withdraw 25);50
(withdraw 60);Insufficient funds
(withdraw 15);35
|#


(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient funds"))))
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
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
#|
(W1 50);50
(W2 70);30
(W2 40);Insufficient funds
(W1 40);10
|#


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
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
((acc 'withdraw) 50);50
((acc 'withdraw) 60)
((acc 'deposit) 40);90
((acc 'withdraw) 60);30
|#

'(exercise 3 1)#|
(define (make-accumulator init)
  (lambda (inc)
    (begin (set! init (+ inc init)) init)))

(define A (make-accumulator 5))
(A 10)
(A 10)
|#


'(exercise 3 2)
#|
;single params
(define (make-monitor f)
  (let ((counter 0))
    (lambda (arg)
      (set! counter (+ 1 counter))
      (f arg))))
(define s (make-monitor sqrt))
(s 100)

;multiple params
(define (make-monitor f)
  (let ((counter 0))
    (define (monitor . args)
      (set! counter (+ 1 counter))
      (apply f args))
    monitor))
(define add (make-monitor +))
(add 1 2 3 4 5 6 7 8 9)
(s 'how-many-calls?)

;add how-many-calls? and reset-count!
(define (make-monitor f)
  (let ((counter 0))
    (define (monitor . args)
      (cond ((eq? (car args) 'how-many-calls?) counter)
            ((eq? (car args) 'reset-count!) (begin (set! counter 0) 
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

;message passing
(define (make-monitor f)
  (let ((counter 0))
    (define (monitor . args)
      (set! counter (+ 1 counter))
      (apply f args))
    (define (how-many-calls?) counter)
    (define (reset-count!) (set! counter 0) counter)
    (define (dispatch m)
      (cond
        ((eq? m 'how-many-calls?) (how-many-calls?))
        ((eq? m 'reset-count!) (reset-count!))
        ((eq? m 'call) monitor)
        (else (display "error"))))
    dispatch))
(define add (make-monitor +))
((add 'call) 1 2 3 4 5 6 7 8 9);45
((add 'call) 1 2 3 4 5);15
((add 'call) 6 7 8 9);30
(add 'how-many-calls?);3
(add 'reset-count!);0
((add 'call) 1 2 3 4);10
(add 'how-many-calls?);1

;remove begin
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



(define (make-accumulator acc)
  (lambda (x)
    (set! acc (+ acc x))
    acc))
(define (make-monitored f)
  (define calls (make-accumulator 0))
  (define (reset) (calls (- (calls 0))))
  (define (mf a)
    (cond ((eq? a 'how-many-calls?) (calls 0))
          ((eq? a 'reset-count) reset)
          (else (calls 1) (f a))))
  mf)
(define s (make-monitored sqrt)) 
(s 100); 10 
(s 144); 12 
(s 'how-many-calls?);2 
|#


'(exercise 3 3)#|
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Ins fund"))
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
          "Ins fund"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cop) "call the cop")
    (define (dispatch pswd-attempt m)
      (if (eq? pswd-attempt password)
          (begin (set! num-fail-login 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (lambda (x) "wrong dispatcher"))))
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
      (set! x (rand-upadte x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monto-carlo trials cesaro-test))))

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
  (define (iter trails-remaining trials-passed x)
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

(estimate-integral P 2.0 8.0 4.0 10.0 100) 
|#


'(exercise 3 6)#|
(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x)) x))
            ((eq? m 'reset)
             (lambda (new-value) (set! x new-value)))))
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


(define (make-joint account pw-old pw-new)
  (lambda (pw-new-attempt msg)
    (if (eq? pw-new-attempt pw-new)
        (account pw-old msg)
        (lambda (x) "Incorrect pw"))))


(define jacc (make-joint acc 'secret 'new-secret))

((acc 'secret 'withdraw) 40);60
((jacc 'new-secret 'withdraw) 40);20
((acc 'secret 'deposit) 10);30
((jacc 'new-secret 'withdraw) 40);"insufficient fund"
((jacc 'guess 'withdraw) 40);"incorrect password"
|#



'(exercise 3 10)

;(let ((var exp)) body)
;((lambda (var) body) exp)

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


(define (sqrt! x)
  (define (good? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insuf fund"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (lambda (x) "error"))))
  dispatch)

#|
(define acc (make-account 50))
((acc 'deposit) 40);90
((acc 'withdraw) 60);30
|#


#|
(define (append! x y)
  (if (null? x)
      y
      (cons (car x) (append! (cdr x) y))))
|#

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))


(define (cons! x y)
  (define (dispatch m)
    (cond
      ((eq? m 'car) x)
      ((eq? m 'cdr) y)
      (else (display "error"))))
  dispatch)


(define (cons! x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond
      ((eq? m 'car) x)
      ((eq? m 'cdr) y)
      ((eq? m 'set-car!) set-x!)
      ((eq? m 'set-cdr!) set-y!)
      (else (display "error"))))
  dispatch)


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
      (display "error")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue) (set-front-ptr! queue new-pair)
                                (set-rear-ptr! queue new-pair)
                                queue)
          (else (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue) (display "error"))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

#|
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)
|#


;; proc \w local state
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (front-queue)
      (if (empty-queue?)
          (display "error")
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
             (display "error"))
            (else (set-front-ptr! (cdr front-ptr)))))
    (define (print-queue) front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'empty-queue) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (display "error"))))
    dispatch))

(define (print-queue queue) (car queue))




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
  (collect (front-ptr deque)))
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque))
          (else
           (set-car! (cdr new-pair) (rear-ptr deque))
           (set-cdr! (cdr (rear-ptr deque)) new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque)))))
(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque))
          (else
           (set-cdr! (cdr new-pair) (front-ptr deque))
           (set-car! (cdr (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)
           (print-deque deque)))))
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
         (set-rear-ptr! deque '())
         (print-deque deque))
        (else
         (set-front-ptr! deque (cddr (front-ptr deque)))
         (set-car! (cdr (front-ptr deque)) '())
         (print-deque deque))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (display "error"))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-rear-ptr! deque (cadr (rear-ptr deque)))
         (set-cdr! (cdr (rear-ptr deque)) '())
         (print-deque deque))))

#|
(define q1 (make-deque))
(front-insert-deque! q1 'a)
; (a)
(front-insert-deque! q1 'b)
; (b a)
(rear-insert-deque! q1 'x)
; (b a x)
(rear-insert-deque! q1 'y)
; (b a x y)
(rear-delete-deque! q1)
; (b a x)
(front-delete-deque! q1)
; (a x)
(front-delete-deque! q1)
; (x)
(front-delete-deque! q1)
; ()
(empty-deque? q1)
; #t

(define q2 (make-deque))
(rear-insert-deque! q2 1)
; (1)
(front-insert-deque! q2 3)
; (3 1)
(front-insert-deque! q2 5)
; (5 3 1)
(front-deque q2)
; 5
(rear-deque q2)
; 1
(front-delete-deque! q2)
; (3 1)
(front-deque q2)
; 3
(rear-deque q2)
; 1
(rear-delete-deque! q2)
; (3)
(front-deque q2)
; 3
(rear-deque q2)
; 3
(empty-deque? q2)
; #f
(rear-delete-deque! q2)
(print-deque q2)
; ()
(empty-deque? q2)
; #t
|#


; => [*][*] => [*][*] => [*][*] => [*][*] => [*][/]
;     |         |         |         |         |
;     V         V         V         V         V
;     (D1)      (D1)      (D1)      (D1)      (D1)
;        ^         ^         ^         ^         ^
;        |         |         |         |         |
;    [/][*] <= [/][*] <= [/][*] <= [/][*] <= [/][*] <= 


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
;(*table* (d . 4) (a . 1) (b . 2) (c . 3))

(define t2 '(*table* 
             (math (+ . 43) (- . 45) (* . 42)) 
             (letters (a . 97) (b . 98))))

(define (make-table) (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (asso key-1 (cdr table))))
    (if subtable
        (let ((record (asso key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

;(lookup 'math '+ t2)

(define (insert! key-1 key-2 value table)
  (let ((subtable (asso key-1 (cdr table))))
    (if subtable
        (let ((record (asso key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
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


