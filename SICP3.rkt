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
                     (lambda (x) (cons num-fail-login '(times failed)))))))
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














;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3.2 The Environment Model of Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

