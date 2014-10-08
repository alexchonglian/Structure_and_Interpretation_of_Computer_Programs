#lang racket

(define calc
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [`(,op, e1, e2) (let ([v1 (calc e1)]
                            [v2 (calc e2)])
                        (match op
                          ['+ (+ v1 v2)]
                          ['- (- v1 v2)]
                          ['* (* v1 v2)]
                          ['/ (/ v1 v2)]))])))

(calc '(+ 1 2))
(calc '(- (* 5 3) (/ 6 2)))


(((lambda (x) (lambda (y) (/ x y))) 6) 3)
;; we can name it f

(define f (lambda (x) (lambda (y) (/ x y))))
((f 6) 3)



;; interpreter for lambda calculus
;; 3 def for environment: env0, ext-env, lookup

;; empty environment
(define env0 '())

;; extend the environment
(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
        [(not p) x]
        [else (cdr p)]))))

;; definition of closure: function f and its env
(struct Closure (f env))

;; recursive definition for interpreter 
;; accept 2 params: expression and environment
;; 5 conditions: variable, function, call, digit, expression
(define interp1
  (lambda (exp env)
    (match exp
      [(? symbol? x) (lookup x env)]         ;; variable
      [(? number? x) x]                      ;; digit
      [`(lambda (,x) ,e) (Closure exp env)]  ;; function
      [`(,e1 ,e2)                            ;; call
       (let ([v1 (interp1 e1 env)]
             [v2 (interp1 e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env1)
            (interp1 e (ext-env x v2 env1))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interp1 e1 env)]
             [v2 (interp1 e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))

;; interface of interpreter
(define interp
  (lambda (exp)
    (interp1 exp env0)))

(interp '(+ 1 2))
;; => 3

(interp '(* 2 3))
;; => 6

(interp '(* 2 (+ 3 4)))
;; => 14

(interp '(* (+ 1 2) (+ 3 4)))
;; => 21

(interp '(((lambda (x) (lambda (y) (* x y))) 2) 3))
;; => 6

(interp '((lambda (x) (* 2 x)) 3))
;; => 6

(interp '((lambda (y) (((lambda (y) (lambda (x) (* y 2))) 3) 0)) 4))
;; => 6

;(interp '(1 2))
;; => match: no matching clause for 1


