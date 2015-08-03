(define (atom? x)
  (and (not (pair? x)) (not (null? x))))



(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat (cons (car lat) seen)))))
      (else
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat) seen)))))))

;(multirember&co 'tuna '(strawberries tuna and swordfish) (lambda (x y) x))


(define (multi&co a lat col)
  (cond
    ((null? lat)
     (col '() '()))
    ((eq? (car lat) a)
     (multi&co a (cdr lat) (lambda (new old)
                             (col new 
                                  (cons (car lat) old)))))
    (else
     (multi&co a (cdr lat) (lambda (new old)
                             (col (cons (car lat) new)
                                  old))))))

;(multi&co 'tuna '(strawberries tuna and swordfish) (lambda (x y) x))


(define (f&co nums col)
  (cond
    ((null? nums) (col '() '()))
    ((even? (car nums))
     (f&co (cdr nums) (lambda (evens odds)
                        (col (cons (car nums) evens) odds))))
    (else
     (f&co (cdr nums) (lambda (evens odds)
                        (col evens (cons (car nums) odds)))))))

;(f&co '(1 2 3 4 5) (lambda (x y) x))


(define (op exp) (car exp))
(define (1se exp) (cadr exp))
(define (2se exp) (caddr exp))

(define expression '(+ (^ 2 3) (/ 10 2)))

;(op expression)
;(1se expression)
;(2se expression)


(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (op nexp) '+) (+ (value (1se nexp)) (value (2se nexp))))
    ((eq? (op nexp) '-) (- (value (1se nexp)) (value (2se nexp))))
    ((eq? (op nexp) '*) (* (value (1se nexp)) (value (2se nexp))))
    ((eq? (op nexp) '/) (/ (value (1se nexp)) (value (2se nexp))))
    ((eq? (op nexp) '^) (expt (value (1se nexp)) (value (2se nexp))))))

;(value expression)


;; abstract out common pattern
(define (atom-to-func x)
  (cond
    ((eq? x '+) +)
    ((eq? x '-) -)
    ((eq? x '*) *)
    ((eq? x '/) /)
    ((eq? x '^) expt)))


(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    (else ((atom-to-func (op nexp))(value (1se nexp))(value (2se nexp))))))

;(value expression)

(define (eternity x) (eternity x))

(define (inc x) (+ 1 x))


(lambda (l)
  (cond
    ((null? l) 0)
    (else (inc (eternity (cdr l))))))

(lambda (l)
  (cond
    ((null? l) 0)
    (else (inc ((lambda (l)
                  (cond
                    ((null? l) 0)
                    (else (inc (eternity (cdr l))))))
                (cdr l))))))

(lambda (l)
  (cond
    ((null? l) 0)
    (else (inc ((lambda (l)
                  (cond
                    ((null? l) 0)
                    (else (inc ((lambda (l)
                                  (cond
                                    ((null? l) 0)
                                    (else (inc (eternity (cdr l))))))
                                (cdr l))))))
                (cdr l))))))


;; len <= 1
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (inc (length (cdr l)))))))
 eternity)

;; len <= 2
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (inc (length (cdr l)))))))
 ((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (inc (length (cdr l)))))))
 eternity))


; len <= 3
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (inc (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (inc (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (inc (length (cdr l)))))))
   eternity)))


;; len<=1
((lambda (mk-len)
   (mk-len eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (inc (length (cdr l))))))))


;; len<=2
((lambda (mk-len)
   (mk-len (mk-len eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (inc (length (cdr l))))))))


;; len<=3
((lambda (mk-len)
   (mk-len (mk-len (mk-len eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (inc (length (cdr l))))))))






