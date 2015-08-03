
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;(atom? '())
;(eq? 'a 'a)
;(eq? 6 6)


(define (lat? l)
  (cond 
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

;(lat? '(beacon and eggs));#t

(define (member? a lat)
  (cond
    ((null? lat) #f)
    (else (or (eq? (car lat) a)
              (member? a (cdr lat))))))

;(member? 'tea '(coffee tea and milk));#t

(define (rember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a) (cdr lat))
    (else (cons (car lat) (rember a (cdr lat))))))

;(rember 'cup '(coffee cup tea cup and hick cup));(tea cup and hick cup)

(define (firsts l)
  (cond 
    ((null? l) '())
    (else (cons (caar l) (firsts (cdr l))))))

;(firsts '((a b c) (1 2 3) (x y z)))

(define (seconds l)
  (cond 
    ((null? l) '())
    (else (cons (cadar l) (seconds (cdr l))))))

;(seconds '((a b c) (1 2 3) (x y z)))

(define (insertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons old (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat))))))

;(insertR 'new 'old '(old what old what))

(define (insertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons new (cons old (cdr lat))))
    (else (cons (car lat) (insertL new old (cdr lat))))))

;(insertL 'new 'old '(old what old what))

(define (subst new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat))))))

;(subst 'new 'old '(old what old what))

(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) o1) (cons new (cdr lat)))
    ((eq? (car lat) o2) (cons new (cdr lat)))
    (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat) '())
    ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
    (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

;(subst2 'new 'o1 'o2 '(o1 what o2 what))

(define (multirember a lat)
  (cond 
    ((null? lat) '())
    ((eq? (car lat) a) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat))))))

;(multirember 'old '(old what old what))

(define (multiinsertR new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
    (else (cons (car lat) (multiinsertR new old (cdr lat))))))

;(multiinsertR 'new 'old '(old what old what))

(define (multiinsertL new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
    (else (cons (car lat) (multiinsertL new old (cdr lat))))))

;(multiinsertL 'new 'old '(old what old what))

(define (multisubst new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
    (else (cons (car lat) (multisubst new old (cdr lat))))))

;(multisubst 'new 'old '(old what old what))

(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))

(define (o+ n m)
  (cond
    ((zero? m) n)
    (else (add1 (o+ n (sub1 m))))))

;(o+ 5 6)

(define (o- n m)
  (cond 
    ((zero? m) n)
    (else (sub1 (o- n (sub1 m))))))

;(o- 11 5)

(define (sum tup)
  (cond
    ((null? tup) 0)
    (else (+ (car tup) (sum (cdr tup))))))

;(sum '(1 2 3 4))

(define (o* n m)
  (cond
    ((zero? m) 0)
    (else (+ n (o* n (sub1 m))))))

;(o* 4 5)

(define (tup+ t1 t2)
  (cond
    ((null? t1) t2)
    ((null? t2) t1)
    (else (cons (+ (car t1) (car t2))
                (tup+ (cdr t1) (cdr t2))))))

;(tup+ '(1 2 3) '(4 5 6))

(define ^ expt)

;(^ 3 4)

;(length '(1 2 3 4))

(define (pick n lat)
  (cond
    ((zero? (sub1 n)) (car lat))
    (else (pick (sub1 n) (cdr lat)))))

;(pick 3 '(1 2 3 4))


(define (rempick n lat)
  (cond 
    ((zero? (sub1 n)) (cdr lat))
    (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

;(rempick 3 '(1 2 3 4))

(define (no-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (no-nums (cdr lat)))
    (else (cons (car lat) (no-nums (cdr lat))))))

;(no-nums '(a 1 b 2))

(define (all-nums lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
    (else (all-nums (cdr lat)))))

;(all-nums '(a 1 b 2))


(define (occur a lat)
  (cond
    ((null? lat) 0)
    ((eq? (car lat) a) (add1 (occur a (cdr lat))))
    (else (occur a (cdr lat)))))

;(occur 1 '(1 2 1 2 3))


(define (rember* a l)
  (cond
    ((null? l) '())
    ((atom? (car l))(cond
                      ((eq? (car l) a) (rember* a (cdr l)))
                      (else (cons (car l) (rember* a (cdr l))))))
    (else (cons (rember* a (car l)) 
                (rember* a (cdr l))))))

;(rember* 'a '(a b (a b (a))))

(define (insertR* n o l)
  (cond
    ((null? l) '())
    ((atom? (car l))(cond
                      ((eq? (car l) o) (cons o (cons n (insertR* n o (cdr l)))))
                      (else (cons (car l) (insertR* n o (cdr l))))))
    (else (cons (insertR* n o (car l))
                (insertR* n o (cdr l))))))

;(insertR* 'c 'a '(a b (a b (a))))

(define (insertL* n o l)
  (cond
    ((null? l) '())
    ((atom? (car l))(cond
                      ((eq? (car l) o) (cons n (cons o (insertL* n o (cdr l)))))
                      (else (cons (car l) (insertL* n o (cdr l))))))
    (else (cons (insertL* n o (car l))
                (insertL* n o (cdr l))))))

;(insertL* 'c 'a '(a b (a b (a))))

(define (occur* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l))(cond
                      ((eq? (car l) a) (add1 (occur* a (cdr l))))
                      (else (occur* a (cdr l)))))
    (else (+ (occur* a (car l))
             (occur* a (cdr l))))))

;(occur* 'a '(a b (a b (a))))

(define (subst* n o l)
  (cond
    ((null? l) '())
    ((atom? (car l))(cond
                      ((eq? (car l) o) (cons n (subst* n o (cdr l))))
                      (else (cons (car l) (subst* n o (cdr l))))))
    (else (cons (subst* n o (car l))
                (subst* n o (cdr l))))))

;(subst* 'c 'a '(a b (a b (a))))

(define (member* a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))(or (eq? (car l) a)
                        (member* a (cdr l))))
    (else (or (member* a (car l))
              (member* a (cdr l))))))

;(member* 'c '(a b (a b (a (c)))))
;(member* 'c '(a b (a b (a ( )))))

(define (leftmost l)
  (cond
    ((atom? (car l)) (car l))
    (else (leftmost (car l)))))

(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2))) (and (eq? (car l1) (car l2))
                                                  (eqlist? (cdr l1) (cdr l2))))
    ((or (atom? (car l1)) (atom? (car l2))) #f)
    (else (and (eqlist? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2))))))

;(eqlist? '(a b (a b (a)))  '(a b (a b (a))) )

#|
(define (equal? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2)) (eq? s2 s2))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist? s1 s2))))
|#

; equal? compares number, atom, list
;(equal? 1 1)
;(equal? 'a 'a)
;(equal? '(a b (a b (a)))  '(a b (a b (a))))


(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else (and (equal? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2))))))

;(eqlist? '(a b (a b (a)))  '(a b (a b (a))) )

(define (rember s l)
  (cond
    ((null? l) '())
    ((equal? (car l) s) (cdr l))
    (else (cons (car l)
                (rember s (cdr l))))))

;(rember '(a b (c ()))  '(0 1 (a b (c ()))) )

; TODO
;(rember* '(x(x(x)))  '(0 1 (a b (c (x(x(x)))))) )

(define (numbered? aexp)
  (cond
    ((atom? aexp) (number? aexp))
    ((eq? (car (cdr aexp)) '+) (and (numbered? (car aexp))
                                    (numbered? (car (cdr (cdr aexp))))))
    ((eq? (car (cdr aexp)) '-) (and (numbered? (car aexp))
                                    (numbered? (car (cdr (cdr aexp))))))
    ((eq? (car (cdr aexp)) '*) (and (numbered? (car aexp))
                                    (numbered? (car (cdr (cdr aexp))))))
    ((eq? (car (cdr aexp)) '/) (and (numbered? (car aexp))
                                    (numbered? (car (cdr (cdr aexp))))))
    (else #f)))

;(numbered? '(1 + (3 * 4)))
;(numbered? '(1 + (3 * a)))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (car (cdr nexp)) '+) (+ (value (car nexp))
                                  (value (car (cdr (cdr nexp))))))
    ((eq? (car (cdr nexp)) '-) (- (value (car nexp))
                                  (value (car (cdr (cdr nexp))))))
    ((eq? (car (cdr nexp)) '*) (* (value (car nexp))
                                  (value (car (cdr (cdr nexp))))))
    ((eq? (car (cdr nexp)) '/) (/ (value (car nexp))
                                  (value (car (cdr (cdr nexp))))))
    (else 0)))

;(value '(1 + (3 * 4)))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (car nexp) '+) (+ (value (car (cdr nexp)))
                            (value (car (cdr (cdr nexp))))))
    ((eq? (car nexp) '-) (- (value (car (cdr nexp)))
                            (value (car (cdr (cdr nexp))))))
    ((eq? (car nexp) '*) (* (value (car (cdr nexp)))
                            (value (car (cdr (cdr nexp))))))
    ((eq? (car nexp) '/) (/ (value (car (cdr nexp)))
                            (value (car (cdr (cdr nexp))))))
    (else 0)))

;(value '(+ 1 (* 3 4)))

(define (operator aexp) (car aexp))
(define (1st-sub-exp aexp) (car (cdr aexp)))
(define (2nd-sub-exp aexp) (car (cdr (cdr aexp))))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp))
                                 (value (2nd-sub-exp nexp))))
    ((eq? (operator nexp) '-) (- (value (1st-sub-exp nexp))
                                 (value (2nd-sub-exp nexp))))
    ((eq? (operator nexp) '*) (* (value (1st-sub-exp nexp))
                                 (value (2nd-sub-exp nexp))))
    ((eq? (operator nexp) '/) (/ (value (1st-sub-exp nexp))
                                 (value (2nd-sub-exp nexp))))
    (else 0)))

;(value '(+ 1 (* 3 4)))

(define (set? lat)
  (cond
    ((null? lat) #t)
    ((memq (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))

;(set? '(1 2 3))
;(set? '(1 2 2 3))

(define (makeset lat)
  (cond
    ((null? lat) '())
    ((memq (car lat) (cdr lat)) (makeset (cdr lat)))
    (else (cons (car lat) (makeset (cdr lat))))))

;(makeset '(1 2 2 3))

(define (subset? s1 s2)
  (cond
    ((null? s1) #t)
    ((memq (car s1) s2) (subset? (cdr s1) s2))
    (else #f)))

;(subset? '(1 2) '(1 2 3))

(define (eqset? s1 s2)
  (and (subset? s1 s2)
       (subset? s2 s1)))

(define (intersect? s1 s2)
  (cond
    ((null? s1) #f)
    ((memq (car s1) s2) #t)
    (else (intersect? (cdr s1) s2))))

;(intersect? '(1 2 3) '(3 4 5))
;(intersect? '(1 2 3) '(4 5))

(define (intersect s1 s2)
  (cond
    ((null? s1) '())
    ((memq (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
    (else (intersect (cdr s1) s2))))

;(intersect '(1 2 3) '(3 4 5))
;(intersect '(1 2 3) '(4 5))

(define (union s1 s2)
  (cond
    ((null? s1) s2)
    ((memq (car s1) s2) (union (cdr s1) s2))
    (else (cons (car s1) (union (cdr s1) s2)))))

;(union '(1 2 3) '(3 4 5))
;(union '(1 2 3) '(4 5))

(define (difference s1 s2)
  (cond
    ((null? s1) '())
    ((memq (car s1) s2) (difference (cdr s1) s2))
    (else (cons (car s1)
                (difference (cdr s1) s2)))))


;(difference '(1 2 3) '(3 4 5))
;(difference '(1 2 3) '(4 5))

(define (intersectall l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else (intersect (car l-set)
                     (intersectall (cdr l-set))))))


(define (a-pair? x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f)))

(define (first p) (car p))
(define (second p) (cadr p))
(define (build s1 s2) (cons s1 (cons s2 '())))

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else (cons (build (second (car rel))
                       (first (car rel)))
                (revrel (cdr rel))))))

;(revrel '((1 2) (a b) (x y)))

(define (revpair pair)
  (build (second pair) (first pair)))

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else (cons (revpair (car rel))
                (revrel (cdr rel))))))

;(revrel '((1 2) (a b) (x y)))

(define (fullfun? fun)
  (set? (seconds fun)))

;(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))














