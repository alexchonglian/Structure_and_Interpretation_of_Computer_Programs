;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The Little Schemer - 4th Edition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Preface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Chapter 1 - Toys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(atom? 'a)
(list? '(1 2 3))
(car '(1 2 3))
(car '(((hotdogs)) (and) (pickle) relish))
(car (car '(((hotdog)))))
(cdr '(1 2 3))
(cdr '(sth))
(cons 'peanut '(jelly and butter))
(cons '(peanut and ) '(jelly and butter))
(cons 'a '())
(define l '())
(null? l)
(null? '())
(atom? (car '(harry and a heap of apple)))
(eq? 'harry 'harry)
(eq? 1 2)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chapter 2 - Do It, Do It Again, and Again, and Again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lat?
  (lambda (l)
    (cond
      ((null? l)#t)
      ((atom? (car l))(lat? (cdr l)))
      (else #f))))

#|
(lat? '(a b c d))
(lat? '((a) b c d))
(null? '(()))
(or (null? '()) (atom? '(a b c)))
|#

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

;(member? 'meat '(fresh meat and peper))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chapter 3 - Cons the Magnificent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat))(cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))(cons old (cons new (cdr lat))))
      (else (cons (car lat)(insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))(cons new (cons old (cdr lat))))
      (else (cons (car lat)(insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))(cons new (cdr lat)))
      (else (cons (car lat)(subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? o1 (car lat))(eq? o2 (car lat)))
       (cons new (cdr lat)))
      (else (cons (car lat)(subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat)'())
      ((eq? a (car lat))(multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat)(multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat)(multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat)(multisubst new old (cdr lat)))))))


#|
(rember 'cup '(coffee cup and tea cup))
(firsts '(((a) (b) c) (1 2 3) (X Y Z)))
(seconds '(((a) (b) c) (1 2 3) (X Y Z)))
(insertR '* '3 '(1 2 3 4 5))
(insertL '* '3 '(1 2 3 4 5))
(subst '* '3 '(1 2 3 4 5))
(subst2 '* '2 '3 '(1 3 2 4 5))
(multirember 'cup '(coffee cup and tea cup and whatever cup))
(multiinsertR '* '3 '(1 2 3 4 5 4 3 2 1))
(multiinsertL '* '3 '(1 2 3 4 5 4 3 2 1))
(multisubst '* '3 '(1 2 3 4 5 4 3 2 1))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chapter 4 - Numbers Games
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

;(add1 67)
;(sub1 67)
;(zero? 0)

(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (plus n (sub1 m)))))))

(define minus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (minus n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup)(addtup (cdr tup)))))))

(define multiply
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (multiply n (sub1 m)))))))

(define tupplus
  (lambda (t1 t2)
    (cond
      ((and (null? t1)(null? t2))'())
      ((null? t1) t2)
      ((null? t2) t1)
      (else (cons (plus (car t1)(car t2))
             (tupplus (cdr t1)(cdr t2)))))))

(define exponent
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (multiply n (exponent n (sub1 m)))))))

(define divide
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (divide (- n m) m))))))

#|
(plus 2 3)
(minus 4 2)
(addtup '(1 2 3 4))
(multiply 2 3)
(tupplus '(1 2 3) '(3 2 1 1))
(exponent 2 3)
(quotient 4 2)
(divide 5 2)
|#

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)(rempick (sub1 n)(cdr lat)))))))

(define nonums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat))(nonums (cdr lat)))
      (else (cons (car lat)(nonums (cdr lat)))))))

(define alnums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat))(cons (car lat)(alnums (cdr lat))))
      (else (alnums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)(number? a2)) (= a1 a2))
      ((or (number? a1)(number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat))(add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

#|
(length '(1 2 3 4 5))
(len '(1 2 3 4 5 6 7 8))
(pick 6 '(1 2 3 4 5 6 7 8))
(rempick 6 '(1 2 3 4 5 6 7 8))
(nonums '(a 1 2 b 3 c))
(alnums '(a 1 2 b 3 c))
(= 1 1)
(occur 1 '(1 2 1 2 1))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chapter 5 - *Oh My Gawd*: It's Full of Stars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)(rember* a (cdr l)))
         (else (cons (car l)(rember* a (cdr l))))))
      (else (cons (rember* a (car l))(rember* a (cdr l)))))))

(define insertR*
  (lambda (n o l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) o)(cons o (cons n (insertR* n o (cdr l)))))
         (else (cons (car l) (insertR* n o (cdr l))))))
      (else (cons (insertR* n o (car l))(insertR* n o (cdr l)))))))

(define occur*
  (lambda (o l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) o)(add1 (occur* o (cdr l))))
         (else (occur* o (cdr l)))))
      (else (+ (occur* o (car l))(occur* o (cdr l)))))))

(define subst*
  (lambda (n o l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) o)(cons n (subst* n o (cdr l))))
         (else (cons (car l)(subst* n o (cdr l))))))
      (else (cons (subst* n o (car l))(subst* n o (cdr l)))))))

(define insertL*
  (lambda (n o l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) o)(cons n (cons o (insertL* n o (cdr l)))))
         (else (cons (car l) (insertL* n o (cdr l))))))
      (else (cons (insertL* n o (car l))(insertL* n o (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l))(member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2))#t)
      ((or (null? l1)(null? l2))#f)
      ((and (atom? (car l1))(atom? (car l2)))
       (and (eqan? (car l1)(car l2))(eqlist? (cdr l1)(cdr l2))))
      ((or (atom? (car l1))(atom? (car l2)))#f)
      (else (and 
             (eqlist? (car l1)(car l2))
             (eqlist? (cdr l1)(cdr l2)))))))


#|assignment disallowed
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1)(atom? s2))(eqan? s1 s2))
      ((or (atom? s1)(atom? s2))#f)
      (else (eqlist? s1 s2)))))
|#


;equal applies to both atom and s-expression
;(equal? 'a 'a)
;(equal? '(a (a)) '(a (a)))


#|
(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
;>>> ((coffee) ((tea)) (and (hick)))

(insertR* 'n 'o '((a a o) (((o)) a) (o) o (o (a o ((o))))) )
;>>> ((a a o n) (((o n)) a) (o n) o n (o n (a o n ((o n)))))

(insertL* 'n 'o '((a a o) (((o)) a) (o) o (o (a o ((o))))) )
;>>> ((a a n o) (((n o)) a) (n o) n o (n o (a n o ((n o)))))

(occur* 'o '((a a o) (((o)) a) (o) o (o (a o ((o))))) )
;>>> 7

(subst* 'n 'o '((a a o) (((o)) a) (o) o (o (a o ((o))))) )
;>>> ((a a n) (((n)) a) (n) n (n (a n ((n)))))

(member* 'hick '((coffee) cup ((tea) cup) (and (hick)) cup))
;>>> #t

(eqlist? '(beef (sausage (and)) soda) '(beef (sausage (and)) soda))
;>>> #t

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chapter 6 - Shadows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp)(number? aexp))
      ((eq? (car (cdr aexp)) '+)
       (and (number? (car aexp))(number? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '*)
       (and (number? (car aexp))(number? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^)
       (and (number? (car aexp))(number? (car (cdr (cdr aexp))))))
      (else #f))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (+ (value (car nexp))(value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*)
       (* (value (car nexp))(value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '^)
       (expt (value (car nexp))(value (car (cdr (cdr nexp))))))
      (else 'error))))

;(expt 3 3)
;(numbered? '(1 ^ 2))
;(value '(4 + (3 ^ 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chapter 7 - Friends and Relations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat)(cdr lat)) #f)
      (else (set? (cdr lat))))))

;first def of makeset
(define makeset_original
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat)(cdr lat))(makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

;second def of makeset
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
                  (makeset (multirember (car lat)(cdr lat))))))))

(define subset?
  (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      (else (and 
             (member (car s1) s2)
             (subset? (cdr s1) s2))))))

(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2)(subset? s2 s1))))

(define intersect?
  (lambda (s1 s2)
    (cond
      ((null? s1) #f)
      (else (or 
             (member? (car s1) s2)
             (intersect? (cdr s1) s2))))))

(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      ((member? (car s1) s2)(cons (car s1)(intersect (cdr s1) s2)))
      (else (intersect (cdr s1) s2)))))

(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((member? (car s1) s2)(union (cdr s1) s2))
      (else (cons (car s1)(union (cdr s1) s2))))))

(define diff
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      ((member? (car s1) s2)(diff (cdr s1) s2))
      (else (cons (car s1) (diff (cdr s1) s2))))))

(define intersectall
  (lambda (lset)
    (cond
      ((null? (cdr lset))(car lset))
      (else (intersect
             (car lset)
             (intersectall (cdr lset)))))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel_original
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons
             (build (second (car rel))
                    (first (car rel)))
             (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair)(first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))(revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))


#|
;eq? works for both atom and num
(eq? 'a 'a)
(eq? 1 1)
(set? '(1 2 3 4))
(set? '(1 2 3 4 1))

(makeset '(1 2 3 4))
(makeset '(1 2 3 4 1))
;>>> (1 2 3 4)
(makeset '(apple peach plum apple peach pear lemon pear))
;>>> (apple peach plum pear lemon)

(subset? '(1 2) '(1 2 3 4))
(eqset? '(2 1 3) '(3 2 1 4))
(eqset? '(2 1 3) '(3 2 1))
(intersect? '(6 5 4) '(1 2 3 4))
(intersect '(6 5 4) '(1 2 3 4))
;>>> (4)

(union '(6 5 4) '(1 2 3 4))
;>>> (6 5 1 2 3 4)

(diff '(6 5 4) '(1 2 3 4))
;>>> (6 5)

(intersectall '((1 2 3 4) (1 3 5) (3 4 5)))
;>>> (3)

(firsts '((1 10) (2 20) (3 30)))
;>>> (1 2 3)
(seconds '((1 10) (2 20) (3 30)))
;>>> (10 20 30)

(fun? '((1 10) (2 20) (3 30)))
;>>> #t
(fun? '((2 10) (2 20) (3 30)))
;>>> #f

(revrel '((1 10) (2 20) (3 30)))
;>>> ((10 1) (20 2) (30 3))
(fullfun? '((1 10) (2 20) (3 30)))
;>>> #t

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chapter 8 - Lambda the Ultimate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;passing eq? or equal? as parameter 
(define rember-f1
  (lambda (test? a l)
    (cond
      ((null? l)'())
      ((test? (car l) a)(cdr l))
      (else (cons (car l)(rember-f1 test? a (cdr l)))))))

;(equal? '(1 (2 3)) '(1 (2 3)))
;(rember-f1 = 5 '(6 5 4 3))
;(rember-f1 equal? '((2 3)) '(1 ((2 3)) 4))
;(rember-f1 eq? 'jelly '(jelly beans are good))

;curry-ing
;lambda that returns lambda
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

;((eq?-c 'salad) 'salad)

;this function takes x as parameter and test if eq to salad
(define eq?-salad
  (eq?-c 'salad))

;(eq?-salad 'salad)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

;(rember-eq? 'tuna '(tuna salad is good))
;((rember-f eq?) 'tuna '(tuna salad is good))
;((rember-f eq?) 'eq? '(equal? eq? eqlist? eqan?))

(define insertL-f
  (lambda (test?)
    (lambda (n o l)
      (cond
        ((null? l) '())
        ((test? (car l) o)(cons n (cons o (cdr l))))
        (else (cons (car l) ((insertL-f test?) n o (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (n o l)
      (cond
        ((null? l) '())
        ((test? (car l) o)(cons n (cons o (cdr l))))
        (else (cons (car l) ((insertR-f test?) n o (cdr l))))))))

(define insert-g
  (lambda (seq)
    (lambda (n o l)
      (cond
        ((null? l) '())
        ((eq? (car l) o)(seq n o (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) n o (cdr l))))))))

(define seqL
  (lambda (n o l)
    (cons n (cons o l))))

(define seqR
  (lambda (n o l)
    (cons o (cons n l))))

(define seqS
  (lambda (n o l)
    (cons n l)))

(define seqRem
  (lambda (n o l)
    (cons l)))

(define insertLL (insert-g seqL))
(define insertRR (insert-g seqR))
(define substSS (insert-g seqS))
(define remberRR 
  (lambda (a l)
    ((insert-g seqRem) #f a l)))

;(insertLL 1 2 '(3 4 2 3))
;(insertRR 1 2 '(3 4 2 3))
;(substSS 1 2 '(3 4 2 3))

(define atom-to-func
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      ((eq? x '^) expt))))

(define value-h
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-func (car (cdr nexp)))
             (value-h (car nexp))
             (value-h (car (cdr (cdr nexp)))))))))

;(value-h '(1 + (3 ^ 2)))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((eq? (car lat) a) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq? (multirember-f eq?))

;(multirember-eq? '2 '(1 2 3 4 3 2 1))
;((multirember-f eq?) '2 '(1 2 3 4 3 2 1))

(define eq?-tuna
 (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test? (cdr lat)))))))

;(multiremberT eq?-tuna '(tuna salad and tuna and salad))

;continuation with collector
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)(col '() '()))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

;(a-friend '1 '(1 2 3 4))

;(multirember&co 'tuna '(a tuna a tuna tuna tuna tuna) collector)

#|
;reminder of previous definition of multiinsertL and multiinsertR

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multiiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
|#

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new 
             (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))))))

;(multiinsertLR '3 '1 '2 '(0 1 0 1 1 2 1 2 0 2 0 1 0 2))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)(col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR
                      (cdr lat)
                      (lambda (newlat L R)
                        (col (cons new (cons oldL newlat))
                             (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR
                      (cdr lat)
                      (lambda (newlat L R)
                        (col (cons oldR (cons new newlat))
                             L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR
                      (cdr lat)
                      (lambda (newlat L R)
                        (col (cons (car lat) newlat)
                             L R)))))))

;(multiinsertLR&co '3 '1 '2 '(0 1 0 1 1 2 1 2 0 2 0 1 0 2)
                  ;(lambda (n l r) n))

(define isEven
  (lambda (n)
    (integer? (/ n 2))))

;(isEven 0)

(define even-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l) (even-only* (cdr l))))
         (else (even-only* (cdr l)))))
      (else (cons (even-only* (car l)) (even-only* (cdr l)))))))

;(even-only* '( 23 4 2 (94 (33)) (() ((1)) 2) 0))
;>>> (4 2 (94 ()) (() (()) 2) 0)

(define even-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (even-only*&co (cdr l)
                         (lambda (newl p s)
                           (col (cons (car l) newl)
                                (* (car l) p)
                                s))))
         (else
          (even-only*&co (cdr l)
                         (lambda (newl p s)
                           (col newl
                                p
                                (+ (car l) s)))))))
      (else (even-only*&co (car l) (_____))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chapter 9 - ... and Again, and Again, and Again,...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(pick 4 '(6 2 4 caviar 5 7 3))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat);sorn = symbol or number
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define eternity
  (lambda (x)
    eternity(x)));it never ends!!!

;(looking 'caviar '(6 2 4 caviar 5 7 3))

(define shift1;without using build
  (lambda (pair)
    (cons
     (first (first pair))
     (cons (cons (second (first pair))
                 (cons (second pair)'()))
           '()))))

(define shift;using build
  (lambda (pair)
    (build
     (first (first pair))(build (second (first pair))
                                (second pair)))))

;(first '(1 2 3))
;(second '(1 (1 11 1)))

;(shift '((a b) c))
;>>> (a (b c))

;(shift '((a b) (c d)))
;>>> (a (b (c d)))


(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((pair? (first pora));a-pair is not defined
       (align (shift pora)))
      (else (build (first pora)(align (second pora)))))))

;(align '((1 2) ((3 4) ((5 6) (7 8)))))
;(1 (2 (3 (4 (5 (6 (7 8)))))))


;it only accept pairs
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (car pora)) (length* (car (cdr pora))))))))

;(length* '(((1 2) (3 4))(5 6)) )
;6

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* 2 (weight* (car pora)))
               (weight* (car (cdr pora))))))))

;(weight* '((a b) c))
;7
;(weight* '(a (b c)))
;5

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((pair? (first pora))(shuffle (revpair pora)))
      (else (build (first pora)(shuffle (second pora)))))))

(define one? (lambda (x)(= 1 x)));create one? for C
;(one? 1)
;#t

(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (/ n 2)))
      (else (C (+ 1 (* n 3)))))))

;(C 13)

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))

;(add1 7)
;(sub1 7)

;(A 1 0)
;2
;(A 1 1)
;3
;(A 2 2)
;7
;(A 3 3)

;don't uncomment the next line cuz it runs forever
;(A 4 3)


;(will-stop? function)
;(define will-stop? (lambda (f)(...)))

(define will-stop?
  (lambda (f)
    ...))

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))

#|
(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

;; what if we do it without "define" ?
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

;; length1: determine the length of a list w\ no more than 1 item
;; to replace length0
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (length0 (cdr l))))))

;; replace length0 by its definition
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (l)
                   (cond
                     ((null? l) 0)
                     (else (add1 (eternity (cdr l))))))
                 (cdr l))))))

;; length2: determine the length of a list w\ no more than 2 item
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (l)
                   (cond
                     ((null? l) 0)
                     (else (add1 ((lambda (l)
                                    (cond
                                      ((null? l) 0)
                                      (else (add1 
                                             (eternity x))))))
                                 (cdr l)))))
                 (cdr l))))))

;; rewrite length0 starting with (lambda (length))
;; lambda (length) returns a function
;; eternity is passed into length
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)



;; rewrite length1 starting with (lambda (length))
((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
 eternity))


;; rewrite length2 starting with (lambda (length))
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
   eternity)))

;name the function that takes length 
;and returns a function that looks like length

;length0 with make-length
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; length1 with make-length
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; length2 with make-length
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; length3 with make-length
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; apply mk-length to mk-length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (lenfun)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (lenfun (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))));pass (cdr l) to mk-l?

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (lenfun)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((lenfun eternity) (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length eternity) (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length) (cdr l))))))))

;; simplify ... 
((lambda (mk)(mk mk))
 (lambda (mk)(lambda (l)
               (cond
                 ((null? l) 0)
                 (else (add1 ((mk mk)(cdr l))))))))

|#


'(infinite loops in this block)#|
;; abstract out
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))

;; simplify the previous one
((lambda (mk)(mk mk))
 (lambda (m)
   ((lambda (le)(lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (len (cdr l)))))))
    (m m))))

;; expand it, we get
((lambda (m)
   ((lambda (le)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (le (cdr l)))))))
    (m m)))
 (lambda (m)
   ((lambda (le)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (le (cdr l)))))))
    (m m))))
|#
'(end of the block of infinite loops)
  
  
#|
((lambda (mk)(mk mk))
 (lambda (mk)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((lambda (x) ((mk mk) x)) (cdr l))))))))

;; replace "(lambda (x) ((mk mk) x))"  with "length"
((lambda (mk)(mk mk))
 (lambda (mk)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (lambda (x)((mk mk) x)))))

;; abstract out the part that looks like length
((lambda (le)
   ((lambda (mk)(mk mk))
    (lambda (mk)
      (le (lambda (x)
            ((mk mk) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;; seperate mk-length and length
(lambda (le)
  ((lambda (mk)(mk mk))
   (lambda (mk)
     (le (lambda (x)
           ((mk mk) x))))))

;; Applicative-Order Y-Combinator
(define Y
  (lambda (le)
    ((lambda (mk)(mk mk))
     (lambda (mk)
       (le (lambda (x)
             ((mk mk) x)))))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Chapter 10 - What Is the Value of All of This ?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; entry = pair of lists {1st is a set, len(e1)=len(e2)}
;; i.e. ((appetizer entree beverage) (pate bouf vin))

;(first '(1 2))
;(second '(1 2))
;(build '(list one) '(list two))

(define new-entry build)


(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names)(entry-f name))
      ((eq? (car names) name)(car values))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr values)
                                  entry-f)))))

;; define to-eat table
(define to-eat '((breakfast lunch dinner)
                 ((milk egg) (beef rice) (noodle soup))))

;(lookup-in-entry 'lunch to-eat (lambda (x) x))
;(lookup-in-entry 'what to-eat (lambda (x) x))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table)(table-f name))
      (else (lookup-in-entry name (car table)
                             (lambda (name) 
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))
#|
(cons 'a
      (cons 'b
            (cons 'c
                  (quote ()))))|#
;(a b c)

#|
(cons 'car
      (cons (cons 'quote
                  (cons
                   (cons 'a
                         (cons 'b
                               (cons 'c
                                     (quote ()))))
                   (quote ())))
            (quote ())))|#
;(car '(a b c))

;((lambda (nothing)(cons nothing '())) '(from nothing to something))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e)(atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))(cond
                        ((eq? (car e)(quote quote))
                         *quote)
                        ((eq? (car e)(quote lambda))
                         *lambda)
                        ((eq? (car e)(quote cond))
                         *cond)
                        (else *application)))
      (else *application))))


(define value
  (lambda (e)
    (meaning (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)
















