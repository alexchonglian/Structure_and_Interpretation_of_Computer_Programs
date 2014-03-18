
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 11 - Welcome Back to the Show
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define atom?
  (lambda (x)
    (and (not (pair? x))(not (null? x)))))

;(atom? '2)
;#t
;(atom? '())
;#f

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

;(member? '1 '(1 2 3 4))
;(member? '0 '(1 2 3 4))

(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? a (car lat))))))

#|
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (or (is-first? (car lat)(cdr lat))
                (two-in-a-row? (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (is-first-b? (car lat)(cdr lat))))))
|#

(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (two-in-a-row? lat))))))

(define two-in-a-row-b?
  (lambda (preceeding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) preceeding)
                (two-in-a-row-b? (car lat)(cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat)(cdr lat))))))

;(two-in-a-row? '(a b c c d))
;#t

;orginal name is sum-of-prefixes
(define integrate
  (lambda (tup)
    (integrate-b 0 tup)))

;orginal name is sum-of-prefixes-b
(define integrate-b
  (lambda (sonssf tup);sum of numbers seen so far
    (cond
      ((null? tup)'())
      (else (cons (+ sonssf (car tup))
                  (integrate-b (+ sonssf (car tup))(cdr tup)))))))

;(integrate '(1 1 1 1))

(define pick
  (lambda (n lat)
    (cond
      ((= 1 n)(car lat))
      (else (pick (- n 1)(cdr lat))))))

;(pick '3 '(1 2 3 4))

;each number is treated as a backward index from its position
(define scramble-b
  (lambda (tup rev-pre);rev-pre = reversed prefix
    (cond
      ((null? tup) '())
      (else (cons (pick (car tup)(cons (car tup) rev-pre))
                  (scramble-b (cdr tup)
                              (cons (car tup) rev-pre)))))))

;(scramble-b '(1 1 1 3 4 2 1 1 9 2) '())
;(1 1 1 1 1 4 1 1 1 9)
;(scramble-b '(1 2 3 4 5 6 7 8 9) '())
;(1 1 1 1 1 1 1 1 1)

(define scramble
  (lambda (tuple)
    (scramble-b tuple '())))

;(scramble '(1 1 1 3 4 2 1 1 9 2))
;(1 1 1 1 1 4 1 1 1 9)
;(scramble '(1 2 3 4 5 6 7 8 9))
;(1 1 1 1 1 1 1 1 1)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 12 - Take Cover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Y
  (lambda (le)
    ((lambda (f)(f f))
     (lambda (f)(le (lambda (x)((f f) x)))))))

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? a (car lat))(mr (cdr lat)))
              (else (cons (car lat)(mr (cdr lat))))))))
     lat)))

(define ???
  ((lambda (le)
     ((lambda (f)(f f))
      (lambda (f)
        (le (lambda (x) ((f f) x))))))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (+ 1 (length (cdr l)))))))))

(define len
  (Y (lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (+ 1 (length (cdr l)))))))))


;; test if a function can be overwritten
;(define funct (lambda (x) (+ 2 x)))
;(funct 2)
;4

;;modify the difinition
;(define funct (lambda (x) (- 2 x)))
;(funct 2)
;0

;; the answer is ...
;; yes it can be overwritten


(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? a (car lat))(mr (cdr lat)))
                  (else (cons (car lat)(mr (cdr lat))))))))
       mr)
     lat)))

;; some change in the letrec line and the last line
(define multirember
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond
                 ((null? lat) '())
                 ((eq? a (car lat))(mr (cdr lat)))
                 (else (cons (car lat)(mr (cdr lat))))))))
      (mr lat))))

; (letrec ((mr ...)) mr)
; ((letrec ((mr ...)) mr) lat)
; (letrec ((mr ...)) (mr lat))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l)((rember-f test?) a (cdr l))))))))

;((rember-f eq?) '1 '(0 1 2 3))
;(0 2 3)

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat))((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))


(define multirember-f
  (lambda (test?)
    (letrec
        ((m-f (lambda (a lat)
                (cond
                  ((null? lat) '())
                  ((test? a (car lat))(m-f a (cdr lat)))
                  (else (cons (car lat)(m-f a (cdr lat))))))))
      m-f)))


(define member?
  (lambda (a lat)
    ((letrec
         ((yes? (lambda (l)
                  (cond
                    ((null? l) #f)
                    ((eq? a (car l)) #t)
                    (else (yes? (cdr l)))))))
       yes?)
     lat)))

;(member? '1 '(0 1 2)); #t


(define member?
  (lambda (a lat)
    (letrec
        ((yes? (lambda (l)
                 (cond
                   ((null? l) #f)
                   ((eq? a (car l)) #t)
                   (else (yes? (cdr l)))))))
      (yes? lat))))

;(member? '1 '(0 1 2)); #t


(define union
  (lambda (s1 s2)
    (letrec
        ((U (lambda (set)
              (cond
                ((null? set) s2)
                ((member? (car set) s2)(U (cdr set)))
                (else (cons (car set)(U (cdr set))))))))
      (U s1))))

;(union '(1 2 3) '(0 1 2))

(define union
  (lambda (s1 s2)
    (letrec
        ((U (lambda (set)
              (cond
                ((null? set) s2)
                ((M? (car set) s2)(U (cdr set)))
                (else (cons (car set)(U (cdr set)))))))
         (M? (lambda (a lat)
               (cond
                 ((null? lat) #f)
                 ((eq? a (car lat))#t)
                 (else (M? a (cdr lat)))))))
      (U s1))))

;(member? '1 '(0 1 2)); #t

(define two-in-a-row?
  (lambda (lat)
    (letrec
        ((W (lambda (a lat)
              (cond
                ((null? lat) #f)
                (else (or (eq? (car lat) a)
                          (W (car lat)(cdr lat))))))))
      (cond
        ((null? lat) #f)
        (else (W (car lat)(cdr lat)))))))

;(two-in-a-row? '(a b c c d)); #t


(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else (or (eq? (car lat) a)
                        (W (car lat)(cdr lat))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat)(cdr lat)))))))

;(two-in-a-row? '(a b c c d)); #t

(define sum-of-prefixes
  (lambda (tup)
    (letrec
        ((S (lambda (sss tup)
              (cond
                ((null? tup) '())
                (else (cons (+ sss (car tup))
                            (S (+ sss (car tup))(cdr tup))))))))
      (S 0 tup))))

;(sum-of-prefixes '(1 1 1 1 1))
;(1 2 3 4 5)

(define sum-of-prefixes
  (letrec
      ((S (lambda (sss tup)
            (cond
              ((null? tup) '())
              (else (cons (+ sss (car tup))
                          (S (+ sss (car tup))(cdr tup))))))))
    (lambda (tup)
      (S 0 tup))))

;(sum-of-prefixes '(1 1 1 1 1))
;(1 2 3 4 5)

(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
                ((null? tup) '())
                (else (cons (pick (car tup)(cons (car tup) rp))
                            (P (cdr tup)(cons (car tup) rp))))))))
      (P tup '()))))

;(scramble '(1 1 1 3 4 2 1 1 9 2))
;(1 1 1 1 1 4 1 1 1 9)
;(scramble '(1 2 3 4 5 6 7 8 9))
;(1 1 1 1 1 1 1 1 1)


(define scramble
  (letrec
      ((P (lambda (tup rp)
            (cond
              ((null? tup) '())
              (else (cons (pick (car tup)(cons (car tup) rp))
                          (P (cdr tup)(cons (car tup) rp))))))))
    (lambda (tup)
      (P tup '()))))

;(scramble '(1 1 1 3 4 2 1 1 9 2))
;(1 1 1 1 1 4 1 1 1 9)
;(scramble '(1 2 3 4 5 6 7 8 9))
;(1 1 1 1 1 1 1 1 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 13 - Hop, Skip, and Jump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define intersect
  (lambda (s1 s2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) '())
                ((member? (car set) s2)(cons (car set)
                                             (I (cdr set))))
                (else (I (cdr set)))))))
      (I s1))))

;(intersect '(0 1 2) '(1 2 3))
;(1 2)

(define intersectall
  (lambda (lset)
    (cond
      ((null? lset) '())
      ((null? (cdr lset))(car lset))
      (else (intersect (car lset)(intersectall (cdr lset)))))))

;(intersectall '((0 1 2 3 4) (2 4 6 8) (1 2 4 8)))
;(2 4)


(define intersectall
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
                ((null? (cdr lset))(car lset))
                (else (intersect (car lset)(A (cdr lset))))))))
      (cond
        ((null? lset) '())
        (else (A lset))))))

;(intersectall '((0 1 2 3 4) (2 4 6 8) (1 2 4 8)))
;(2 4)

;(lambda (x y) M)
;(letrec ((x F) (y G)) M)

(define intersectall
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset))(hop '()))
                   ((null? (cdr lset))(car lset))
                   (else (intersect (car lset)(A (cdr lset))))))))
         (cond
           ((null? lset) '())
           (else (A lset))))))))


;(intersectall '((0) () (0 1 2 3 4) (2 4 6 8) (1 2 4 8)))
;()


(define intersect
  (lambda (s1 s2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) '())
                ((member? (car set) s2)(cons (car set)
                                             (I (cdr set))))
                (else (I (cdr set)))))))
      (cond
        ((null? s2) '())
        (else (I s1))))))

#|
(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
          ((A ...)
           (I ...))
        (cond
          ((null? lset) '())
          (else (A lset)))))))ƒƒ
|#

(define intersectall
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset))(hop '()))
                   ((null? (cdr lset))(car lset))
                   (else (I (car lset)(A (cdr lset)))))))
            (I (lambda (s1 s2)
                 (letrec
                     ((J (lambda (s1)
                           (cond
                             ((null? s1) '())
                             ((member? (car s1) s2)(J (cdr s1)))
                             (else (cons (car s1)(J (cdr s1))))))))
                   (cond
                     ((null? s2) '())
                     (else (J s1)))))))
         (cond
           ((null? lset) '())
           (else (A lset))))))))

;(intersectall '((0) () (0 1 2 3 4) (2 4 6 8) (1 2 4 8)))
;(2 4)

(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a)(cdr lat))
                (else (cons (car lat)(R (cdr lat))))))))
      (R lat))))

;(rember '1 '(0 1 2 3 4 5))
;(0 2 3 4 5)

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a)'())
                (else (cons (car lat)(R (cdr lat))))))))
      (R lat))))

;(rember-beyond-first '1 '(0 1 2 3 4 5))
;(0)

(define rember-upto-last
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a)(cdr lat))
                (else (R (cdr lat)))))))
      (R lat))))

;(rember-upto-last '1 '(0 1 2 3 4 5))
;(2 3 4 5)
;(rember-upto-last '10 '(0 1 2 3 4 5))
;()
;(rember-upto-last '1 '(0 1 2 3 4 1 5))
;(2 3 4 1 5)

(define rember-upto-last
  (lambda (a lat)
    (call-with-current-continuation
     (lambda (skip)
       (letrec
           ((R (lambda (lat)
                 (cond
                   ((null? lat) '())
                   ((eq? (car lat) a)(skip (R (cdr lat))))
                   (else (cons (car lat)(R (cdr lat))))))))
         (R lat))))))

;(rember-upto-last '1 '(0 1 2 3 4 5))
;(2 3 4 5)
;(rember-upto-last '10 '(0 1 2 3 4 5))
;(0 1 2 3 4 5)
;(rember-upto-last '1 '(0 1 2 3 4 1 5))
;(5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 14 - Let There Be Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))(car l))
      (else (leftmost (car l))))))

;(leftmost '(((((5) 4) 3) 2) 1))
;5
(leftmost '(() 1 2 3))
;()















