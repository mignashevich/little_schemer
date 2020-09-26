#lang scheme
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
(define add1
  (lambda (a)
    (+ a 1)))

(define sub1
  (lambda (a)
    (- a 1)))

(define plus
 (lambda (a b)
   (cond
     ((zero? b) a)
     (else
      (add1 (plus a (sub1 b)))))))

(define plus1
  (lambda (a b)
    (cond
      ((zero? b ) a )
      (else
       (plus1 (add1 a) (sub1 b))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (plus (car tup) (addtup (cdr tup)))))))

(define mult
  (lambda (a b)
    (cond
      ((eq? b 1) a)
      (else (plus a (mult a (sub1 b)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) (quote ()))
      (else
       (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else
       (> (sub1 a) (sub1 b))))))

(define <
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else
       (< (sub1 a) (sub1 b))))))

(define =
  (lambda (a b)
    (cond
      ((< a b) #f)
      ((> a b) #f)
      (else #t))))

(define pow
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else
       (mult a (pow a (sub1 b)))))))

(define div
  (lambda (a b)
    (cond
      ((< a b) 0)
      (else
       (add1 (div (- a b) b))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((null? lat) #f)
      (else
       (cond
         ((zero? n) (car lat))
         (else
          (pick (sub1 n) (cdr lat))))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((number? (car lat)) (no-nums (cdr lat)))
         (else (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      ((or (number? a) (number? b)) #f)
      (else (equal? a b)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat))
       (cond
         ((eq? a (car lat)) (rember* a (cdr lat)))
       (else
        (cons (car lat) (rember* a (cdr lat))))))
      (else (cons (rember* a (car lat)) (rember* a (cdr lat)))))))

(define insertR*
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old) (cons (car lat) (cons new (insertR* new old (cdr lat)))))
         (else (cons (car lat) (insertR* new old (cdr lat))))))
      (else (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))

(define occur*
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((atom? (car lat))
       (cond
         ((eq? a (car lat)) (add1 (occur* a (cdr lat))))
         (else (occur* a (cdr lat)))))
      (else (plus (occur* a (car lat)) (occur* a (cdr lat)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (old new l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (cons old (insertL* old new (cdr l)))))
         (else (cons (car l) (insertL* old new (cdr l))))))
      (else
       (cons (insertL* old new (car l)) (insertL* old new (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))


               
       

       








   