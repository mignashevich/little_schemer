#lang scheme
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? l) l)
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((null? l1) (null? l2))
      ((atom? (car l1))
              (cond
                ((and (atom? (car l2)) (eq? (car l1) (car l2)))
                 (eqlist? (cdr l1) (cdr l2)))
               (else #f)))
      (else
       (cond
         ((atom? (car l2)) #f)
         (else
          (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eq? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist1? s1 s2)))))

(define eqlist1?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else 
        (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))))

(define numbered?
  (lambda (exp)
    (cond
      ((atom? exp) (number? exp))
      ((eq? (car (cdr exp)) (quote +))  (and (numbered? (car exp)) (numbered? (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) (quote -))  (and (numbered? (car exp)) (numbered? (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) (quote x))  (and (numbered? (car exp)) (numbered? (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) (quote ~))  (and (numbered? (car exp)) (numbered? (car (cdr (cdr exp)))))))))

(define value
  (lambda (exp)
    (cond 
      ((atom? exp) exp)
      ((eq? (car (cdr exp)) (quote +)) (+ (value (car exp)) (value (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) (quote -)) (- (value (car exp)) (value (car (cdr (cdr exp)))))))))

(define value-polish
  (lambda (exp)
    (cond 
      ((atom? exp) exp)
      ((eq? (car exp) (quote +)) (+ (value-polish (car (cdr exp))) (value-polish (car (cdr (cdr exp))))))
      ((eq? (car exp) (quote -)) (- (value-polish (car (cdr exp))) (value-polish (car (cdr (cdr exp)))))))))

(define 1-sub-exp
  (lambda (exp)
    (car (cdr exp))))

(define 2-sub-exp
  (lambda (exp)
    (car (cdr (cdr exp)))))

(define operator
  (lambda (exp)
    (car exp)))

(define value-polish-2
  (lambda (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (operator exp) (quote +)) (+ (value-polish-2 (1-sub-exp exp)) (value-polish-2 (2-sub-exp exp))))
       ((eq? (operator exp) (quote -)) (- (value-polish-2 (1-sub-exp exp)) (value-polish-2 (2-sub-exp exp)))))))

(define has?
  (lambda (a l)
    (cond
    ((null? l) #f)
    ((eq? a (car l)) #t)
    (else (has? a (cdr l))))))


(define set?
  (lambda (s)
    (cond
      ((null? s) #t)
      (else (and (not (has? (car s) (cdr s))) (set? (cdr s)))))))


(define makeset
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((set? (cons (car l) (makeset (cdr l)))) (cons (car l) (makeset (cdr l))))
      (else (makeset (cdr l))))))

(define subset?
  (lambda (s1 s2)
    (cond
    ((null? s1) #t)
    ((has? (car s1) s2) (subset? (cdr s1) s2))
    (else #f))))

(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2) (subset? s2 s1))))

(define intersect?
 (lambda (s1 s2)
   (cond
     ((null? s1) #f)
     ((has? (car s1) s2) #t)
     (else (intersect? (cdr s1) s2)))))

(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) (quote ()))
      ((has? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
      (else (intersect (cdr s1) s2)))))

(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      (else (makeset (cons (car s1) (union (cdr s1)  s2)))))))

(define intersectall-l
  (lambda (ls)
    (cond
      ((null? (cdr ls)) (car ls))
      (else (intersect (car ls) (intersectall-l (cdr ls)))))))

(define a-pair?
  (lambda (p)
    (cond
      ((atom? p) #f)
      ((null? p) #f)
      ((null? (cdr p)) #f)
      (else (null? (cdr (cdr p)))))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
   (car (cdr p))))

(define build
  (lambda (l r)
    (cons l (cons r '()))))

(define firsts
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (first (car rel)) (firsts (cdr rel)))))))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (second (car rel)) (seconds (cdr rel)))))))

       
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

                          

   
      

