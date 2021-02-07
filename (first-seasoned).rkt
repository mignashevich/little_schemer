#lang scheme
(define member
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member a (cdr lat))))))

(define two-in-a-row
  (lambda (lat)
    (cond
      ((null? lat) #f)
      ((null? (car lat)) #f)
      ((null? (cdr lat)) #f)
      ((null? (car (cdr lat))) #f)
      (else (or (eq? (car lat) (car (cdr lat))) (two-in-a-row (cdr lat)))))))

(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? (car lat) a)))))

(define two-in-a-row-2
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (or (is-first? (car lat) (cdr lat)) (two-in-a-row-2 (cdr lat)))))))

(define two-in-a-row-3
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (two-in-a-row-3 lat)))))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? preceding (car lat)) (two-in-a-row-b? (car lat) (cdr lat)))))))

(define sum-of-prefixes
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (sum-of-prefixes-b (car lat) (cdr lat)))))))

(define sum-of-prefixes-b
  (lambda (prefix lat)
    (cond
      ((null? lat) '())
      (else (cons (+ prefix (car lat)) (sum-of-prefixes-b (+ prefix (car lat)) (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else (pick (- n 1) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
       (cons (pick (car tup) (cons (car tup) rev-pre))
             (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (lat)
    (scramble-b lat '())))
