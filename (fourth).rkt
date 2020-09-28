#lang scheme
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define rember-f
  (lambda (test? val l)
    (cond
      ((null? l) '())
      ((test? (car l) val) (rember-f test? val (cdr l)))
      (else (cons (car l) (rember-f test? val (cdr l)))))))

(define rember-f-1
  (lambda (test?)
    (lambda (val l)
      (cond
        ((null? l) '())
        ((test? (car l) val) (rember-f test? val (cdr l)))
        (else (cons (car l) (rember-f test? val (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (old new ls)
      (cond
        ((null? ls) '())
        ((test? (car ls)) (cons new (cons old (cdr ls))))
        (else (cons (car ls) ((insertL-f test?) old new (cdr ls))))))))

(define insertR-f
  (lambda (test?)
    (lambda (old new ls)
      (cond
        ((null? ls) '())
        ((test? (car ls)) (cons old (cons new (cdr ls))))
        (else (cons (car ls) ((insertR-f test?) old new (cdr ls))))))))

(define seqL
  (lambda (old new ls)
    (cons new (cons old ls))))
(define seqR
  (lambda (old new ls)
    (cons old(cons new ls))))

(define insert-g
  (lambda (sec)
    (lambda (test?)
      (lambda (old new ls)
        (cond
          ((null? ls) '())
          ((test? (car ls) old) (sec old new (cdr ls)))
          (else (cons (car ls) (((insert-g sec) test?) old new (cdr ls)))))))))

(define atom-to-func
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x 'x) *))))

(define operator
  (lambda (x)
    (car x)))

(define operand-1
  (lambda (x)
    (car (cdr x))))

(define operand-2
  (lambda (x)
    (car (cdr (cdr x)))))

(define value
  (lambda (x)
    (cond
      ((atom? x) x)
      (else 
       ((atom-to-func (operator x)) (value (operand-1 x)) (value(operand-2 x)))))))

(define multirember-f
  (lambda (test?)
    (lambda (v ls)
      (cond
        ((null? ls) '())
        ((test? (car ls) v) ((multirember-f test?) v (cdr ls)))
        (else
         (cons (car ls) ((multirember-f test?) v (cdr ls))))))))

(define multiremberT
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        ((f (car ls)) ((multiremberT f) (cdr ls)))
        (else
         (cons (car ls) ((multiremberT f) (cdr ls))))))))




    
