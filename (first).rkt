#lang scheme

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (fs)
    (cond
      ((null? fs) (quote ()))
      (else (cons (car (car fs)) (firsts (cdr fs)))))))

(define insertR
  (lambda (new old ls)
    (cond
      ((null? ls) (quote ()))
      ((eq? old (car ls)) (cons old (cons new (cdr ls))))
      (else (cons (car ls) (insertR new old (cdr ls)))))))

(define insertL
  (lambda (new old ls)
    (cond
      ((null? ls) (quote ()))
      ((eq? old (car ls)) (cons new ls))
      (else (cons (car ls) (insertL new old (cdr ls)))))))

(define subst
  (lambda (new old ls)
    (cond
      ((null? ls) (quote ()))
      ((eq? old (car ls)) (cons new (cdr ls)))
      (else (cons (car ls) (subst new old (cdr ls)))))))