#lang scheme
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define multiinsertLR
  (lambda (new oldL oldR ls)
    (cond
      ((null? ls) '())
      ((eq? (car ls) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr ls)))))
      ((eq? (car ls) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr ls)))))
      (else
       (cons (car ls) (multiinsertLR new oldL oldR (cdr ls)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR ls co)
    (cond
      ((null? ls) (co '() '()))
      ((eq? (car ls) oldL) (multiinsertLR&co new oldL oldR (cdr ls) (lambda (left right) (co (cons (car ls) left) right))))
      ((eq? (car ls) oldR) (multiinsertLR&co new oldL oldR (cdr ls) (lambda (left right) (co left (cons (car ls) right)))))
      (else (multiinsertLR&co new oldL oldR (cdr ls) (lambda (left right) (co  left  right)))))))

(define multiinsertLR-co
  (lambda (new oldL oldR ls co)
    (cond
      ((null? ls) (co '() 0 0))
      ((eq? (car ls) oldL) (multiinsertLR-co new oldL oldR (cdr ls) (lambda (res L R) (co (cons new (cons (car ls) res)) (+ L 1) R))))
      ((eq? (car ls) oldR) (multiinsertLR-co new oldL oldR (cdr ls) (lambda (res L R) (co (cons (car ls) (cons new res)) L (+ R 1)))))
      (else (multiinsertLR-co new oldL oldR (cdr ls) (lambda (res L R) (co (cons (car ls) res) L R )))))))

(define even
  (lambda (a)
    (cond
      ((= a 1) #f)
      (else (not (even (- a 1)))))))
               
        
(define evens-only*
  (lambda (ls)
    (cond
    ((null? ls) '())
    ((atom? (car ls))
     (cond
       ((even (car ls)) (cons (car ls) (evens-only* (cdr ls))))
       (else (evens-only* (cdr ls)))))
    (else
     (cons (evens-only* (car ls)) (evens-only* (cdr ls)))))))

(define evens-only&co
  (lambda (ls co)
    (cond
      ((null? ls) (co '() 0 1))
      ((atom? (car ls))
       (cond
         ((even (car ls)) (evens-only&co (cdr ls) (lambda (l sum prod) (co (cons (car ls) l) sum (* prod (car ls))))))
         (else (evens-only&co (cdr ls) (lambda (l sum prod) (co l (+ sum (car ls)) prod))))))
      (else
       (evens-only&co (car ls) (lambda (l sum prod) (evens-only&co (cdr ls) (lambda (l1 sum1 prod1) (co (cons l l1) (+ sum sum1) (* prod prod1))))))))))
         
                         
       
  