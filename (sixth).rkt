#lang scheme
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define a-pair?
  (lambda (p)
    (cond
      ((atom? p) #f)
      ((null? p) #f)
      ((null? (cdr p)) #f)
      (else (null? (cdr (cdr p)))))))

(define pick
  (lambda (n ls)
    (cond
      ((null? ls) #f)
      (else
       (cond
       ((zero? (- n 1)) (car ls))
       (else (pick (- n 1) (cdr ls))))))))

  
(define looking
  (lambda (a ls)
    (keep-looking a (pick 1 ls) ls)))

(define keep-looking
  (lambda (a sorn ls)
    (cond
      ((number? sorn) (keep-looking a (pick sorn ls) ls))
      (else (eq? sorn a)))))

(define build
  (lambda (a ls)
    (cons a (cons ls '()))))

(define first
  (lambda (ls)
    (car ls)))

(define second
  (lambda (ls)
    (car (cdr ls))))

(define shift
  (lambda (ls)
    (build (first (first ls)) (build (second (first ls)) (second ls)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora)) (length* (second pora)))))))

(define Y
  (lambda (le)
    ((lambda (f)
      (f f))
    ((lambda (f)
       (le (lambda (x) ((f f) x))))))))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name set values f)
    (cond
      ((null? set) (f name))
      ((eq? (car set) name) (car values))
      (else
       (lookup-in-entry-help name (cdr set) (cdr values) f)))))

(define extend-table cons)

(define lookup-in-table
   (lambda (name table table-f)
     (cond
       ((null? table) (table-f name))
       (else (lookup-in-entry name (car table)
                              (lambda (n)
                                (lookup-in-table name (cdr table) table-f)))))))

(define expression-to-action
  (lambda (exp)
    (cond
      ((atom? exp) (atom-to-action exp))
      (else (list-to-action exp)))))

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
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
       (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

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
    (car (quote()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive) (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (e table)
    (cond
      ((null? e) '())
      (else
       (cons (meaning (car e) table) (evlis (cdr e) table))))))

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table) (evlis (arguments-of e) table))))

(define function-of first)
(define arguments-of second)

(define primitive?
  (lambda (x)
    (eq? (first x) (quote primitive))))

(define non-primitive?
  (lambda (x)
    (eq? (first x) (quote non-primitive))))

(define apply
  (lambda (fun vals)
    ((primitive? fun) (apply-primitive (second fun) vals))
    ((non-primitive? fun) (apply-closure (second fun) vals))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? (quote cons) name) (cons (first vals) (second vals)))
      ((eq? (quote car) name) (car (first vals)))
      ((eq? (quote cdr) name) (cdr (first vals)))
      ((eq? (quote null?) name) (null? (first vals)))
      ((eq? (quote eq?) name) (eq? (first vals) (second vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry (formals-of closure) vals)
              (table-of closure)))))
    


      






       
                          
       
                  
      


  
      
     
                      