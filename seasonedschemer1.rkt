#lang racket

(define (member? elem coll)
  "is elem in collection?"
  (if (null? coll)
      #f
      (or (eq? elem (car coll))
	  (member? elem (cdr coll)))))


(define (two-in-a-row? lst)
  "does one element in lst occur twice in a row?"
  (cond
   ((null? lst) #f)
   ((null? (cdr lst)) #f)
   ((eq? (first lst)
	 (second lst)) #t)
   (else (two-in-a-row? (cdr lst)))))

(define (is-first? elem lst)
  "is elem eq to car of lst"
  (if (null? lst)
      #f
      (eq? (first lst) elem)))

(define (two-in-a-row?-b lst)
  (if (null? lst)
      #f
      (or (is-first? (first lst) (cdr lst))
	  (two-in-a-row?-b (cdr lst)))))


(define (two-in-a-row?-c preceeding lst)
  (if (null? lst)
      #f
      (or (eq? (first lst) preceeding)
	  (two-in-a-row?-c (first lst)
			   (cdr lst)))))

(define (two-in-a-row-b? lst)
  (if (null? lst)
      #f
      (two-in-a-row?-c (first lst) (cdr lst))))

(define (sum-of-prefixes-b sumsf lst)
  (if (null? lst) '()
      (cons (+ sumsf (first lst))
	    (sum-of-prefixes-b
	     (+ sumsf (first lst))
	     (cdr lst)))))

(define (sum-of-prefixes lst)
  (sum-of-prefixes-b 0 lst))


(define (pick n lst)
  "retrun n-th element from lst counting from 1"
  (if (= 1 n)
      (first lst)
      (pick (sub1 n) (cdr lst))))

(define (scrable-b lst rev-pre)
  (if (null? lst) '()
      (cons (pick (first lst)
		  (cons (first lst) rev-pre))
	    (scrable-b (cdr lst)
		       (cons (first lst) rev-pre)))))

(define (scramble lst)
  (scrable-b lst '()))

(define (my-multirember a lst)
  "remove all occurrences of a in lst"
  (cond
   ((null? lst) '())
   ((eq? a (first lst))
    (my-multirember a (cdr lst)))
   (else (cons (first lst) (my-multirember a (cdr lst))))))

(define (multirember a lst)
  (letrec ((mr (lambda (lst)
		 (cond
		  ((null? lst) '())
		  ((eq? a (first lst))
		   (mr (cdr lst)))
		  (else
		   (cons (first lst)
			 (mr (cdr lst))))))))
    (mr lst)))

(define (rember-f test?)
  (lambda (a l)
    (cond
     ((null? l) '())
     ((test? (first l) a) (cdr l))
     (else (cons (first l)
		 ((rember-f test?) a
		  (cdr l)))))))

(define rember-eq?
  (rember-f eq?))

(define (multirember-f test?)
  (lambda (a lst)
    (cond
     ((null? lst) '())
     ((test? (first lst) a)
      ((multirember-f test?) a
       (cdr lst)))
     (else (cons (first lst)
		 ((multirember-f test?) a (cdr lst)))))))

(define (union s1 s2)
  (cond
   ((null? s1) s2)
   ((member? (first s1) s2)
    (union (cdr s1) s2))
   (else (cons (first s1)
	       (union (cdr s1) s2)))))

(define (union-b s1 s2)
  (letrec ((u (lambda (set)
		(cond
		 ((null? set) s2)
		 ((member? (first set) s2)
		  (u (cdr set)))
		 (else (cons (car set)
			     (u (cdr set))))))))
    (u s1)))



(define (intersect s1 s2)
  (cond
   ((null? s1) '())
   ((member? (first s1) s2)
    (cons (first s1) (intersect (cdr s1) s2)))
   (else (intersect (cdr s1) s2))))

(define (intersect-b s1 s2)
  (letrec ((i (lambda (set)
		(cond
		 ((null? set) '())
		 ((member? (first set) s2)
		  (cons (first set) (i (cdr set))))
		 (else (i (cdr set)))))))
    (i s1)))



