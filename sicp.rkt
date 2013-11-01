#lang racket

(define (gcd a b)
  "calculate gcd"
  (if (zero? b) a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (primes-in-20)
  (filter prime? (cdr (range 21))))

(define (expmod base exp m)
  (cond
   (( = exp 0) 1)
   ((even? exp)
    (remainder (square (expmod base (/ exp 2) m))
	       m))
   (else
    (remainder (* base
		  (expmod base (- exp 1) m))
	       m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (add1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
   ((= times 0) true)
   ((fermat-test n)
    (fast-prime? n (- times 1)))
   (else false)))

(define (cube n)
  (* n n n))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n)
  (+ n 1))

(define (sum-cubes a b)
  (sum cube a add1 b))

(define (msum coll)
  (if (null? coll)
      0
      (+ (car coll)
	 (msum (cdr coll)))))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (plus3 n)
  (map (λ (x) (+ 3 x)) n))



(define (last-pair lst)
  (cond
   ((null? lst) '())
   ((null? (rest lst)) (first lst))
   (else (last-pair (rest lst)))))

(define (butlast lst)
  (cond
   [(null? lst) '()]
   [(null? (rest lst)) '()]
   [else (cons (first lst) (butlast (rest lst)))]))

(define (mreverse lst)
  (cond
   [(null? lst) '()]
   [else (cons (last-pair lst) (mreverse (butlast lst)))]))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coins)
  (cond
   [(= 0 amount) 1]
   [(< amount 0) 0]
   [(null? coins) 0]
   [else (+ (cc amount (rest coins))
	    (cc (- amount (first coins)) coins))]))

(define (f w . x)
  x)

(define (same-parity f . r)
  (if
   (odd? f)
   (cons f (filter odd? r))
   (cons f (filter even? r))))








