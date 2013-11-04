#lang racket

(require rackunit)

(define (triangle length height)
  (/ (* length height) 2))

(define (convert3 a b c)
  (+ (+ ( * a 100) (* b 10)) c))

(check-equal? 123 (convert3 1 2 3))
(check-equal? 245 (convert3 2 4 5))

(define (tax pay)
  (* pay 0.15))

(define (sum-coins pennies nickels dimes quarters)
  (+ pennies
     (* nickels 5)
     (* dimes 10)
     (* quarters 25)))

(define (total-profit attendees)
  (- (* attendees 5)
     (* attendees 0.5)
     20))

(define (profit ticket-price)
  "profit : number -> number
   to compute the profit as the difference between revenue
   and costs at some given ticket price."
  (- (revenue ticket-price)
     (cost ticket-price)))

(define (revenue ticket-price)
  "revenue : number -> number
   to compute the revenue, given the ticket price."
  (* (attendees ticket-price) ticket-price))

(define (cost ticket-price)
  "cost : number -> number
   to compute the costs, given ticket-price."
  (+ 180
     (* (attendees ticket-price) 0.04)))

(define (attendees ticket-price)
  "attendees : number -> number
   to compute the number of attendees, given ticket price.
   (attendees 5.00) -> 120"
  (+ 120
     (* (- 5.00 ticket-price)
        (/ 15 0.10))))

;;; 6.1 Structures.

(define-struct posn (a b))

(define-struct entry (name zip phone))

(define address (make-entry 'martin 2233 02020))

(define-struct jet-fighter (designation acceleration top-speed range))

(define (within-range jet distance)
  (>= (- (jet-fighter-range jet) distance )0))

(check-true (within-range (make-jet-fighter 'f22 2323 100 2000) 1800))


