
;; Example: Counting change
;; It takes only a bit of cleverness to come up with the iterative Fibonacci al-
;; gorithm. In contrast, consider the following problem: How many differ-
;; ent ways can we make change of $1.00, given half-dollars, quarters, dimes,
;; nickels, and pennies? More generally, can we write a procedure to compute
;; the number of ways to change any given amount of money?
;; This problem has a simple solution as a recursive procedure. Suppose
;; we think of the types of coins available as arranged in some order. Then
;; the following relation holds:
;; The number of ways to change amount a using n kinds of coins equals
;; • the number of ways to change amount a using all but the first kind of
;; coin, plus
;; • the number of ways to change amount a − d using all n kinds of coins,
;; where d is the denomination of the first kind of coin.
;; To see why this is true, observe that the ways to make change can be di-
;; vided into two groups: those that do not use any of the first kind of coin,
;; and those that do. Therefore, the total number of ways to make change
;; for some amount is equal to the number of ways to make change for the
;; amount without using any of the first kind of coin, plus the number of ways
;; to make change assuming that we do use the first kind of coin. But the lat-
;; ter number is equal to the number of ways to make change for the amount
;; that remains after using a coin of the first kind.
;; Thus, we can recursively reduce the problem of changing a given
;; amount to the problem of changing smaller amounts using fewer kinds of
;; coins. Consider this reduction rule carefully, and convince yourself that we69
;; can use it to describe an algorithm if we specify the following degenerate
;; cases: 33
;; • If a is exactly 0, we should count that as 1 way to make change.
;; • If a is less than 0, we should count that as 0 ways to make change.
;; • If n is 0, we should count that as 0 ways to make change.

(defparameter *denoms*
  '(0.5 0.25 0.10 0.5 0.1))

(defun ways-to-change (amount denoms)
  "how many ways can you make change for amount with the 
   given denoms.")
