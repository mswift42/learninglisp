(defun two-in-a-row-p (lst)
  (cond
    ((null lst) nil)
    ((equal (first lst) (second lst)) t)
    (t (two-in-a-row-p (rest lst)))))

(defun sum (lst)
  (reduce #'+ lst))

(defun sum-of-prefixes-helper (seensofar tup)
  (cond
    ((null tup) nil)
    (t (cons (+ seensofar (first tup))
	     (sum-of-prefixes-helper (+ seensofar (first tup)) (rest tup))))))

(defun sum-of-prefixes (lst)
  (sum-of-prefixes-helper 0 lst))

(defun pick (n lst)
  (if
   (= n 1) (first lst)
   (pick (1- n) (rest lst))))

(defun scramble-b (lst rev-pre)
  (cond
    ((null lst) nil)
    (t ( cons (pick (first lst)
		    (cons (first lst) rev-pre))
	      (scramble-b (rest lst) (cons
				      (first lst) rev-pre))))))

(defun scramble (lst)
  (scramble-b lst nil))

(defun multirember (n lst)
  (cond
    ((null lst) nil)
    ((equal n (first lst)) (multirember n (rest lst)))
    (t (cons (first lst) (multirember n (rest lst))))))

(defun multirember2 (n lst)
  (labels
      ((mr (lst))
       (cond
	 ((null lst) nil)
	 ((equal n (first lst)) (mr (rest lst)))
	 (t (cons (first lst) (mr (rest lst))))))
    mr lst))
