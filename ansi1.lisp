(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(defun len (lst)
  (labels ((rec (lst acc)
	     (if (null lst)
		 acc
		 (rec (rest lst) (+ acc 1)))))
    (rec lst 0)))

(defun fac (n)
  (labels ((rec (n acc)
	     (if (< n 2)
		 acc
		 (rec (1- n) (* n acc)))))
    (rec n 1)))

(defun occurrences (list)
  "return list indicating the number of occurrences of each
   item, sorted from most to least. (occurrences '(a a b c c)) ->
   '((a . 2) (c . 2) (b . 1))"
  (let ((result))
    (loop for i in list
	  do (pushnew (cons i (count i list)) result)
	  finally (return (sort (reverse result) #'> :key #'cdr)))))
