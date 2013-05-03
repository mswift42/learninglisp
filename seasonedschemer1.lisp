(defun two-in-a-row-p (lst)
  (cond
    ((null lst) nil)
    ((equal (first lst) (second lst)) t)
    (t (two-in-a-row-p (rest lst)))))
