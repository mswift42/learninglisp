;;; Second part in exercises in a gentle introduction to symbolic
;;; computation in Common Lisp.

;; Chapter 8.


(defun laugh (n)
  (cond
    ((zerop n) nil)
    (t (cons 'ha (laugh (1- n))))))

(defun add-up (list)
  (cond
    ((null list) 0)
    (t (+ (first list) (add-up (rest list))))))

(defun alloddp (list)
  (cond
    ((null list) t)
    (t (and (oddp (first list))
	    (alloddp (rest list))))))

(defun rec-member (a list)
  (cond
    ((equal a (first list)) list)
    (t (rec-member a (rest list)))))

(defun rec-assoc (a table)
  (cond
    ((null table) nil)
    ((equal a (first (first table))) (first table))
    (t (rec-assoc a (rest table)))))

(defun rec-nth (n list)
  (cond
    ((null list) nil)
    ((zerop n) (first list))
    (t (rec-nth (1- n) (rest list)))))

(defun add1 (n)
  (1+ n))

(defun sub1 (n)
  (1- n))

(defun rec-plus (a b)
  (cond
    ((zerop b) a)
    (t (rec-plus (add1 a) (sub1 b)))))

(defun fib-rec (n)
  (cond
    ((< n 2) 1)
    (t (+ (fib-rec (- n 2)) (fib-rec (- n 1))))))

(defun anyoddp (x)
  (cond
    ((null x) nil)
    ((oddp (first x)) t)
    (t (anyoddp (rest x)))))

(defun find-first-odd (list)
  (cond
    ((null list) nil)
    ((oddp (first list)) (first list))
    (t (find-first-odd (rest list)))))

(defun find-first-atom (x)
  (cond
    ((atom x) x)
    (t (find-first-atom (first x)))))

(defun add-nums (n)
  (cond
    ((zerop n) 0)
    (t (+ n (add-nums (1- n))))))

(defun all-equal (list)
  (cond
    ((< (length list) 2) t)
    (t (and (equal (first list) (second list))
	    (all-equal (rest list))))))

(defun count-down (n)
  (cond
    ((zerop n) nil)
    (t (cons n (count-down (1- n))))))

(defun fac-count-down (n)
  (reduce #'* (count-down n)))

(defun count-down-zero (n)
  (cond
    ((zerop n) (list 0))
    (t (cons n (count-down-zero (1- n))))))

(defun square-list (list)
  (cond
    ((null list) nil)
    (t (cons (* (first list) (first list))
	     (square-list (rest list))))))

(defun my-nth-modified (n list)
  (cond
    ((null list) nil)
    ((> n (length list)) nil)
    ((zerop n) (first list))
    (t (my-nth-modified (1- n) (rest list)))))

(setf family
      '((colin nil nil)
	(deirdre nil nil)
	(arthur nil nil)
	(kate nil nil)
	(frank nil nil)
	(linda nil nil)
	(suzanne colin deirdre)
	(bruce arthur kate)
	(charles arthur kate)
	(david arthur kate)
	(ellen arthur kate)
	(george frank linda)
	(hillary frank linda)
	(andre nil nil)
	(tamara bruce suzanne)
	(vincent bruce suzanne)
	(wanda nil nil)
	(ivan george ellen)
	(julie george ellen)
	(marie george ellen)
	(nigel andre hillary)
	(frederick nil tamara)
	(zelda vincent wanda)
	(joshua ivan wanda)
	(quentin nil nil)
	(robert quentin julie)
	(olivia nigel marie)
	(peter nigel marie)
	(erica nil nil)
	(yvette robert zelda)
	(diane peter erica)))

(defun father (name)
  (second (assoc name family)))

(defun mother (name)
  (third (assoc name family)))

(defun parents (name)
  (rest (assoc name family)))

(defun children (name)
  (mapcar #'first ( remove-if-not #'(lambda (x)
				      (or (equal (second x) name)
					  (equal (third x)  name))) family)))


(defun siblings (name)
  (remove name (union (children (father name)) (children (mother name)))))

(defun mapunion (fn list)
  (reduce #'union (mapcar fn list)))

(defun grandparents (name)
  (mapunion #'parents (parents name)))

(defun cousins (name)
  (mapunion #'children (mapunion #'siblings (parents name))))



