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

(defun descended-from (n1 n2)
  (or (member n2 (parents n1))
	     (member n2 (grandparents n1))))

(defun my-reverse (x)
  (cond ((null x) nil)
	(t (append (reverse (rest x))
		   (list (first x))))))

(defun tr-reverse (x)
  (tr-rev1 x nil))

(defun tr-rev1 (list result)
  (cond ((null list) result)
	(t (tr-rev1
	    (rest list)
	    (cons (first list) result)))))


(defun count-up (num)
  (cond
    ((zerop num) nil)
    (t (append (count-up (1- num)) (list num)))))

(defun count-up-tr (num)
  (count-up-helper num nil))

(defun count-up-helper (num result)
  (cond
    ((zerop num) result)
    (t (count-up-helper (1- num) (append (list num) result )))))


(defun fac-rec (num)
  (fac-rec-helper num 1))

(defun fac-rec-helper (num result)
  (cond
    ((zerop num) result)
    (t ( fac-rec-helper (1- num) (* result num)))))

(defun arith-value (list)
  (cond
    ((null list) nil)
    ((numberp (first list)) (cons (list (second list) (first list)) (arith-value (cadr list))))
    (t (cons (arith-value (first list)) (arith-value (rest list))))))


(defun square-talk (n)
  (format t "~&~d squared is ~d" n (* n n)))

(defun test (string)
  (format t "~&With escape characters: ~S" string)
  (format t "~&Without escape characters: ~A" string))

(defun print-pilot ()
  (format t "~&There are old pilots,")
  (format t "~&and there are bold pilots,")
  (format t "~&but there are no old bold pilots."))

(defun draw-line (n)
  (loop
       for i from 1 to n
       do (format t "*")))

(defun draw-line-rec (n)
  (if
   (zerop n) nil
   (progn
     (format t "*")
     (draw-line-rec (1- n)))))

(defun draw-box (line row)
  (loop
       for i from 1 to row
       do (format t "~%")
       (loop
	    for j from 1 to line
	    do (format t "*"))))

(defun ninety-nine (n)
  (if
   (zerop n) nil
   (progn
     (format t "~&There are ~d bottles of beer on the wall" n)
     (format t "~&~d bottles of beer." n)
     (ninety-nine (1- n)))))

(defun hourly-pay ()
  (format t "Please type in hourly wage ")
  (let ((wage (read)))
    (format t "~&Enter hours worked ")
    (let ((hours (read)))
      (format t "~&Your wage is ~d:~%" (* wage hours)))))




(defun get-tree-data ()
  (with-open-file (stream "timber.dat")
    (let* ((tree-loc (read stream))
	   (tree-table (read stream))
	   (num-trees (read stream)))
      (format t "~& There are ~S trees on ~S."
	      num-trees tree-loc)
      (format t "~&They are :   ~S" tree-table))))

(defun space-over-rec (n)
  (cond
    ((< n 0) (format t "Error!"))
    ((zerop n) (format t ""))
    (t (progn
	 (format t " ")
	 (space-over-rec (1- n))))))

(defun test (n)
  (format t "~%>>>")
  (space-over-rec n)
  (format t "<<<"))

(defun plot-one-point (plotting-string y-val)
  (space-over-rec y-val)
  (format t plotting-string)
  (format t "~%"))

(defun plot-points (plotting-string valuelist)
  (loop
       for i in valuelist
       do (plot-one-point plotting-string i)))

(defun plot-points-2 (plotting-string valuelist)
  (mapcar #'(lambda (x) (plot-one-point plotting-string x)) valuelist))

(defun generate (m n)
  (loop
       for i from m to n collecting i))

(defun make-graph (func start end plotting-string)
  (plot-points plotting-string (mapcar func (generate start end))))

(defun square (n)
  (* n n))


(setf *total-glasses* 0)

(defun sell (n)
  "ye olde lemonade stand: Sales by the glass."
  (incf *total-glasses* n)
  (format t
	  "~&That makes ~S glasses so far today."
	  *total-glasses*))

(defun len (lst)
  (labels ((rev (lst acc)
	     (if (null lst) acc
		 (rev (rest lst) (1+ acc)))))
    (rev lst 0)))

(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
	((equal v 10) "X")
	(t " ")))

(defun print-row (x y z)
  (format t "~&   ~A | ~A | ~A"
	  (convert-to-letter x)
	  (convert-to-letter y)
	  (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&   ----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&   ----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(setf b (make-board))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(setf *computer* 10)
(setf *opponent* 1)

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)
	(1 4 7) (2 5 8) (3 6 9)
	(1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet)  board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
	      (sum-triplet board triplet)) *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move
		     *opponent*
		     pos
		     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format t "~&You win!"))
	  ((board-full-p new-board)
	   (format t "~&Tie game."))
	  (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
		     (<= 1 pos 9)))
	   (format t "~&Invalid input.")
	   (read-a-legal-move board))
	   ((not (zerop (nth pos board)))
	    (format t
		    "~&That space is already occupied.")
	    (read-a-legal-move board))
	   (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format t "~&I win!"))
	  ((board-full-p new-board)
	   (format t "~&Tie game."))
	  (t (opponent-move new-board)))))

(defun choose-best-move (board)
  (random-move-strategy board))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
	"random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
	pos
	(pick-random-empty-position board))))


(defun chop-1 (lst)
  (first lst))

(defun find-first-odd (lst)
  (dolist (e lst)
    (format t "~&Testing ~S...." e)
    (when (oddp e)
      (format t "found an odd number.")
      (return e))))

(defun check-all-odd (lst)
  (dolist (e lst t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e)) (return nil))))

(defun do-member (elem lst)
  (dolist (e lst nil)
    (format t "~&Checking ~S..." e)
    (if (equal e elem) (return t))))

(defun it-assoc (elem lst)
  (dolist (x lst nil)
    (format t "~&Checking ~S..." x)
    (if (member elem x) (return x))))

(defun check-all-odd-rec (lst)
  (if
   (null lst)
   nil
   (and (oddp (first lst))
	(check-all-odd-rec (rest lst)))))

(defun it-length (lst)
  (loop
       for i in lst count i))

(defun it-nth (lst n)
  (loop
       for i in lst and a = 0 then (1+ a)
       when (= n a) return i))

(defun launch (n)
  (do ((cnt n (1- cnt)))
      ((zerop cnt) (format t "Blast off!"))
    (format t "~S..." cnt)))

(defstruct starship
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))

(setf s2 '#s(starship speed (warp 3)
		       condition red
		       shields up))

(setf s3 '#s(starship name hoden
		      speed fast
		      condition black-metal
		      shields super))
  
(defun alert (x)
  (setf (starship-shields x) 'up)
  (if (equal (starship-condition x) 'black-metal)
      (setf (starship-condition x) 'death-metal))
  'shields-raised)


(defmacro simple-incf (var)
  (list 'setq var (list '+ var 1)))

(defmacro set-nil (var)
  `(setq ,var nil))


