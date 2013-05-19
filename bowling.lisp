;; This description is based on that at www.xprogramming.com/xpmag/acsBowling.htm

;; Problem Description

;; Create a program, which, given a valid sequence of rolls for one line of American Ten-Pin Bowling, produces the total score for the game. Here are some things that the program will not do:

;;     We will not check for valid rolls.
;;     We will not check for correct number of rolls and frames.
;;     We will not provide scores for intermediate frames. 

;; Depending on the application, this might or might not be a valid way to define a complete story, but we do it here for purposes of keeping the kata light. I think you'll see that improvements like those above would go in readily if they were needed for real.

;; We can briefly summarize the scoring for this form of bowling:

;;     Each game, or "line" of bowling, includes ten turns, or "frames" for the bowler.
;;     In each frame, the bowler gets up to two tries to knock down all the pins.
;;     If in two tries, he fails to knock them all down, his score for that frame is the total number of pins knocked down in his two tries.
;;     If in two tries he knocks them all down, this is called a "spare" and his score for the frame is ten plus the number of pins knocked down on his next throw (in his next turn).
;;     If on his first try in the frame he knocks down all the pins, this is called a "strike". His turn is over, and his score for the frame is ten plus the simple total of the pins knocked down in his next two rolls.
;;     If he gets a spare or strike in the last (tenth) frame, the bowler gets to throw one or two more bonus balls, respectively. These bonus throws are taken as part of the same turn. If the bonus throws knock down all the pins, the process does not repeat: the bonus throws are only used to calculate the score of the final frame.
;;     The game score is the total of all frame scores. 

;; More info on the rules at: www.topendsports.com/sport/tenpin/scoring.htm

;; Clues

;; What makes this game interesting to score is the lookahead in the scoring for strike and spare. At the time we throw a strike or spare, we cannot calculate the frame score: we have to wait one or two frames to find out what the bonus is.

;; Suggested Test Cases

;; (When scoring "X" indicates a strike, "/" indicates a spare, "-" indicates a miss)

;;     "XXXXXXXXXXXX" (12 rolls: 12 strikes) = 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 = 300
;;     "9-9-9-9-9-9-9-9-9-9-" (20 rolls: 10 pairs of 9 and miss) = 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 = 90
;;     "5/5/5/5/5/5/5/5/5/5/5" (21 rolls: 10 pairs of 5 and spare, with a final 5) = 10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5 = 150 
(load "~/quicklisp/setup.lisp")
(ql:quickload "lisp-unit")


(lisp-unit:define-test test-sum-score
  (lisp-unit:assert-equal 300 (sum-score
			       (build-sublists (convert-to-list
						"xxxxxxxxxxxx"))))
  (lisp-unit:assert-equal 10 (sum-score
			      (build-sublists (convert-to-list
					       "1-1-1-1-1-1-1-1-1-1-"))))
  (lisp-unit:assert-equal 150 (sum-score
			       (build-sublists (convert-to-list
						"5/5/5/5/5/5/5/5/5/5/5"))))
  (lisp-unit:assert-equal 68 (sum-score
			      (build-sublists (convert-to-list
					       "x455-5-5-5-5-5-5-5-")))))

(lisp-unit:define-test test-strike-p
  (lisp-unit:assert-true (strike-p #\x))
  (lisp-unit:assert-false (strike-p #\-))
  (lisp-unit:assert-false (strike-p 4)))

(lisp-unit:define-test test-spare-p
  (lisp-unit:assert-true (spare-p #\/))
  (lisp-unit:assert-false (spare-p #\x))
  (lisp-unit:assert-false (spare-p 3)))


(lisp-unit:define-test test-convert-to-list
  (lisp-unit:assert-equal '(7 #\/ 6 #\/ 8 #\/ ) (convert-to-list "7/6/8/")))

(lisp-unit:define-test test-build-sublists
  (lisp-unit:assert-equal '((3 4) (4 #\/) (#\x) (4 5) (#\- 4))
			  (build-sublists '(3 4 4 #\/ #\x 4 5 #\- 4))))

(lisp-unit:define-test test-single-score
  (lisp-unit:assert-equal 10 (single-score '(#\x)))
  (lisp-unit:assert-equal 10 (single-score '(3 #\/)))
  (lisp-unit:assert-equal 7  (single-score '(3 4)))
  (lisp-unit:assert-equal 8  (single-score '(9 #\-))))

(lisp-unit:define-test test-simple-score
  (lisp-unit:assert-equal 40 (simple-score '((4 #\/) (3 3) (#\x) (4 #\-)
					     (2 5) (2 1))))
  (lisp-unit:assert-equal 20 (simple-score '((2 #\/) (#\- 5) (#\- 5)))))

(defun strike-p (char)
  (equal char #\x))

(defun spare-p (char)
  (equal char #\/))

(defun convert-to-number (char)
  (if (digit-char-p char)
      (digit-char-p char)
      char))

(defun convert-to-list (string)
  (loop
       for char across string
       collect (convert-to-number char)))

(defun build-sublists (lst)
  (cond
    ((null lst) nil)
    ((strike-p (first lst))
     (cons (cons (first lst) nil) (build-sublists (rest lst))))
    (t (cons (list (first lst) (second lst) ) (build-sublists (cddr lst))))))

(defun simple-score (lst)
  (reduce #'+ (mapcar #'single-score lst)))

(defun single-score (lst)
  (cond
    ((strike-p (first lst)) 10)
    ((spare-p (second lst)) 10)
    (t (reduce #'+ lst))))

(defparameter *testlist1* (build-sublists (convert-to-list "xxxxxxxxxx")))

(defun sum-score (lst)
  (cond
    ((null lst) 0)
    ((and (strike-p (first (first lst)))
	  (strike-p (first (second lst))))
     (+ 10 (single-score (first (first lst)))
	   (single-score (first (second lst)))
	   (sum-score (rest lst))))
    ((strike-p (first (first lst)))
     (+ 10 (single-score (second lst)) (sum-score (rest lst))))
    ((spare-p (first lst)) (+ 10 (if (strike-p (first (second lst)))
				     10
				     (first (second lst)))
			      (sum-score (rest lst))))
    (t (+ (single-score (first lst)) (sum-score (rest lst))))))


(defun score (string)
  (sum-score (build-sublists (convert-to-list string))))
