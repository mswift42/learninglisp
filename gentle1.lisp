(defun add1 (num)
  "ADD 1 to input"
  (+ num 1))

(defun add2 (num)
  (add1 (add1 num)))

(defun twop (num)
  (= num 2))

(defun onemorep (a b)
  (= a (add1 b)))

(defun twomorep (a b)
  ( = a (add2 b)))

(defun two2 (a b)
  (= b (- a 2)))

(defun average (a b)
  (/ (+ a b) 2))

(defun morethan-half (a b)
  (> a (/ b 2)))

(defun not-onep (a)
  (not (= a 1)))

(defun not-plusp (a)
  (not (> a 1)))

(defun my-third (l)
  (first (rest (rest l))))

(defun my-th2 (l)
  (second (rest l)))

(defun greeter ()
  (do 
   (prin1 "enter name")
   (let (greet (read-line))
     (prin1 greet))))

(defun say-what (l)
  (cons 'what (rest l)))

(defun fourin (a b c d)
  (cons (cons a b) (cons (cons  c d)nil)))

(defun duo-cons (a b l)
  (cons a (cons b l)))

(defun two-deeper (l)
  (list (list l)))

(defun two-deeper2 (l)
  (cons (cons l nil) nil))

(defun pythag (a b)
  (sqrt (+ (* a a) (* b b))))

(defun longer-than (l1 l2)
  (> (length l1) (length l2)))

(defun addlength (list)
  (cons (length list) list))

(defun scrabble (word)
  (list word 'is 'a 'word))

(defun make-even (num)
  (if (evenp num)
      num
      (1+ num)))

(defun further (num)
  (cond
    ((< num 0) (1- num))
    ((> num 0) (1+ num))
    (t 0)))

(defun my-abs (x)
  (if (< x 0) (- x) x))

(defun coin ()
  (let ((toss (random 101)))
    (cond
      ((< toss 50) 'tails)
      ((> toss 50) 'heads)
      (t 'edge))))

(defun throw-die ()
  (random 6))

(defun throw-dice ()
  (list (throw-die) (throw-die)))

(setf nerd-states '((sleeping eating)
		    (eating waiting-for-a-computer)
		    (waiting-for-a-computer programming)
		    (programming debugging)
		    (debugging sleeping)))

(defun nerdus (state)
  (second (assoc state nerd-states)))

(defun sleepless-nerd (state)
  (if (eql state 'debugging)
      'eating
      (nerdus state)))

(defun rem-last (list)
  (let* ((l (last list)))
    (remove l list)))

(setf x '(1 2 3 4 5))

(defun but-last (list)
  (cond
    ((null list) nil)
    ((null (rest list)) nil)
    (t  (cons (first list) (but-last (rest list))))))

(defun swap-first-last (list)
  (append (last list) (but-last (rest list)) (list (first list))))

(defun rotate-left (list)
  (append (rest list) (list (first list))))

(defun rotate-right (list)
  (append (last list) (but-last list)))


(setf member-table '((a b c d e)
		     (b c d e)
		     (c d e)
		     (d e)
		     (e e)))

(setf rooms
      '((living-room         (north front-stairs)
	                     (south dining-room)
	                     (east kitchen))
	(upstairs-bedroom    (west library)
	                     (south front-stairs))
	(dining-room         (north living-room)
	                     (east pantry)
	                     (west downstairs-bedroom))
	(kitchen             (west living-room)
	                     (south pantry))
	(pantry              (north kitchen)
	                     (west dining-room))
	(downstairs-bedroom  (north back-stairs)
	                     (east dining-room))
	(back-stairs         (south downstairs-bedroom)
	                     (north library))
	(front-stairs        (north upstairs-bedroom)
	                     (south living-room))
	(library             (east upstairs-bedroom)
	                     (south back-stairs))))

(defun choices (room)
  (rest (assoc room rooms)))

(defun look (direction room)
  (let ((c (choices room)))
    (loop for item in c
	  when (member direction item)
	  do (return (second item)))))

(setf *loc* nil)

(defun set-robbie-location (place)
  (setf *loc* place))

(defun how-many-choices ()
  (length (choices *loc*)))

(defun upstairsp (place)
  (or (eql place 'library) (eql place 'upstairs-bedroom)))

(defun onstairsp (place)
  (or (eql place 'front-stairs) (eql place 'back-stairs)))

(defun where ()
  (cond
    ((upstairsp *loc*) (list 'robbie 'is 'upstairs 'in 'the *loc*))
    ((onstairsp *loc*) (list 'robbie 'is 'on 'the *loc*))
    (t (list 'robbie 'is 'downstairs 'in 'the *loc*))))

(defun move (direction)
  (let ((newdir (look direction *loc*)))
    (if (null newdir)
	(list 'ouch 'robbie)
	(changedir newdir))))
      

(defun changedir (newdir)
  (progn
    (set-robbie-location newdir)
    (where)))

(defun royal-we (list)
  (subst  'we 'i list))

(setf cards '((3 clubs) (5 diamonds) (ace spades)))

(setf words
      '((one un)
	(two deux)
	(three trois)
	(four quatre)
	(five cinq)))

(defun translate (word)
  (second (assoc word words)))

(defun add1 (x)
  (1+ x))

(defun add1-list (list)
  (mapcar #'add1 list))

(setf daily-planet
      '((olsen jimmy 123-76-4535 cub-reporter)
	(kent clark 089-520676 reporter)
	(lane lois 951-26-1438 reporter)
	(white perry 355-16-7439 editor)))

(defun get-social-number (list)
  (mapcar #'(lambda (x) (third x)) list))

(defun all-nil (list)
  (mapcar #'zerop list))

(defun bigger-than-five (list)
  (mapcar #'(lambda (x) (> x 5)) list))

(defun minus-seven (list)
  (mapcar #'(lambda (x) (- x 7)) list))

(defun my-t (list)
  (mapcar #'(lambda (x) ((if x))) '(t 2)))

(defun flip (list)
  (mapcar #'(lambda (x) (if (equal x 'up) 'down 'up)) list))

(defun roughly (l x)
  (find-if #'(lambda (y) (<= (abs (- x y)) 10)) l))

(defun find-nested (l)
  (find-if #'(lambda (x) (not (null x))) l))

(setf note-table
      '((c 1)
	(c-sharp 2)
	(d 3)
	(d-sharp 4)
	(e 5)
	(f 6)
	(f-sharp 7)
	(g 8)
	(g-sharp 9)
	(a 10)
	(a-sharp 11)
	(b 12)))

(defun get-num (num)
  (second (assoc num note-table)))

(defun my-num (numberlist)
  (mapcar #'get-num numberlist))

(defun find-by-number (num)
  (first (find-if #'(lambda (x) (equal (second x) num)) note-table)))

(defun notes (l)
  (mapcar #'find-by-number l))

(defun raise ( a l)
  (mapcar #'(lambda (x) (+ x a))l))

(defun norm (x)
  (if (> x 12)
      (- x 12)
      x))

(defun normalize (l)
  (mapcar #'norm l))

(defun transpose (n song)
  (notes (normalize (raise n (my-num song)))))

(defun one-to-five (l)
  (remove-if-not #'(lambda (x) (and (< x 5) (> x 1))) l))

(defun count-the (l)
  (count 'the l))

(defun len-two (l)
  (remove-if-not #'(lambda (x) (= 2 (length x))) l))

(defun rank (card)
  (first card))

(defun suit (card)
  (second card))

(setf my-hand
      '((3 hearts)
	(5 clubs)
	(2 diamonds)
	(4 diamonds)
	(ace spades)))

(defun count-suit (suit hand)
  (length (remove-if-not #'(lambda (x) (equal suit (suit x))) hand)))

(setf colors
      '((clubs black)
	(diamonds red)
	(hearts red)
	(spades black)))

(defun color-of (hand)
  (second (assoc (suit hand) colors)))

(defun first-red (hand)
  (find-if #'(lambda (x) (equal 'red (color-of x))) hand))

(defun black-cards (hand)
  (remove-if-not #'(lambda (x) (equal 'black (color-of x))) hand))

(defun what-ranks (s hand)
  (mapcar #'rank (remove-if-not #'(lambda (x) (equal s (suit x))) hand)))

(setf all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun beforep (a b l)
  (member b (member a l)))

(defun higher-rank-p (c1 c2)
  (not (null ( beforep (rank c2) (rank c1) all-ranks))))

(defun higher-card (c1 c2)
  (if (higher-rank-p c1 c2) c1 c2))

(defun high-card (hand)
  (reduce #'higher-card hand))

(defun reduce-set (l)
  (reduce #'union l))

(defun sum-length-lists (l)
  (reduce #'+ (mapcar #'length l)))

(defun all-odd (l)
  (every #'oddp l))

(defun none-odd (l)
  (every #'evenp l))

(defun not-all-odd (l)
  (not (every #'oddp l)))

(defun not-none-odd (l)
  (not (none-odd l)))

(defun half (n)
  (* n 0.5))

(defun average (x y)
  (+
      (half x)
      (half y)))

(setf database
      '((b1 shape brick)
	(b1 color green)
	(b1 size small)
	(b1 supported-by b2)
	(b1 supported-by b3)
	(b2 shape brick)
	(b2 color red)
	(b2 size small)
	(b2 supports b1)
	(b2 left-of b3)
	(b3 shape brick)
	(b3 color red)
	(b3 size small)
	(b3 supports b1)
	(b3 right-of b2)
	(b4 shape pyramid)
	(b4 color blue)
	(b4 size large)
	(b4 supported-by b5)
	(b5 shape cube)
	(b5 color green)
	(b5 size large)
	(b5 supports b4)
	(b6 shape brick)
	(b6 color purple)
	(b6 size large)))

(defun match-element (s1 s2)
  (or (equal s1 s2) (equal s2 '?)))

(defun match-triple (a1 a2)
  (and (match-element (first a1) (first a2))
       (match-element (second a1) (second a2))
       (match-element (third a1) (third a2))))

(defun fetch (pattern)
  (remove-if-not #'(lambda (x) (match-triple x pattern)) database))

(defun shape-b4 ()
  (third (first ( fetch '(b4 shape ?)))))

(defun bricksp ()
  (fetch '(? shape brick)))

(defun b2-b3 ()
  (fetch '(b2 ? b3)))

(defun b4 ()
  (list (fetch '(b4 ? ?)) (fetch '(? ? b4))))

(defun get-pattern (b)
  (list b 'color '?))

(defun get-common-pattern (b p)
  (list b p '?))

(defun get-support-pattern (b)
  (list '? 'supports b))

(defun supporters (b)
  (mapcar #'first (fetch (get-support-pattern b))))

(defun cubep ()
  (mapcar #'first ( fetch '(? shape cube))))

(defun supp-cube (b)
  (intersection ( cubep  )(supporters b)))

(defun get-desc-pattern-a (b)
  (list b '? '?))

(defun get-desc-pattern-b (b)
  (list '? '? b))

(defun desc1 (b)
  (fetch (get-desc-pattern-a b)))

(defun desc2 (b)
  (mapcar #'rest (desc1 b)))

(defun description (b)
  (reduce #'append (desc2 b)))
