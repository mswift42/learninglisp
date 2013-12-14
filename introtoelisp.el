(defun double-1 (number)
  "double number"
  (+ number number))

(defun double-2 (number)
  "double number using interactive"
  (interactive "p")
  (message "the result is: %d" (+ number number)))

(defun bigger-fill-p (arg)
  "test weather arg is bigger than val of fill-column"
  (if (> arg fill-column)
      (message "%d is bigger than than fill-column" arg)
    (message "%d is not bigger than fill-column" arg)))


(defun simplified-end-of-buffer ()
  "implement end-of-buffer funciton"
  (interactive)
  (push-mark)
  (goto-char (point-max)))

(defun buffer-p (buffer)
  "check whether buffer exists."
  (interactive "bEnter name of buffer: ")
  (if (get-buffer buffer)
      (message "Yep, buffer exists with the name %s" (get-buffer buffer))
    (message "No buffer does not exist")))

;; 5.5 Optional Argument Exercise
(defun compare-fill-column (&optional arg)
  (if arg
      (cond
       ((> arg fill-column) (message "argument is bigger than fill column"))
       ((= arg fill-column) (message "argument is equal to fill-column"))
       (t (message "argument is less than fill-column")))
    (cond
     ((> 56 fill-column) (message "56 is bigger than fill-column"))
     ((= 56 fill-column) (message "56 is equal to fill-column"))
     (t (message "56 is smaller than fill-column")))))

(defun compare-fill-column-2 (&optional arg)
  "version two of compare-fill-column. As before check for argument 
supplied. If no argument is supplied call function with 56 as argument."
  (if arg
      (cond
       ((> arg fill-column) (message "argument is bigger than fill-column"))
       ((= arg fill-column) (message "argument is equal to fill-column"))
       (t (message "argument is less than fill-column")))
    (compare-fill-column-2 56)))

(cl-defun compare-fill-column-3 (&optional (arg 56))
  "version three of compare-fill-column. supply default
  argument in lambda list. In order to use default argument 
  values, you have to use cl-defun in elisp."
  (cond
       ((> arg fill-column) (message "argument is bigger than fill-column"))
       ((= arg fill-column) (message "argument is equal to fill-column"))
       (t (message "argument is less than fill-column"))))

;; 6.3 Exercise with Narrowing
(defun show-first-sixty ()
  "display the first 60 characters of the current buffer."
  (interactive)
  (push-mark)
  (save-restriction
    (save-excursion
      (widen)
      (beginning-of-buffer)))
  (narrow-to-region 1 60))

;; 7.7 cons
(defun birdlist ()
  (list 'sparrow 'hawk 'eagle 'vulture)) 

(defun replace-first ()
  (let ((bl (birdlist)))
    (setcar bl 'trout)
    bl))


;; 8.7 Searching Exercises
(defun test-search (string)
  "If successful search of string, message Found!"
  (interactive "sEnter String: ")
   (when (search-forward string)
     (message "Found!")))


