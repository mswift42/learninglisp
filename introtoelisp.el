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
  (interactive "P\nbEnter name of buffer: ")
  (if (get-buffer buffer)
      (message "Yep, buffer exists with the name %s" (get-buffer buffer))
    (message "No buffer does not exist")))




