;;; Third Implementation of livescores app. This time in CL.

(load "~/quicklisp/setup.lisp")
(ql:quickload "ltk")
(ql:quickload "drakma")
(ql:quickload "rss")

(defpackage :myed
  (:use :ltk :cl :drakma ))

(in-package :myed)

(defparameter *leagues* '( ( bundesliga "http://rss.kicker.de/live/bundesliga") ( 2-Bundesliga "http://rss.kicker.de/live/2bundesliga") (Belgium "http://rss.kicker.de/live/jupilerleague") (austria "http://rss.kicker.de/live/tmobilebundesliga") ( champions-league "http://rss.kicker.de/live/championsleague") ( DFB-Pokal "http://rss.kicker.de/live/dfbpokal") ( Premier-League "http://rss.kicker.de/live/premierleague") ( England-Championship "http://rss.kicker.de/live/thecocacolafootballleaguechampionship") ( France "http://rss.kicker.de/live/thecocacolafootballleaguechampionship") (Italy "http://rss.kicker.de/live/serieatim") ( Spain "http://rss.kicker.de/live/primeradivision") ( Kicker-News "http://rss.kicker.de/news/aktuell") ( Scotland "http://rss.kicker.de/live/schottland") ( Switzerland "http://rss.kicker.de/live/axposuperleague") ( Netherlands "http://rss.kicker.de/live/eredivisie")  ))

(defun feedadress (league)
  "Return string of feed-url"
  (first (rest (assoc league *leagues*))))


(defun feedurl (league)
  "download the feedpage in a rss:parse-able format"
  (http-request (feedadress league) ))


(defun feedstr (target league)
  "Convert List of FeedLists to String with extralines after each 
   feeditem."
  (loop for i in (both-feed league)
        do (append-text target (format nil "~%~A~%~A~%" (first i) (second i)))))


(defun title-feed (league)
  "Return rss:title of feed"
  (map 'list  #'(lambda (x)  ( rss:title x)) (rss:items (rss:parse-rss-stream (feedurl league)))))

(defun description-feed (league)
  "Return description of feed"
  (map 'list #'(lambda (x) (rss:description x)) (rss:items (rss:parse-rss-stream (feedurl league)))))

(defun both-feed (league)
  "'ZIP' both title and description lists."
  (mapcar #'list (title-feed league) (description-feed league)))

(defun score-label (fr)
  (make-instance 'label :master fr :font "Sans-Serif" :text "Scores"))


(defun mainwindow ()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
	   (g (make-instance 'frame))
	   (scroll (make-instance 'scrolled-text :master g))
	   (textwin (textbox scroll))
	   (lb (make-instance 'listbox)))
      (pack f)
      (pack g :expand :t :fill :both :ipady 2)
      (configure textwin :font "monospaced" :background "#aea79f" :wrap :word)
      (pack (score-label f))
      (listbox-append lb (mapcar #'first *leagues*))
      (pack lb  :side :bottom :anchor :sw  :fill :x)
      (pack scroll  :expand :t :ipady 2 :fill :both)
      (bind lb "<<ListboxSelect>>" (lambda (event) (update-view textwin lb)))
      (configure f :borderwidth 4)
      (feedstr textwin 'kicker-news))))


(defun update-view (target source)
  "When league in listbox is selected, clear Textwindow. Because 
   listbox-get-selection only returns the index of its selection
   return the nth element of *leagues*"
  (clear-text target)
  (feedstr target (first (nth (first (listbox-get-selection source)) *leagues*))))






