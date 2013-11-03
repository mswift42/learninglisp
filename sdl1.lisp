(ql:quickload '(:lispbuilder-sdl :cl-opengl))

(defpackage #:sdl1
  (:use :cl :lispbuilder-sdl ))

(in-package #:sdl1)
(defparameter *random-color* sdl:*white*)
(defun mouse-rect-2d ()
  (sdl:with-init ()
    (sdl:window 500 600 :title-caption "Move a rectangle using the mouse")
    (setf (sdl:frame-rate) 60)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
       (sdl:push-quit-event))
      (:idle ()
       ;; Change the color of the box if the left mouse button is depressed
       (when (sdl:mouse-left-p)
         (setf *random-color* (sdl:color :r (random 255) :g (random 255) :b (random 255))))
       
       ;; Clear the display each game loop
       
       
       ;; Draw the box having a center at the mouse x/y coordinates.
       
       

       ;; Redraw the display
       (sdl:update-display)))))




(defun opengl-test-1 ()
  (sdl:with-init ()
    (sdl:window 250 250
                :title-caption "OpenGL Example"
                :icon-caption "OpenGL Example"
                :opengl t
                :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (gl:clear-color 0 0 0 0)
    ;; Initialize viewing values.
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -1 1)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
       (gl:clear :color-buffer-bit)
       ;; Draw white polygon (rectangle) with corners at
       ;; (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0).
       (gl:color 1 1 1)
       (gl:with-primitive :polygon
                          (gl:vertex 0.25 0.25 0)
                          (gl:vertex 0.75 0.25 0)
                          (gl:vertex 0.75 0.75 0)
                          (gl:vertex 0.25 0.75 0))
       ;; Start processing buffered OpenGL routines.
       (gl:flush)
       (sdl:update-display)))))

(defun sdl-2-2 ()
  (sdl:with-init ()
    (sdl:window 700 800 :title-caption "brueste sind am besten!")
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event t)
      (:key-down-event (:key key)
		       (when (sdl:key= key (or  :sdl-key-escape :sdl-key-space))
			 (sdl:push-quit-event)))
      (:idle ()
       (sdl:draw-string-solid-* "pimmel" 20 20)))))


