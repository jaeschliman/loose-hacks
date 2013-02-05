(ql:quickload '(:usocket :alexandria :bordeaux-threads :cl-js))

#|

  simple line-based socket server thingy.
  useful for one-off hacks, could def be improved

  if you load this file and run (sock::main)
  then $ nc localhost 7890
  you'll get a little echo server.
  quit with :q
  also there's this for emacs:

  (defun nc (name port)
    (interactive "sName:\nsPort:")
    (switch-to-buffer (make-comint name "nc"  nil "localhost" port)))

|#

(defpackage :sock
  (:use :cl :alexandria))
(in-package :sock)

(defvar *server* nil)
(defvar *sock* nil)

(defun start (port)
  (setf *server* (usocket:socket-listen "127.0.0.1" port)))

(defun connect ()
  (setf *sock* (usocket:socket-accept *server*)))

(defun readln ()
  (read-line (usocket:socket-stream *sock*) nil nil))

(defun listener (prompt fn)
  (lambda ()
    (let (*sock* (forever t))
      (connect)
      (write-string (funcall prompt) (usocket:socket-stream *sock*))
      (force-output (usocket:socket-stream *sock*))
      (loop
         while forever
         with stream = (usocket:socket-stream *sock*)
         for line = (readln)
         while line
         do (progn 
              (force-output stream)
              (if (string= line ":q")
                  (setf forever nil)
                  (progn
                    (funcall fn stream line)
                    (write-string (funcall prompt) stream)
                    (force-output stream)) ))
         finally (usocket:socket-close *sock*)))))

(defun echo (stream string)
  (format stream "<<~A>>~%" string))

(defun package-directory-prompt ()
  (format nil "(~A)~%~A>"
          (package-name *package*)
          (truename *default-pathname-defaults*)))



(defun spawn-fns (prompt fn)
  (bordeaux-threads::make-thread
   (listener prompt
             fn)
   :name (format nil "spawned function ~S ~S" prompt fn)))

;(spawn-fns 'package-directory-prompt 'echo)

(defun show (thing)
  (format nil "~A" thing))

(defun js-prompt ()
  (format nil "JS>"))

(defun js-run (stream string)
  (let ((s
         (handler-case
             (let ((*standard-output* stream))
               (cl-js:to-string (cl-js:run-js string)))
           (error (it) (show it))
           (condition (it) (show it))
           (cl-js:js-condition (it) (show it)))))
    (write-string s stream)
    (terpri stream)))


(defun main ()
  (start 7890)
  #+nil(spawn-fns 'js-prompt 'js-run)
  (spawn-fns 'package-directory-prompt 'echo))



