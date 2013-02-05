(in-package :altform)

(defun make-simple-syntax-class (&key
                                   class
                                   lbracket
                                   rbracket
                                   does-sharp-dispatch)
  (let ((make-class (intern (concatenate 'string
                                         (string 'make-)
                                         (string class))))
        (read-class (intern (concatenate 'string
                                         (string 'read-)
                                         (string class)))))
    `(progn
       (defclass ,class ()
         ((val :initarg :val)))

       (defun ,make-class (&rest args)
         (make-instance ',class :val (copy-list args)))

       (defmethod print-object ((a ,class) stream)
         (when ,does-sharp-dispatch
           (write-char #\# stream))
         (write-char ,lbracket stream)
         (let ((v (slot-value a 'val)))
           (when v
             (print-object (car v) stream)
             (dolist (o (cdr v))
               (write-char #\Space stream)
               (print-object o stream))))
         (write-char ,rbracket stream))

       (defun ,read-class (stream char)
         (declare (ignore char))
         (apply ',make-class (read-delimited-list ,rbracket stream t)))

       (if ,does-sharp-dispatch
           (set-dispatch-macro-character #\# ,lbracket (lambda (a b c)
                                                         (declare (ignore c))
                                                         (,read-class a b)))
           (set-macro-character ,lbracket ',read-class))
       (set-macro-character ,rbracket (get-macro-character #\) nil))

       (defmethod transform ((object ,class) env)
         (declare (ignore env))
         (cons ',make-class (slot-value object 'val)))

       (defmethod special-object-p ((object ,class))
         t)

       (defmethod splicing-form ((object ,class))
         (list 'quote (seq-to-list (seq object)))))))


(defmacro simple-syntax-class (name lbracket rbracket &optional does-sharp-dispatch)
  (assert (and
           (typep name 'symbol)
           (typep lbracket 'character)
           (typep rbracket 'character)
           (typep does-sharp-dispatch 'boolean)))
  (make-simple-syntax-class :class name
                            :lbracket lbracket
                            :rbracket rbracket
                            :does-sharp-dispatch does-sharp-dispatch))


