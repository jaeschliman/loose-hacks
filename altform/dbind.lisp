(in-package :altform)

(defvar *hole* (cons nil nil))

(defgeneric binding-form (left right))

(defmethod binding-form ((a symbol) value)
  `(let ((,a ,value)) ,*hole*))

(defmethod binding-form ((a cons) value)
  (let (s f)
    (loop
     for form in a
     with g = nil
     if (symbolp form)
      collect form into syms
     else
      do (setf g (gensym))
      and collect g into syms
      and collect (binding-form form g) into forms
     finally (setf s syms f forms))
    (thread-binding-forms `((multiple-value-bind ,s ,value ,*hole*)
                                    ,@f))))

(defmethod binding-form ((a syntax-vector) value)
  (let* ((vs (seq-to-list (seq a)))
         (s (gensym))
         (initial (binding-form s `(seq ,value))))
    (thread-binding-forms
     (cons initial
           (loop
              for (v . more) on vs
              appending (let ((next (gensym)))
                          (prog1 (if more
                                     (list
                                      (binding-form v `(head ,s))
                                      (binding-form next `(tail ,s)))
                                     (list (binding-form v `(head ,s)))) 
                            (setf s next))))))))

(defun simple-let-p (l)
  (and
   (eq 'let (car l))
   (consp (second l))
   (= 1 (length (second l)))
   (= 2 (length (first (second l))))))

(defun let*-p (l)
  (and
   (eq 'let* (car l))
   (consp (second l))))

(defun let-p (l)
  (or (simple-let-p l)
      (let*-p l)))

(defun combine-lets (a b)
  `(let* (,@(second a)
          ,@(second b))
     ,*hole*))

(defun simplify-lets (list)
  (reduce (lambda (a acc)
            (if acc
                (let ((b (car acc)))
                  (if (and (let-p a) (let-p b))
                      (cons (combine-lets a b)
                            (cdr acc))
                      (cons a acc)))
                (list a)))
          list :initial-value nil
          :from-end t))

(defun thread-binding-forms (forms)
  (reduce (lambda (v acc)
            (subst v *hole* acc)) (reverse (simplify-lets forms))
            :from-end t))


(defun binding-pairs (bindings)
  (let ((s (seq-to-list bindings)))
    (thread-binding-forms
     (loop for (b v) on s by #'cddr
        collect (binding-form b v)))))

(defun translate-dbind (bindings body)
  (subst `(progn ,@body) *hole* (binding-pairs bindings)))

(defmacro dbind (bindings &body body)
  (translate-dbind bindings body))
