(ql:quickload '(:alexandria :com.clearly-useful.protocols :optima))

(defpackage :monad
  (:use :cl
        :alexandria
        :optima
        :com.clearly-useful.protocols))

(in-package :monad)

(shadow 'do)
(shadow 'return)

(defvar *monad* nil)

(defprotocol monad
  (bind-to   (self mv mf))
  (return-to (self v)))

(defun >>= (mv mf)
  (bind-to *monad* mv mf))

(defun return (v)
  (return-to *monad* v))


(defmacro do (monad (&rest bindings) &rest forms)
  `(let ((*monad* ,monad))
     ,(thread-bindings
       (append bindings
               (list `(return (progn ,@forms)))))))

(defun thread-binding (pair acc)
  (destructuring-bind (sym val) pair
    `(>>= ,val (lambda (,sym) ,acc))))

(defun thread-bindings (bindings)
  (reduce 'thread-binding bindings :from-end t))

(extend-object 'list
  monad
  (bind-to (_ mv mf)
           (mappend mf mv))
  (return-to (_ v) (list v)))

(extend-object 'maybe
  monad
  (bind-to (_ mv mf)
           (match mv
             ((cons 'just x) (funcall mf x))
             ('nothing 'nothing)))
  (return-to (_ v)
             `(just . ,v)))

(extend-object t
  monad
  (bind-to (_ mv mf)
           (when mv
             (funcall mf mv)))
  (return-to (_ v)
             v))

(extend-type function
  monad
  (bind-to (self mv mf)
           (when (funcall self mv)
             (funcall mf mv)))
  (return-to (_ v) v))
