(in-package :altform)

(unless (named-readtables:find-readtable 'example)
  (named-readtables:defreadtable example
    (:merge :standard)))


;;----------------------------------------------------------------
;;  hook into ccl's eval/compile,
;;  based on http://www.flownet.com/ron/lisp/combination-hook.lisp
;;
;;  note -- superceded by redefs.lisp
;;----------------------------------------------------------------

(defgeneric transform (object env)
  (:method (o env)
    (declare (ignore env))
    o))

(defgeneric splicing-form (object)
  (:method (o) o))

(defgeneric special-object-p (object)
  (:method (o) nil))

#+ (or) (defmacro hook (callback)
          `(destructuring-bind (form env) ccl::arglist
             (let ((new form ;(transform form env)
                     ))
               (if (eq new form)
                   (:do-it)
                   (apply ',callback (list new env))))))


(in-package :ccl)


(in-package :altform)

;;;;; an idea:
;;;; specialized packages, (similar to keyword)
;;;; such that taking the value of any symbol
;;;; in the package that is unbound calls a function
;;;; and sets it with that value. I think ccl
;;;;; already does this with the cocoa bridge,
;;;;; look into it.

;;;;; another idea:
;;;;; implement pjb's token customizer function
;;;;; there should be some code laying around that
;;;;; already pretty much does it.
;;;;; a good way to chain things would be
;;;;; if the fn returns it's argument (eq x (foo x))
;;;;; then it is passed up the call chain. although
;;;;; if you simply want to perform name translation
;;;;; i.e. aliasing, this could be an issue. perhaps
;;;;; better would be (values form done-p) where
;;;;; done-p indicates translation is finished.

#+ (or) (progn
          (ccl::advise ccl::nx1-transformed-form
	     (hook ccl::nx1-transformed-form)
	     :when :around
	     :name :transform)

          (ccl::advise ccl::cheap-eval-in-environment
                       (hook ccl::cheap-eval-in-environment)
                       :when :around
                       :name :transform))
