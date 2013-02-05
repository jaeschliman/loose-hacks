(in-package :altform)


(in-package :ccl)

(let ((*warn-if-redefine-kernel* nil))
  
  (defun cheap-eval-in-environment (form env &aux sym)
    ;; Allow ADVICE, TRACE to have effects on self-calls.
    (declare (notinline cheap-eval-in-environment))
    ;; records source locations if *nx-source-note-map* is bound by caller
    (setq *loading-toplevel-location* (or (nx-source-note form) *loading-toplevel-location*))
    (flet ((progn-in-env (body&decls parse-env base-env)
             (multiple-value-bind (body decls) (parse-body body&decls parse-env)
               (setq base-env (augment-environment base-env :declare (decl-specs-from-declarations decls)))
               (loop with default-location = *loading-toplevel-location*
                  while (cdr body) as form = (pop body)
                  do (cheap-eval-in-environment form base-env)
                  do (setq *loading-toplevel-location* default-location))
               (cheap-eval-in-environment (car body) base-env))))
      (if form
          (cond ((symbolp form) 
                 (multiple-value-bind (expansion win) (cheap-eval-macroexpand-1 form env)
                   (if win 
                       (cheap-eval-in-environment expansion env)
                       (let* ((defenv (definition-environment env))
                              (constant (if defenv (assq form (defenv.constants defenv))))
                              (constval (%cdr constant)))
                         (if constant
                             (if (neq (%unbound-marker-8) constval)
                                 constval
                                 (error "Can't determine value of constant symbol ~s" form))
                             (if (constant-symbol-p form)
                                 (%sym-global-value form)
                                 (symbol-value form)))))))
                ;; --->     ------> ---->  <-----
                ((atom form)                          ; here <------
                 (if (altform::special-object-p form) ; <----
                     (cheap-eval-in-environment       ; <---  Look
                      (altform::transform form nil) env)
                     form))
                ((eq (setq sym (%car form)) 'quote)
                 (verify-arg-count form 1 1)
                 (%cadr form))
                ((eq sym 'function)
                 (verify-arg-count form 1 1)
                 (cond ((symbolp (setq sym (%cadr form)))
                        (multiple-value-bind (kind local-p)
                            (function-information sym env)
                          (if (and local-p (eq kind :macro))
                              (error "~s can't be used to reference lexically defined macro ~S" 'function sym)))
                        (%function sym))
                       ((setf-function-name-p sym)
                        (multiple-value-bind (kind local-p)
                            (function-information sym env)
                          (if (and local-p (eq kind :macro))
                              (error "~s can't be used to reference lexically defined macro ~S" 'function sym)))
                        (%function (setf-function-name (%cadr sym))))
                       (t (cheap-eval-function nil sym env))))
                ((eq sym 'nfunction)
                 (verify-arg-count form 2 2)
                 (cheap-eval-function (%cadr form) (%caddr form) env))
                ((eq sym 'progn) (progn-in-env (%cdr form) env env))
                ((eq sym 'setq)
                 (if (not (%ilogbitp 0 (list-length form)))
                     (verify-arg-count form 0 0)) ;Invoke a "Too many args" error.
                 (let* ((sym nil)
                        (val nil)
                        (original form))
                   (while (setq form (%cdr form))
                     (setq sym (require-type (pop form) 'symbol))
                     (multiple-value-bind (expansion expanded)
                         (cheap-eval-macroexpand-1 sym env)
                       (if expanded
                           (setq val (cheap-eval-in-environment
                                      (cheap-eval-transform original `(setf ,expansion ,(%car form)))
                                      env))
                           (set sym (setq val (cheap-eval-in-environment (%car form) env))))))
                   val))
                ((eq sym 'eval-when)
                 (destructuring-bind (when . body) (%cdr form)
                   (when (or (memq 'eval when) (memq :execute when)) (progn-in-env body env env))))
                ((eq sym 'if)
                 (destructuring-bind (test true &optional false) (%cdr form)
                   (setq test (let ((*loading-toplevel-location* *loading-toplevel-location*))
                                (cheap-eval-in-environment test env)))
                   (cheap-eval-in-environment (if test true false) env)))
                ((eq sym 'locally) (progn-in-env (%cdr form) env env))
                ((eq sym 'symbol-macrolet)
                 (multiple-value-bind (body decls) (parse-body (cddr form) env)
                   (progn-in-env body env (augment-environment env :symbol-macro (cadr form) :declare (decl-specs-from-declarations decls)))))
                ((eq sym 'macrolet)
                 (let ((temp-env
                        (augment-environment
                         env :macro 
                         (mapcar #'(lambda (m)
                                     (destructuring-bind (name arglist &body body) m
                                       (list name (enclose (parse-macro name arglist body env)
                                                           env))))
                                 (cadr form)))))
                   (progn-in-env (cddr form) temp-env temp-env)))
                ((and (symbolp sym) 
                      (compiler-special-form-p sym)
                      (not (functionp (fboundp sym))))
                 (if (eq sym 'unwind-protect)
                     (destructuring-bind (protected-form . cleanup-forms) (cdr form)
                       (unwind-protect
                            (let ((*loading-toplevel-location* *loading-toplevel-location*))
                              (cheap-eval-in-environment protected-form env))
                         (progn-in-env cleanup-forms env env)))
                     (let ((fn (cheap-eval-function nil (cheap-eval-transform form `(lambda () (progn ,form))) env)))
                       (funcall fn))))
                ((and (symbolp sym) (macro-function sym env))
                 (cheap-eval-in-environment (cheap-eval-macroexpand-1 form env) env))
                ((or (symbolp sym)
                     (and (consp sym) (eq (%car sym) 'lambda)))
                 (let ((args nil) (form-location *loading-toplevel-location*))
                   (dolist (elt (%cdr form))
                     (push (cheap-eval-in-environment elt env) args)
                     (setq *loading-toplevel-location* form-location))
                   (apply #'call-check-regs (if (symbolp sym) sym (cheap-eval-function nil sym env))
                          (nreverse args))))
                (t
                 (signal-simple-condition 'simple-program-error "Car of ~S is not a function name or lambda-expression." form))))))

  (defun nx-transform (form &optional (environment *nx-lexical-environment*) (source-note-map *nx-source-note-map*))
    (macrolet ((form-changed (form)
                 `(progn
                    (unless source (setq source (gethash ,form source-note-map)))
                    (setq changed t))))
      (prog (sym transforms lexdefs changed enabled macro-function compiler-macro (source t))
         (when source-note-map
           (setq source (gethash form source-note-map)))
         (go START)
         LOOP
         (form-changed form)
         (when (and (consp form) 
                    (or (eq (%car form) 'the)
                        (and sym (eq (%car form) sym))))
           (go DONE))
         START
         (when (non-nil-symbol-p form)
           (multiple-value-bind (newform win) (nx-transform-symbol form environment)
             (unless win (go DONE))
             (setq form newform)
             (go LOOP)))
         (when (atom form)
           ;; ---->  here <----
           (when (altform::special-object-p form)
             (setq form (altform::transform form nil)))
           (go DONE))
         (unless (symbolp (setq sym (%car form)))
           (go DONE))
         #+no
         (when (eq sym 'the)
           (destructuring-bind (typespec thing) (cdr form)
             (if (constantp thing)
                 (progn
                   (setq form thing)
                   (go LOOP))
                 (multiple-value-bind (newform win) (nx-transform thing environment source-note-map)
                   (when win
                     (form-changed newform)
                     (if (and (self-evaluating-p newform)
                              (typep newform typespec))
                         (setq form newform)
                         (setq form `(the ,typespec ,newform)))
                     (go DONE))))))
         (when (nx-quoted-form-p form)
           (when (self-evaluating-p (%cadr form))
             (setq form (%cadr form)))
           (go DONE))
         (when (setq lexdefs (nx-lexical-finfo sym environment))
           (if (eq 'function (%car lexdefs))
               (go DONE)))
         (setq transforms (setq compiler-macro (compiler-macro-function sym environment))
               macro-function (macro-function sym environment)
               enabled (nx-allow-transforms environment))
         (unless macro-function
           (let* ((win nil))
             (when (and enabled (functionp (fboundp sym)))
               (multiple-value-setq (form win) (nx-transform-arglist form environment source-note-map))
               (when win
                 (form-changed form)))))
         (when (and enabled
                    (not (nx-declared-notinline-p sym environment)))
           (multiple-value-bind (value folded) (nx-constant-fold form environment)
             (when folded
               (setq form value)
               (form-changed form)
               (unless (and (consp form) (eq (car form) sym)) (go START))))
           (when compiler-macro
             (multiple-value-bind (newform win) (compiler-macroexpand-1 form environment)
               (when win
                 (when (and (consp newform) (eq (car newform) sym) (functionp (fboundp sym)))
                   (setq sym nil))
                 (setq form newform)
                 (go LOOP))))
           (multiple-value-bind (newform win) (maybe-optimize-slot-accessor-form form environment)
             (when win
               (setq sym nil)
               (setq form newform)
               (go START)))
           (unless macro-function
             (when (setq transforms (structref-info sym environment))
               (setq form (defstruct-ref-transform transforms (%cdr form) environment))
               (form-changed form)
               (go START))
             (when (setq transforms (assq sym *nx-synonyms*))
               (setq form (cons (%cdr transforms) (setq sym (%cdr form))))
               (go LOOP))))
         (when (and macro-function
                    (or lexdefs
                        (not (and (gethash sym *nx1-alphatizers*) (not (nx-declared-notinline-p sym environment))))))
           (nx-record-xref-info :macro-calls (function-name macro-function))
           (setq form (macroexpand-1 form environment))
           (form-changed form)
           (go START))
         DONE
         (if (eq source t)
             (setq source nil)
             (let ((this (nx-source-note form)))
               (if this
                   (setq source this)
                   (when source
                     (unless (and (consp form)
                                  (eq (%car form) 'the)
                                  (eq source (gethash (caddr form) source-note-map)))
                       (when (or (consp form) (vectorp form) (pathnamep form))
                         (unless (or (eq form (%unbound-marker))
                                     (eq form (%slot-unbound-marker)))
                           (setf (gethash form source-note-map) source))))))))
         ;; Return source for symbols, even though don't record it in hash table.
         (return (values form changed source)))))
  
  )







