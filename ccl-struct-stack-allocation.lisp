#|

messing around with stack allocation of structs in ccl by poking around,
disassembling test functions.

CL-USER> (lisp-implementation-version)
"Version 1.9-r15759  (DarwinX8664)"


no guarantee that this will continue to work etc etc

what seems to work is using a macro to create a gvector
with correct type tag and class cell, compiler then uses
dynamic-extent declaration to allocate on the stack.

Mon May 26 11:39:41 PDT 2014

|#

(error "Do not load this file")

(defstruct test-struct a b c)

(defun test/a ()
  (let ((s (make-test-struct)))
    (test-struct-a s)))

(defun test/b ()
  (declare (optimize (speed 3)
                     (safety 0)))
  (let ((s (make-test-struct)))
    (test-struct-a s)))

(defun test/c ()
  (declare (optimize (speed 3)
                     (safety 0)))
  (let ((s (make-test-struct)))
    (declare (type test-struct s))
    (test-struct-a s)))

(defun test/d ()
  (declare (optimize (speed 3)
                     (safety 0)))
  (let ((s (make-test-struct)))
    (declare (type test-struct s)
             (dynamic-extent s))
    (test-struct-a s)))


;;allocates vector on the stack, and struct accessors work, but
;;that's not really good enough...
(defun test/e ()
  (declare (optimize (speed 3)
                     (safety 0)))
  (let ((s (vector nil nil nil nil)))
    (declare (type test-struct s)
             (dynamic-extent s))
    (test-struct-a s)))



(defun test/f ()
  (declare (optimize (speed 3)
                     (safety 0)))
  (let ((s (ccl::%gvector target::subtag-struct (load-time-value (ccl::%svref (make-test-struct) 0)) nil nil nil)))
    (declare (type test-struct s)
             (dynamic-extent s))
    (test-struct-a s)))


(declaim (inline %mk-test-struct))
(defun %mk-test-struct (a b c)
  (declare (optimize (speed 3)
                     (safety 0)))
  (ccl::%gvector target::subtag-struct (load-time-value (ccl::%svref (make-test-struct) 0)) a b c))

;; does not actually inline
(defun test/g ()
  (declare (optimize (speed 3)
                     (safety 0)))
  (let ((s (%mk-test-struct nil nil nil)))
    (declare (type test-struct s)
             (dynamic-extent s))
    (test-struct-a s)))


(defmacro mk-test-struct (a b c)
  `(ccl::%gvector target::subtag-struct (load-time-value (ccl::%svref (make-test-struct) 0)) ,a ,b ,c))


;; 'works'
(defun test/h ()
  (declare (optimize (speed 3)
                     (safety 0)))
  (let ((s (mk-test-struct nil nil nil)))
    (declare (type test-struct s)
             (dynamic-extent s))
    (test-struct-a s)))

(defmacro with-test-struct ((var &key a b c) &body body)
  `(locally
       (declare (optimize (speed 3)
                          (safety 0)))
     (let ((,var (mk-test-struct ,a ,b ,c)))
       (declare (type test-struct ,var)
                (dynamic-extent ,var))
       ,@body)))


(defun test-print ()
  (with-test-struct (s :a 'one :b 'two :c 'three)
    (format t " the struct is ~A~%" s)))


#|
;; "home:ex;cl;loose-hacks;ccl-struct-stack-allocation.lisp.newest":2699-2811
L0
         (leaq (@ (:^ L0) (% rip)) (% fn))       ;     [0]
         (testl (% nargs) (% nargs))             ;     [7]
         (jne L169)                              ;     [9]
         (pushq (% rbp))                         ;    [15]
         (movq (% rsp) (% rbp))                  ;    [16]

;;; (with-test-struct (s :a 'one :b 'two :c 'three) (format t " the struct is ~A~%" s))
         (pushq ($ #x1B0))                       ;    [19]
         (pushq (@ '(#<CLASS-CELL for TEST-STRUCT #x30200134D86D>) (% fn))) ;    [24]

;;; 'one 
         (pushq (@ 'ONE (% fn)))                 ;    [31]

;;; 'two 
         (pushq (@ 'TWO (% fn)))                 ;    [38]

;;; 'three
         (pushq (@ 'THREE (% fn)))               ;    [45]

;;; (with-test-struct (s :a 'one :b 'two :c 'three) (format t " the struct is ~A~%" s))
         (movl ($ 40) (% nargs))                 ;    [52]
         (leaq (@ (:^ L77) (% fn)) (% temp2))    ;    [57]
         (nop)                                   ;    [64]
         (nop)                                   ;    [67]
         (jmpq (@ .SPSTKGVECTOR))                ;    [70]
L77
         (leaq (@ (:^ L0) (% rip)) (% fn))       ;    [77]
         (pushq (% arg_z))                       ;    [84]

;;; (format t " the struct is ~A~%" s)
         (leaq (@ (:^ L133) (% fn)) (% temp0))   ;    [85]
         (pushq (% temp0))                       ;    [92]
         (movl ($ #x1302E) (% arg_x.l))          ;    [93]

;;; " the struct is ~A~%"
         (movq (@ '" the struct is ~A~%" (% fn)) (% arg_y)) ;    [99]

;;; (format t " the struct is ~A~%" s)
         (movl ($ 24) (% nargs))                 ;   [106]
         (movq (@ 'FORMAT (% fn)) (% temp0))     ;   [111]
         (pushq (@ #x12FB8))                     ;   [118]
         (jmpq (@ 10 (% temp0)))                 ;   [125]
L133
         (leaq (@ (:^ L0) (% rip)) (% fn))       ;   [133]
         (movq (@ 72 (% rcontext)) (% imm0))     ;   [140]
         (movq (@ (% imm0)) (% imm0))            ;   [144]
         (movq (% imm0) (@ 72 (% rcontext)))     ;   [147]
         (movq (% imm0) (@ #x158 (% rcontext)))  ;   [151]
         (jmpq (@ .SPNVALRET))                   ;   [158]

;;; #<no source text>
L169
         (uuo-error-wrong-number-of-args)        ;   [169]

|#
