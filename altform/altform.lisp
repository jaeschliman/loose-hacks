;;;; altform.lisp

(in-package #:altform)
(named-readtables:in-readtable example)

(defun foo (&rest r)
  `[surrounding ,@r !])

(let ((table (make-hash-table)))
  (setf (gethash :x table) (list 'a 'b 'c))
  (multiple-value-bind (v f) (gethash :x table)
    (let ((a (first v))
          (b (second v))
          (c (third v)))
      (list c b a f))))

(dbind [([a b c] f) (value-for-key {:x '(x y z) } :x) ]
       [c b a f])

;;hooray, it works.
#+ (or) (dbind [([a b c] f) (value-for-key {:x '[x y z] } :x) ]
               [c b a f])

#+(or) (dbind [([a b c] f) (value-for-key {:x `[x y z] } :x) ]
               [c b a f])
