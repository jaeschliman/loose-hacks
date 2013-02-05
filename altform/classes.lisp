(in-package :altform)
(named-readtables:in-readtable example)

(simple-syntax-class syntax-vector #\[ #\])
(simple-syntax-class syntax-map    #\{ #\})
(simple-syntax-class syntax-set    #\{ #\} t)

(defun val (o) (slot-value o 'val))

(extend-type syntax-vector
  collection
  (empty (o) (make-syntax-vector))
  (empty-p (o) (empty-p (val o)))

  seqable
  (seq (o) o)

  seq
  (head (o) (car (val o)))
  (tail (o) (when (cdr (val o))
              (apply 'make-syntax-vector (cdr (val o)))))

  countable
  (counted-p (o) t)
  (count-elements (o) (count-elements (val o)))

  indexable
  (element-at (o index) (element-at (val o) index)))


(defmethod conj ((a syntax-vector) val)
  (apply 'make-syntax-vector (append (val a) (list val))))

(defun pair-up (list)
  (loop for (a b) on list by #'cddr
       collect (list a b)))

(extend-type syntax-map
  collection
  (empty (o) (make-syntax-map))
  (empty-p (o) (empty-p (val o)))

  countable
  (counted-p (o) t)
  (count-elements (o) (/ (count-elements (val o)) 2))

  associative
  (all-keys (o)
            (loop for a in (val o) by #'cddr
                 collect a))
  (all-values (o)
              (loop for a in (cdr (val o)) by #'cddr
                   collect a))
  (contains-key-p (o k)
                  (member k (all-keys o)))
  (value-for-key (o k)
                 (if (contains-key-p o k)
                     (values (getf (val o) k) t)
                     (values nil nil))))

(defmethod conj ((a syntax-map) kv)
  (let ((val (copy-list (val a))))
    (setf (getf val (head kv)) (head (tail kv)))
    (apply 'make-syntax-map val)))



(extend-type syntax-set
  collection
  (empty (o) (make-syntax-set))
  (empty-p (o) (empty-p (val o)))

  seqable
  (seq (o) (copy-list (val o))) 

  countable
  (counted-p (o) t)
  (count-elements (o) (count-elements (val o))))

(defmethod conj ((a syntax-set) v)
  (let ((val (val a)))
    (loop for o in val
         when (equalp o v)
         do (return a)
         finally
         (return (apply 'make-syntax-set (cons v val))))))
