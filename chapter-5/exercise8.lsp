#|
	Define a single recursive function that returns, as two values, the maximum
	and minimum elements of a vector.
|#

(defun min-max (seq)
    (let ((head (elt seq 0)))
         (cond
             ((= (length seq) 0) (values nil nil))
             ((= (length seq) 1) (values head head))
             (t (multiple-value-bind (min max) (min-max (subseq seq 1))
                 (cond
                     ((< head min) (values head max))
                     ((> head max) (values min head))
                     (t (values min max))))))))

;; Test calls.
;; Test call for vector
(multiple-value-bind (a b) (min-max (vector -4 1 6 2 9 6 8))
    (print a)
    (print b))

;; Test call for list. The min-max function works for both vectors and lists because
;; of we use functions working with sequences
(multiple-value-bind (a b) (min-max '(-4 1 6 2 9 6 8))
    (print a)
    (print b))
