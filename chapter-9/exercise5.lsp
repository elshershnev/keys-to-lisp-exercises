#|
    5. Suppose f is a function of one (real) argument, and that min and max
    are nonzero reals with different signs such that f has a root (returns
    zero) for one argument i such that min < i < max. Define a function
    that takes four arguments, f, min, max, and epsilon, and returns an
    approximation of i accurate to within plus or minus epsilon.
|#

;;; Function that returns 0 for x = -1.
(defun test-func (x)
	(+ (* 2 x) 2))

(defun find-approximate-solution (func minimum maximum epsilon)
	(let*
        ((left minimum)
         (right maximum)
         (middle (/ (+ left right) 2)))
	    (do ((fl (funcall func left) (funcall func left))
	         (fr (funcall func right) (funcall func right))
	         (fm (funcall func middle) (funcall func middle)))
            ((< (abs (- fl fr)) epsilon) (float left))
            (if (> fm 0)
                (setf right middle)
                (setf left middle))
            (setf middle (/ (+ left right) 2)))))
	


;; Test call.
(find-approximate-solution #'test-func -5 7 0.01)
