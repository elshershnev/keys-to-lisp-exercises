#|
	Modify most (page 105) to return, as two values, the two highest-
	scoring elements of a list.
|#

;;; Original most. Returns the element of a list with the highest score, according to
;;; some scoring function. It returns two values, the winning element, and its score
(defun most (fn lst)
    (if (null lst)
        (values nil nil)
        (let* ((wins (car lst))
               (max (funcall fn wins)))
			(dolist (obj (cdr lst))
				(let ((score (funcall fn obj)))
					(when (> score max)
						(setf wins obj
							  max  score))))
			(values wins max))))

;;; New version of most. Returns 2 elements with the highest score according to some
;;; score function
(defun new-most (fn lst)
    (if (null lst)
		(values nil nil)
		(let* ((max-el1 (car lst))
			   (max-el2 nil)
               (max-score (funcall fn max-el1)))
			(dolist (obj (cdr lst))
				(let ((score (funcall fn obj)))
					(when (>= score max-score)
						(setf max-el2 max-el1
							  max-el1 obj
							  max-score score))))
			(values max-el1 max-el2))))



;; Test calls.
(multiple-value-bind (value score) 
    (most 'length '((1 2 3) (1 2) (1 2 3 4 5)))
    (print value)
    (print score))

(multiple-value-bind (max1 max2) 
    (new-most 'length '((1 2 3) (1 2) (1 2 3 4 5)))
    (print max1)
    (print max2))
