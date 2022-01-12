#|
	Define remove-if (no keywords) in terms of filter (page 105)
|#

;;; Function 'fiter' from page 105
(defun filter (fn lst)
	(let ((acc nil))
		(dolist (x lst)
			(let ((val (funcall fn x)))
				(if val (push val acc))))
		(nreverse acc)))

(defun new-remove-if (fn lst)
    (filter (lambda (x) (unless (funcall fn x) x)) lst))


;; Test calls.
(remove-if #'numberp '(1 a d g 2 3 4))
(new-remove-if #'numberp '(1 a d g 2 3 4))
