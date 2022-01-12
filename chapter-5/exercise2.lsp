#|
	Rewrite mystery (page 29) to use cond
|#

;;; Original mystery.
(defun mystery (x y)
	(if (null y)
		nil
		(if (eql (car y) x)
			0
			(let ((z (mystery x (cdr y))))
				(and z (+ z 1))))))

;;; Modified mystery.
(defun new-mystery (x y)
    (cond
        ((null y) nil)
        ((eql (car y) x) 0)
        (t (let ((z (new-mystery x (cdr y))))
                (and z (+ z 1))))))

;; Test calls. Both functions return position of first argument in list (second argument) starting from 0.
(mystery 6 '(3 2 6 8 0 5 1))
(new-mystery 6 '(3 2 6 8 0 5 1))