#|
	Define a version of tokens (page 67) that takes :test and :start 
	arguments defaulting to #'constituent and 0 respectively.
|#

;;; Original function.
(defun tokens (str test start)
	(let ((p1 (position-if test str :start start)))
		(if p1
			(let ((p2 (position-if
							#'(lambda (c) (not (funcall test c)))
                            str
							:start p1)))
				(cons (subseq str p1 p2)
					(if p2
						(tokens str test p2)
						nil)))
			nil)))

(defun constituent (c)
	(and (graphic-char-p c)
		(not (char= c #\ ))))


;;; New version of 'tokens'.
(defun new-tokens (str &key (test #'constituent) (start 0))
	(let ((p1 (position-if test str :start start)))
		(if p1
			(let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
				(cons (subseq str p1 p2)
					(if p2
						(new-tokens str :test test :start p2)
						nil)))
			nil)))

;; Test calls.
(tokens "ab12 3cde.f" #'constituent 0)
(new-tokens "ab12 3cde.f")
