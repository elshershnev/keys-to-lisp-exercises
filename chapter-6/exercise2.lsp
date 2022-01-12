#|
	Define a version of bin-search (page 60) that takes :key, :test, 
	:start, and :end arguments with the usual meanings and defaults. 
|#

(defun bin-search (obj vec &key (key #'identity) (test #'eql) (start 0) (end (- (length vec) 1)))
	(let ((len (length vec)))
		(and (not (zerop len))
			(finder obj vec start end key test))))

(defun finder (obj vec start end key test)
	(let ((range (- end start)))
		(if (<= range 0) ; zerop is changed to <= to avoid problem with searching of something less than the first element
			(if (funcall test obj (funcall key (aref vec start)))
				obj
				nil)
			(let ((mid (+ start (round (/ range 2)))))
				(let ((obj2 (funcall key (aref vec mid))))
					(if (< obj obj2)
						(finder obj vec start (- mid 1) key test)
						(if (> obj obj2)
							(finder obj vec (+ mid 1) end key test)
							obj)))))))

;; Test calls.
(bin-search 3 #(1 2 3 4 5))
(bin-search 3 #((1 2) (3 4) (5 6)) :test #'eql :key #'car :start 0 :end 1)
