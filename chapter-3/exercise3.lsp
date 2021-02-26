#|
	Define a function that takes a list and returns a list indicating the 
	number of times each (eql) element appears, sorted from most common 
	element to least common:
	
	> (occurrences '(a b a d a c d e a)) 
	((A . 4) (C . 2) (D . 2) (B . 1))
|#


(defun occurrences (lst)
    (let ((keys Nil))
        (dolist (el lst)
            (setf pair (assoc el keys))
            (let ((count (if (null pair) 1 (+ (cdr pair) 1))))
                (setf keys (cons (cons el count) (delete el keys :key #' car)))))
        keys))


;; test call

(occurrences '(a b a d a c d c a))
