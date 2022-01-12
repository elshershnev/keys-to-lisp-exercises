#|
	Define iterative and recursive versions of a function that takes an object 
	and a list, and returns a new list in which the object appears between 
	each pair of elements in the original list: 
	> (intersperse '- '(a b e d)) 
	(A - B - C - D) 
|#

(defun intersperse-rec (obj lst)
    (cond
        ((< (length lst) 2) lst)
        ((= (length lst) 2) (list (car lst) obj (cadr lst)))
        (t (append
            (list (car lst) obj)
            (intersperse-rec obj (cdr lst))))))


(defun cut-last (lst)
    (reverse (cdr (reverse lst))))

(defun intersperse-iter (obj lst)
    (let ((r nil))
         (dolist (x lst (cut-last r))
             (setf r (append r (list x) (list obj))))))
				 
;; Test calls.
(intersperse-rec '- '(a b e d))
(intersperse-iter '- '(a b e d))

