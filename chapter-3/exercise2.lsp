#|
    Write a version of union that preserves the order of the elements in 
    the original lists:

	> (new-union '(a b c) '(b a d)) 
	(A B C D) 
|#


;;; util for solution 1
(defun construct-list (lst1 lst2 result)
    (if (and (null lst1) (null lst2))
        result
        (let ((car1 (car lst1))
              (car2 (car lst2)))
            (if (and car1 (not (member car1 result)))
                (setf result (cons car1 result)))
            (if (and car2 (not (member car2 result)))
                (setf result (cons car2 result)))
            (construct-list (cdr lst1) (cdr lst2) result))))


;;; Solution 1 (recursive)
(defun new-union1 (lst1 lst2)
    (reverse (construct-list lst1 lst2 Nil)))


;;; Solution 2 (iterarive)
(defun new-union2 (lst1 lst2)
    (let ((lst Nil))
        (dolist (item (append lst1 lst2))
            (if (not (member item lst)) (setf lst (cons item lst))))
        (reverse lst)))


;; test calls

(new-union1 '(a b c) '(b a d))
(new-union2 '(a b c) '(b a d))
