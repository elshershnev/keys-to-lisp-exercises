#|
    1. Define a function that takes a list of reals and returns true if they are 
    in nondecreasing order.
|#

(defun are-sorted0 (lst)
    (when (car lst) (apply #'<= lst)))

(defun are-sorted1 (lst)
    (if (or (null lst) (eq (length lst) 1))
        t
        (let ((min (car lst)))
            (dolist (i (cdr lst))
            (if (< i min)
                (return-from are-sorted1 nil)
                (setf min i)))
            t)))

(defun are-sorted2 (lst)
    (cond
        ((null lst) t)
        ((null (cdr lst)) t)
        ((<= (car lst) (cadr lst)) (are-sorted2 (cdr lst)))
        (t nil)))

;; Test calls.
(are-sorted0 '(1 1 2.3 45/6 78))
(are-sorted1 '(1 1 2.3 45/6 78))
(are-sorted2 '(1 1 2.3 45/6 78))
(are-sorted0 '(1 1 2.3 45/6 78 9))
(are-sorted1 '(1 1 2.3 45/6 78 9))
(are-sorted2 '(1 1 2.3 45/6 78 9))
