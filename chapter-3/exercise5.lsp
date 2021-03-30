#|
	Suppose the function pos+ takes a list and returns a list of each element 
	plus its position: 
	> (pos+ '(7514)) 
	(7 6 3 7) 
	Define this function using (a) recursion, (b) iteration, (c) mapcar.
|#


;;; version (a) recursive, consists from helper and auxiliary function that reduce
;;; number of arguments
(defun pos+v1-helper (lst number)
    (if (null lst)
        Nil
        (append
			(list (+ (car lst) number))
			(pos+v1-helper (cdr lst) (+ number 1)))))


(defun pos+v1 (lst)
	(pos+v1-helper lst 0))


;;; version (b) iterative
(defun pos+v2 (lst)
    (let ((result Nil))
        (dotimes (i (length lst))
            (setf result (append result (list (+ (nth i lst) i)))))
        result))


;;; version (c) using mapcar
(defun pos+v3 (lst)
    (setf
         start -1
         nths (mapcar (lambda (x) (setf start (+ start 1))) lst))
    (mapcar '+ lst nths))



;;; another version (c) using mapcar, consists of 2 functions
(defun numlist (lst num)
    (if (null lst)
        Nil
        (cons num (numlist (cdr lst) (+ num 1)))))


(defun pos+v4 (lst)
    (mapcar '+ lst (numlist lst 0)))    


;; test calls

(pos+v1 '(7 5 1 4))

(pos+v2 '(7 5 1 4))

(pos+v3 '(7 5 1 4))

(pos+v4 '(7 5 1 4))
