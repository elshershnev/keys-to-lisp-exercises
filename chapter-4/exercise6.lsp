#|
	The contents of any hash table can be described by an assoc-list whose 
	elements are (k . v), for each key-value pair in the hash table. Define 
	a function that 
	(a) takes an assoc-list and returns a corresponding hash table 
	(b) takes a hash table and returns a corresponding assoc-list 
|#

;;; subtask a
(defun make-hash-tbl (assoc-lst)
    (setf ht (make-hash-table))
    (dolist (el assoc-lst)
        (let ((key (car el))
              (val (cdr el)))
             (setf (gethash key ht) val)))
    ht)

;;; subtask b
(defun make-assoc-list (hash-table)
    (setf alist Nil)
    (maphash #'(lambda (k v) (setf alist (append alist (list (list k v)))))
             hash-table)
    alist)

;; demo code
(setf assoc-lst '((+ . "add") (- . "sub") (* . "mul"))
	  ht (make-hash-tbl  assoc-lst))

(gethash '+ ht)
(gethash '* ht)

(make-assoc-list ht)
