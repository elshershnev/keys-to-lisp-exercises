#|
	Define a function that takes a filename and returns a list of the expres-
	sions in the file.
|#

(defun read-expressions (filename &key dirlist)
	(let*
		((make-path-args (if dirlist (append '(:directory) (list (cons ':absolute dirlist)) (list ':name filename)) (list ':name filename)))
		 (path (apply 'make-pathname make-path-args))
		 (expressions nil))
		 (with-open-file (stream path :direction :input)
			(do 
				((expression (read stream nil 'eof) (read stream nil 'eof)))
				((equal expression 'eof) expressions)
				(if (listp expression) (setf expressions (append expressions (list expression))))))))
				
;; Test calls.
;; WARNING: we have to create text file named test.txt in the specific path before running this.
;; Please pay attention that test.txt contains some examples of lisp code
(read-expressions "test.txt" :dirlist '("users" "user" "desktop")) ; when test.txt is located in \users\user\desktop
(read-expressions "test.txt") ; when test.txt in the same place where Lisp
