#|
	Define a function that takes a filename and returns a list of strings 
	representing each line in the file. 
|#

(defun read-lines (filename &key dirlist)
	(let*
		((make-path-args (if dirlist (append '(:directory) (list (cons ':absolute dirlist)) (list ':name filename)) (list ':name filename)))
		 (path (apply 'make-pathname make-path-args))
		 (lines nil))
		 (with-open-file (stream path :direction :input)
			(do 
				((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
				((equal line 'eof) lines)
				;; For reasons unknown if we don't remove \n symbol from the end of
				;; the read line explicitly, function result looks strange in top level
				;; (in fact they are looking good). Uncomment the line below to see
				;; fine clear result in toplevel either.
				; (setf line (subseq line 0 (- (length line) 1)))
				(setf lines (append lines (list line)))))))

;; Test calls.
;; WARNING: we have to create text file named test.txt in the specific path before running this.
(read-lines "test.txt" :dirlist '("users" "user" "desktop")) ; when test.txt is located in \users\user\desktop
(read-lines "test.txt") ; when test.txt in the same place where Lisp
