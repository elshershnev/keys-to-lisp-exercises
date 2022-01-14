#|
	Suppose that in some format for text files, comments are indicated by 
	a % character. Everything from this character to the end of the line is 
	ignored. Define a function that takes two filenames, and writes to the 
	second file a copy of the first, minus comments. 

|#

(defun remove-comments (in out &key dirlist)
	(let*
		((make-path-common-args (if dirlist (append '(:directory) (list (cons ':absolute dirlist))) nil))
		 (input-path (apply 'make-pathname (append make-path-common-args (list ':name in))))
		 (output-path (apply 'make-pathname (append make-path-common-args (list ':name out)))))
		(let
			((input-stream (open input-path :direction :input))
			 (output-stream (open output-path :direction :output :if-exists :supersede)))
				(do 
					((line (read-line input-stream nil 'eof) (read-line input-stream nil 'eof)))
					((equal line 'eof) (progn (close input-stream) (close output-stream)))
					(let ((index (position #\% line)))
						(when (or (not index) (> index 0))
							(format output-stream "~A~%"
								(if index
									(subseq line 0 index)
									line))))))))
								
				
;; Test calls.
;; WARNING: we have to create text file named test.txt in the specific path before running this.
(remove-comments "test.txt" "out.txt" :dirlist '("users" "user" "desktop")) ; when test.txt is located in \users\user\desktop
(remove-comments "test.txt" "out.txt") ; when test.txt in the same place where Lisp
