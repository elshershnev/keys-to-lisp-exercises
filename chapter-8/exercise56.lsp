#|
	5. Write a program that can verify whether or not a quote was produced
	   by Henley (Section 8.8). 
	6. Write a version of Henley that can take a word and generate a sentence 
	   with that word in the middle of it. 
|#

;; Original Henley.

(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun punc (c)
  (case c
    (#\. '|.|)
	(#\, '|,|)
	(#\; '|;|) 
    (#\! '|!|)
	(#\? '|?|)))

(let ((prev '|.|))
	(defun see (symb)
		(let ((pair (assoc symb (gethash prev *words*))))
			(if (null pair)
				(push (cons symb 1) (gethash prev *words*))
				(incf (cdr pair))))
		(setf prev symb)))

(defun read-text (pathname)
	(with-open-file (s pathname :direction :input)
		(let ((buffer (make-string maxword))
			  (pos 0))
		    (do ((c (read-char s nil :eof) 
				  (read-char s nil :eof)))
				((eql c :eof))
				(if (or (alpha-char-p c) (char= c #\'))
					(progn
						(setf (aref buffer pos) c)
						(incf pos))
					(progn
						(unless (zerop pos)
							(see (intern (string-downcase 
									   (subseq buffer 0 pos))))
						(setf pos 0))
						(let ((p (punc c)))
							(if p (see p)))))))))

(defun random-next (prev)
	(let* ((choices (gethash prev *words*))
           (i (random (reduce #'+ choices 
                            :key #'cdr))))
		(dolist (pair choices)
			(if (minusp (decf i (cdr pair)))
				(return (car pair))))))

(defun generate-text (n &optional (prev '|.|))
	(if (zerop n)
		(terpri)
		(let ((next (random-next prev)))
			(format t "~A " next)
			(generate-text (1- n) next))))

;; End of original Henley.

;;; Helper for implementing exercise 5.
(defun make-words-list (str)
	(labels ((append-word (lst sym) (append lst (list sym)))
			 (make-sym (buf pos) (intern (string-downcase (subseq buf 0 pos)))))
		(let ((words '())
			  (buffer (make-string maxword))
			  (pos 0)
			  (len (length str)))
			(do* ((i 0 (+ i 1))
			      (c (elt str i) (and (< i len) (elt str i))))
				((not c) (unless (zerop pos) (setf words (append-word words (make-sym buffer pos)))) words)
				(if (or (alpha-char-p c) (char= c #\'))
					(progn
						(setf (aref buffer pos) c)
						(incf pos))
					(progn
						(unless (zerop pos)
							(setf words (append-word words (make-sym buffer pos))))
						(setf pos 0)
						(let ((p (punc c)))
							(if p (setf words (append-word words p))))))))))

;;; Key to exercise 5
(defun made-by-henley-p (quo)
	(let* ((words (make-words-list quo))
		   (prev (car words)))
		(if (not (cadr words))
			(multiple-value-bind (_ a) (gethash prev *words*) a)
			(dolist (word (cdr words) t)
				(if (assoc word (gethash prev *words*))
					(setf prev word)
					(return nil))))))

;; Helpers for implementing exercise 6

(defun get-ht-keys (ht)
	(let ((keys nil))
		(maphash #'(lambda (k v) (push k keys)) ht)
		keys))

(defun filter (func lst)
	(let ((acc nil))
		(dolist (item lst)
			(let ((val (funcall func item)))
				(when val (push item acc))))
		acc))

(let ((hash-table nil)
	  (all-ht-keys nil))
	(defun get-random-previous-word (word-as-sym ht)
		(unless hash-table (setf hash-table ht))
		(unless all-ht-keys (setf all-ht-keys (get-ht-keys ht)))
		(let ((words (filter #'(lambda (k) (assoc word-as-sym (gethash k hash-table))) all-ht-keys)))
			(elt words (random (length words))))))

;;; Key to exercise 6
(defun generate-text-with-word (n word ht)
	(let ((word-as-sym (intern (string-downcase word)))
		  (half (floor (/ n 2))))
		(if (or (not word) (not (gethash word-as-sym ht)))
			(terpri)
			(progn
				(dotimes (i half)
					(format t "~A " (setf word-as-sym (get-random-previous-word word-as-sym ht))))
				(format t "~A " word)
				(generate-text (if (evenp n) (1- half) half) word-as-sym)))))

;; Test calls.
(read-text "text.txt")

(made-by-henley-p "foo")
(made-by-henley-p "this now high ! there stood , inevitably thou my might determine , where adam first warmly") ; real example of what Henley made
(made-by-henley-p "No ground of enmitie between us known, Why hee should mean me ill, or seek to harme.") ; quote of original text

(generate-text-with-word 10 "empyreal" *words*)
(generate-text-with-word 13 "hell" *words*)
