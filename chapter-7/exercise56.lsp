#|
	5. Modify stream-subst to allow wildcards in the pattern. If the char-
	acter + occurs in old, it should match any input character. 

	6. Modify stream-subst so that the pattern can include an element that 
	matches any digit character, an element that matches any alphanumeric 
	character, or an element that matches any character. The pattern must 
	also be able to match any specific input character. (Hint: old can no 
	longer be a string.) 
|#

(defstruct buf
	vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
	(svref (buf-vec buf)
		   (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
	(setf (svref (buf-vec buf)
				 (mod n (length (buf-vec buf))))
		  val))

(defun new-buf (len)
	(make-buf :vec (make-array len)))

(defun buf-insert (x b)
	(setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
	(prog1 
		(bref b (incf (buf-start b)))
		(setf (buf-used b) (buf-start b)
			  (buf-new  b) (buf-end   b))))

(defun buf-next (b)
	(when (< (buf-used b) (buf-new b))
		(bref b (incf (buf-used b)))))

(defun buf-reset (b)
	(setf (buf-used b) (buf-start b)
		  (buf-new  b) (buf-end   b)))

(defun buf-clear (b)
	(setf (buf-start b) -1 (buf-used  b) -1
		  (buf-new   b) -1 (buf-end   b) -1))

(defun buf-flush (b str)
	(do ((i (1+ (buf-used b)) (1+ i)))
		((> i (buf-end b)))
		(princ (bref b i) str)))

;;; version with patterns inside function body (not so good but acceptable)
(defun match1 (sym pattern pos)
	(if (stringp pattern)
		;; key to exercise 5
		(let ((ch (char pattern pos)))
			(cond
				((equal ch #\+) t)
				((char= sym ch) t)))
		;; key to exercise 6
		(let 
			((str (elt pattern pos)))
			(cond
				((and
					(or (equal str "\d") (equal str "[0-9]")) ; an element that matches any digit character
					(digit-char-p sym)) t)
				((and
					(or (equal str "\w") (equal str "[0-9a-z]")) ; an element that matches any alphanumeric character
					(or (digit-char-p sym) (alpha-char-p sym)) t))
				((or (equal str ".") (equal str #\.)) t) ; an element that matches any character
				(t (equal str sym))))))

;;; version with patterns and executors in the hash table in closure (a little better
;;; because of easier extensible)
(let
	((ht (make-hash-table :test 'equal))  ; hash table for exercise 6
	 (pairs '(
		("\d"       (lambda (sym) (digit-char-p sym)))
		("[0-9]"    (lambda (sym) (digit-char-p sym)))
		("\w"       (lambda (sym) (or (digit-char-p sym) (alpha-char-p sym))))
		("[0-9a-z]" (lambda (sym) (or (digit-char-p sym) (alpha-char-p sym))))
		("."        (lambda (sym) t))
		(#\.        (lambda (sym) t))
									 )))
		(dolist (item pairs)
			(let ((pattern (car item))
				  (func (eval (cadr item))))
				(setf (gethash pattern ht) func)))

(defun match2 (sym pattern pos)
	(if (stringp pattern)
		;; key to exercise 5 equals to key from match1
		(let ((ch (char pattern pos)))
			(cond
				((equal ch #\+) t)
				((char= sym ch) t)))
		;; key to exercise 6
		(let 
			((str (elt pattern pos)))
			(multiple-value-bind (predicate exists) (gethash str ht)
				(if exists
					(funcall predicate sym)
					(equal str sym)))))))

(defun stream-subst (old new in out)
	(let* ((pos 0)
           (len (length old))
           (buf (new-buf len))
           (from-buf nil))
		(do ((c (read-char in nil :eof)
				(or (setf from-buf (buf-next buf))
					(read-char in nil :eof))))
			((eql c :eof))
			(cond
				((match1 c old pos) ; change match1 to match2 to check that match2 is working too
					(incf pos)
					(cond
						;; The variable pos points to the position of the character we are trying to match in the sought-for string.
						;; When and if pos is equal to the length of this string, we have a complete match, and we write the
						;; replacement string to the output stream, also clearing the buffer
						((= pos len)
							(princ new out)
							(setf pos 0)
							(buf-clear buf))
						; When a match begins, the characters involved are queued in the buffer buf
						((not from-buf)
							(buf-insert c buf))))
				;; Until the input character matches the first element of the sought-for string,
				;; it is written immediately to the output stream
				((zerop pos)           
					(princ c out)
					(when from-buf
						(buf-pop buf)
						(buf-reset buf)))
				;; If the match fails before this point, we can pop the first character in the buffer and write it to the output
				;; stream, after which we reset the buffer and start over with pos equal to zero
				(t
					(unless from-buf
						(buf-insert c buf))
					(princ (buf-pop buf) out)
					(buf-reset buf)
					(setf pos 0))))
		(buf-flush buf out)))

(defun file-subst (old new file1 file2)
	(with-open-file (in file1 :direction :input)
		(with-open-file (out file2 :direction :output :if-exists :supersede)
			(stream-subst old new in out))))

;; Test calls.

;; make sure that test1 have something for substitution
;; example with a string pattern
(file-subst " +h" " Z" "test.txt" "out.txt")
;; example with a list pattern
(file-subst '(#\a "\d" #\s) "QWE" "test.txt" "out.txt")
