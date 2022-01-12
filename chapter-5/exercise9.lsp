#|
	The program in Figure 3.12 continues to search as the first complete 
	path works its way through the queue. In broad searches this would be 
	a problem. 
	(a) Using catch and throw, modify the program to return the first 
		complete path as soon as it is discovered. 
	(b) Rewrite the program to do the same thing without using catch 
		and throw.
|#

;; Define graph for both versions. Each element of the list represents a parent node (first element) and
;; its child nodes. For example (a b c) means that we have a node named 'a' with two child nodes 'b' and 'c'.
#|
	a --- b
	 \    \
	  ---- c --- d
|#
(setf tree '((a b c) (b c) (c d)))

;;; catch throw version (a)
(defun new-paths-ct (path node net end)
    (mapcar
		#'(lambda (n)
			(let ((new-path (cons n path)))
				(if (eql (car new-path) end)
					(throw 'solved (reverse new-path))
					new-path)))
		(cdr (assoc node net))))

(defun bfs-ct (end queue net)
	(if (null queue)
		nil
		(let ((path (car queue)))
			(catch 'solved
				(let ((node (car path)))
					(if (eql node end)
						(reverse path)
						(bfs-ct
							end
							(append (cdr queue)
                                (new-paths-ct path node net end))
							net)))))))

(defun shortest-path-ct (start end net)
    (bfs-ct end (list (list start)) net))


;;; regular version without catch throw (b)
(defun new-paths-reg (path node net end)
    (mapcar #'(lambda (n)
        (let ((new-path (cons n path)))
            (if (eql (car new-path) end)
                (return-from new-paths-reg (list new-path))
                new-path)))
        (cdr (assoc node net))))

(defun bfs-reg (end queue net)
    (if (null queue)
        nil
        (let ((path (car queue))
              (solve (member end queue :key 'car)))
            (if solve
                (reverse (car solve))
                (let ((node (car path)))
                    (if (eql node end)
                        (reverse path)
                        (bfs-reg
							end
                            (append
								(cdr queue)
                                (new-paths-reg path node net end))
							net)))))))

(defun shortest-path-reg (start end net)
    (bfs-reg end (list (list start)) net))


;; Test calls.
(shortest-path-ct 'a 'd tree)
(shortest-path-reg 'a 'd tree)
