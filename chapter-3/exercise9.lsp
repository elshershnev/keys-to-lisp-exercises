#|
	Write a program to find the longest finite path through a network 
	represented as in Section 3.15. The network may contain cycles
|#


(defun longest-path (start end net)
	(bfs end (list (list start)) net))


(defun bfs (end queue net)
	(if (null queue)
		nil
		(let ((path (car queue)))
			(let ((node (car path)))
				(if (eql node end)
					(reverse path)
					(bfs
						end
						(append (new-paths path node net) (cdr queue))
						net))))))


(defun new-paths (path node net)
  (mapcar #'(lambda (n)
               (cons n path))
          (cdr (assoc node net))))


(setf net '((a b c) (b c) (c d)))

;; test call

(longest-path 'a 'd net)
