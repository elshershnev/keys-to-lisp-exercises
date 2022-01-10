#|
	3. Define a structure to represent a tree where each node contains some 
	data and has up to three children. Define 
		(a) a function to copy such a tree (so that no node in the copy is eql 
		to a node in the original) 
		(b) a function that takes an object and such a tree, and returns true if 
		the object is eql to the data field of one of the nodes 

	4. Define a function that takes a BST and returns a list of its elements 
	ordered from greatest to least. 

	5. Define bst-adjoin. This function should take the same arguments as 
	bst-insert, but should only insert the object if there is nothing eql 
	to it in the tree. 
|#


(defstruct (node (:print-function
                   (lambda (n s d)
                     (format s "#<~A>" (node-elt n)))))
  elt (l nil) (m nil) (r nil))

(defun ttree-insert (obj tree)
  (if (null tree)
      (make-node :elt obj)
      (let ((elt (node-elt tree)))
        (if (eql obj elt)
            (make-node
                  :elt elt
                  :l   (node-l tree)
                  :m   (ttree-insert obj (node-m tree))
                  :r   (node-r tree))

            (if (< obj elt)
                (make-node
                  :elt elt
                  :l   (ttree-insert obj (node-l tree))
                  :m   (node-m tree)
                  :r   (node-r tree))
                (make-node
                  :elt elt
                  :l   (node-l tree)
                  :m   (node-m tree)
                  :r   (ttree-insert obj (node-r tree))))))))

;;; exercise 3a
(defun ttree-copy (tree)
    (if (null tree)
        nil
        (let ((elt (node-elt tree))
              (l (node-l tree))
              (m (node-m tree))
              (r (node-r tree)))
         (make-node
           :elt elt
           :l (copy-tree l)
           :m (copy-tree m)
           :r (copy-tree r)))))

;;; exercise 3b
(defun ttree-find (obj tree)
  (if (null tree)
      nil
      (let ((elt (node-elt tree)))
        (if (eql obj elt)
            T
            (if (< obj elt)
                (ttree-find obj (node-l tree))
                (ttree-find obj (node-r tree)))))))

;;; exercise 4
(defun ttree-traverse (tree)
    (when tree
        (ttree-traverse (node-r tree))
        (princ (node-elt tree))
        (ttree-traverse (node-m tree))
        (ttree-traverse (node-l tree))))

;;; exercise 5
(defun ttree-adjoin (obj tree)
    (if (null tree)
        (make-node :elt obj)
	    (if (ttree-find obj tree)
	        tree
		    (ttree-insert obj tree))))

;; demo code
(setf ttree nil)
(dolist (x '(5 8 4 5 2 1 9 8 6 7 3 3 4))
    (setf ttree (ttree-insert x ttree)))
(format t "~%Original tree: ")
(print (ttree-traverse ttree))

(setf copied-ttree (ttree-copy ttree))
(format t "~%Copied tree: ")
(print (ttree-traverse copied-ttree))

(setf ttree-no-duplicates nil)
(dolist (x '(5 8 4 5 2 1 9 8 6 7 3 3 4))
    (setf ttree-no-duplicates (ttree-adjoin x ttree-no-duplicates)))
(format t "~%Tree with no duplicates: ")
(print (ttree-traverse ttree-no-duplicates))
