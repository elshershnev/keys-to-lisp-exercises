#|
    4. Add the code necessary to make the code in Figure 7.1 be in a package
    named "RING", and that in Figure 7.2 be in a package named "FILE".
    The existing code should remain unchanged.
|#

(defpackage "RING"
            (:use "COMMON-LISP")
            (:export "NEW-BUF" "BUF-CLEAR" "BUF-INSERT" "BUF-POP" "BUF-RESET" "BUF-NEXT" "BUF-FLUSH"))

(in-package "RING")

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

(defpackage "FILE"
            (:use "COMMON-LISP" "RING")
            (:export "FILE-SUBST"))

(in-package "FILE")

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
                ((char= c (char old pos))
                    (incf pos)
                    (cond
                        ((= pos len)
                            (princ new out)
                            (setf pos 0)
                            (buf-clear buf))
                        ((not from-buf)
                            (buf-insert c buf))))
                ((zerop pos)
                    (princ c out)
                    (when from-buf
                        (buf-pop buf)
                        (buf-reset buf)))
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
(in-package "COMMON-LISP-USER") ; return to default package
;; In the next comment there is example of test.txt for testing purposes.
#|
The struggle between Liberty and Authority is the most conspicuous feature 
in the portions of history with which we are earliest familiar, particularly 
in that of Greece, Rome, and England.
|#
(FILE:file-subst " th" " Z" "test.txt" "out.txt")
