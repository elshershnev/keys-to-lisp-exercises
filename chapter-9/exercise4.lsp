#|
    4. Define a function that takes 8 reals representing the endpoints of two
    segments in 2-space, and returns either nil if the segments do not
    intersect, or two values representing the x- and y-coordinates of the
    intersection if they do.
|#

(defun intersection-point (p1x p1y p2x p2y p3x p3y p4x p4y)
    ;; p1 and p2 are points of first segment
    ;; p3 and p4 are points of second segment
    ;; If the first point of segment is located to the right of the second one, we are swapping them
    (when (< p2x p1x) (rotatef p1x p2x) (rotatef p1y p2y))
    (when (< p4x p3x) (rotatef p3x p4x) (rotatef p3y p4y))
    (cond
        ;; If the first segment is located to the left of the second or if the second segment is
        ;; located to the right of the first then they are not intersected.
        ((or (< p2x p3x) (< p4x p1x)) nil)
        ((and (= p1x p2x) (= p3x p4x)) ; Both the segments are vertical
            ;; The segments lie on the same vertical straight-line. In this case we have
            ;; to check each segment's ends and find the one (point) which lies on the other segment.
            (if (= p1x p3x)
                (cond
                    ((and (<= p1y p3y) (>= p1y p4y)) (values p1x p1y))
                    ((and (<= p2y p3y) (>= p2y p4y)) (values p2x p2y))
                    ((and (<= p3y p1y) (>= p3y p2y)) (values p3x p3y))
                    ((and (<= p4y p1y) (>= p4y p2y)) (values p4x p4y))
                    (t nil))
                nil))
        ;; If we didn't returned value yet then we have to find intersection point of two straight-lines
        ;; passing through segments with some additional checks.
        ;; The equation of a straight-line in general form looks like y = kx + b
        ((= p1x p2x) ; Only the first segment is vertical.
            (let*
                (
                 (k2 (/ (- p3y p4y) (- p3x p4x))) ; Coefficient K of the second straight-line.
                 (b2 (- p3y (* k2 p3x))) ; Coefficien B of the second straingt-line.
                 (x p1x)
                 (y (+ (* k2 x) b2)))
                (if (and (< p3x x p4x) (or (< p1y y p2y) (< p2y y p1y)))
                    (values x y)
                    nil)))
        ((= p3x p4x) ; Only the second segment is vertical.
            (let*
                ((k1 (/ (- p1y p2y) (- p1x p2x)))
                 (b1 (- p1y (* k1 p1x)))
                 (x p3x)
                 (y (+ (* k1 x) b1)))
                (if (and (< p1x x p2x) (or (< p3y y p4y) (< p4y y p3y)))
                    (values x y)
                    nil)))
        (t
            (let*
                ((k1 (/ (- p1y p2y) (- p1x p2x))) ; Coefficient K of the first straight-line.
                 (k2 (/ (- p3y p4y) (- p3x p4x)))) ; Coefficient K of the second straight-line.
                (if (= k1 k2)
                    nil ; The segments are parallel. No intersection points in this case.
                    (let*
                        ((b1 (- p1y (* k1 p1x))) ; Coefficien B of the first straingt-line.
                         (b2 (- p3y (* k2 p3x))) ; Coefficien B of the second straingt-line.
                         (x (/ (- b1 b2) (- k2 k1)))
                         (y (+ (* k1 x) b1)))
                        (if (or (< x (max p1x p3x)) (> x (max p2x p4x)))
                            nil ; Straight-lines intersects outside of the segments projection
                            (values x y))))))))
    
;; Test calls.

(INTERSECTION-POINT 0.5 0.5 1 1.5 2 2 2.5 0.5) ; the first segment is located to the left of the second
(INTERSECTION-POINT 2 0.5 2.5 1.5 0.5 1 1 0.5) ; the second segment is located to the left of the first

;; Both the segments are vertical and have at minimum one common point
(INTERSECTION-POINT 1 2 1 0.5 1 1.5 1 1) ; second is inside the first
(INTERSECTION-POINT 1 2 1 0.5 1 2.5 1 1) ; second is a falf above the first
(INTERSECTION-POINT 1 2 1 0.5 1 1.5 1 0) ; second in a falf below the first

;; Both the segments are vertical and have no common points
(INTERSECTION-POINT 1 2 1 0.5 1 3 1 5)

;; The first segment is vertical
(INTERSECTION-POINT 1 2 1 1 0.5 1 1.5 2)
(INTERSECTION-POINT 1 2 1 1 0.5 0.5 2 1)

;; The second segment is vertical
(INTERSECTION-POINT 0.5 0.5 1.5 1.5 1 0.5 1 3)
(INTERSECTION-POINT 1.5 0.5 2.5 1.5 1 0.5 1 3)

;; The segments are parallel
(INTERSECTION-POINT 0.5 1 2 2 1 1 2.5 2)
;; The segments are intersected in (1.5, 1.5)
(INTERSECTION-POINT 1 1 2 2 1 2 2 1)
;; The segments are not intersected
(INTERSECTION-POINT 0.5 0.5 1 2 2 0.5 1.5 2)
