
(defpackage :aoc2018-05
  (:use :cl :aoc2018)
  (:export :run))

(in-package :aoc2018-05)

(defparameter *units*
  (loop for c
        from (char-code #\a)
          to (char-code #\z)
        collect (code-char c)))

(defun unit-equal (u1 u2)
  (char-equal u1 u2))

(defun unit-react-p (u1 u2)
  (and (char-equal u1 u2) (char/= u1 u2)))

(defun polymer-react (polymer &key exclude-unit)
  (let ((result (make-array (length polymer)
                            :element-type (array-element-type polymer)
                            :fill-pointer 0)))
    (do ((i 0 (1+ i)))
        ((>= i (length polymer))
         result)
      (let ((unit (char polymer i)))
        (unless (and exclude-unit (unit-equal exclude-unit unit))
          (vector-push unit result)
          (loop while (>= (length result) 2) do
            (let ((u1 (char result (- (length result) 1)))
                  (u2 (char result (- (length result) 2))))
              (unless (unit-react-p u1 u2)
                (return))
              (decf (fill-pointer result) 2))))))))

(defun polymer-reactions (polymer)
  (mapcar (lambda (unit)
            (polymer-react polymer :exclude-unit unit))
          *units*))

(defun shortest-polymer (polymer)
  (car (sort (polymer-reactions polymer) #'< :key #'length)))

(defun run (path)
  (let ((polymer (read-file path)))
    (values (length (polymer-react polymer))
            (length (shortest-polymer polymer)))))
