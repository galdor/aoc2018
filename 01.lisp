
(defpackage :aoc2018-01
  (:use :cl :aoc2018)
  (:export :run))

(in-package :aoc2018-01)

(defun compute-final-frequency (deltas)
  (reduce #'+ deltas))

(defun find-repeated-frequency (deltas)
  (let ((frequencies (make-hash-table))
        (frequency 0))
    (loop
      (dolist (delta deltas)
        (when (= (incf (gethash frequency frequencies 0)) 2)
          (return-from find-repeated-frequency frequency))
        (incf frequency delta)))))

(defun run (path)
  (let ((deltas (read-file-lines path #'parse-integer)))
    (values (compute-final-frequency deltas)
            (find-repeated-frequency deltas))))
