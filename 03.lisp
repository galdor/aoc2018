
(defpackage :aoc2018-03
  (:use :cl :ppcre :aoc2018)
  (:export :run))

(in-package :aoc2018-03)

(defstruct claim
  id
  x
  y
  width
  height)

(defun read-claim (string)
  (ppcre:register-groups-bind
      ((#'parse-integer id)
       (#'parse-integer x)
       (#'parse-integer y)
       (#'parse-integer width)
       (#'parse-integer height))
      ("^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$" string)
    (make-claim :id id
                :x x
                :y y
                :width width
                :height height)))

(defun make-fabric (claims)
  (let ((fabric (make-array '(1000 1000) :element-type 'integer
                                         :initial-element 0)))
    (dolist (claim claims fabric)
      (with-slots (x y width height) claim
        (loop for px from x below (+ x width) do
          (loop for py from y below (+ y height) do
            (incf (aref fabric px py))))))))

(defun count-claim-overlaps (fabric)
  (do ((size (array-total-size fabric))
       (i 0 (1+ i))
       (n 0))
      ((>= i size)
       n)
    (when (> (row-major-aref fabric i) 1)
      (incf n))))

(defun intact-claim-p (claim fabric)
  (with-slots (x y width height) claim
    (loop for px from x below (+ x width) do
      (loop for py from y below (+ y height) do
        (unless (= (aref fabric px py) 1)
          (return-from intact-claim-p nil)))))
  t)

(defun find-intact-claim (claims fabric)
  (find-if (lambda (claim)
             (intact-claim-p claim fabric))
           claims))

(defun run (path)
  (let* ((claims (read-file-lines path 'read-claim))
         (fabric (make-fabric claims)))
    (values (count-claim-overlaps fabric)
            (claim-id (find-intact-claim claims fabric)))))
