
(defpackage :aoc2018
  (:use :cl)
  (:export :read-file
           :do-file-lines
           :read-file-lines))

(in-package :aoc2018)

(defun read-file (path)
  (with-open-file (file path :element-type 'character
                             :external-format :utf-8)
    (let ((data (make-array (file-length file) :element-type 'character)))
      (read-sequence data file)
      data)))

(defmacro do-file-lines ((path line &optional result-var) &body body)
  (let ((stream (gensym "STREAM-")))
    `(with-open-file (,stream ,path)
       (handler-case
           (loop
             (let ((,line (read-line ,stream)))
               (progn
                 ,@body)))
         (end-of-file ()
           ,result-var)))))

(defun read-file-lines (path &optional (transform #'identity))
  (let ((lines nil))
    (do-file-lines (path line (nreverse lines))
      (push (funcall transform line) lines))))
