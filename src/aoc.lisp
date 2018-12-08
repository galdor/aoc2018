
(defpackage :aoc2018
  (:use :cl)
  (:export :do-file-lines
           :read-file-lines))

(in-package :aoc2018)

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
