
(defpackage :aoc2018-02
  (:use :cl :aoc2018)
  (:export :run))

(in-package :aoc2018-02)

(defun count-id-letters (id)
  (let ((counts nil))
    (do ((i 0 (1+ i)))
        ((>= i (length id))
         counts)
      (let ((pair (assoc (char id i) counts)))
        (if pair
            (incf (cdr pair))
            (push (cons (char id i) 1) counts))))))

(defun compute-ids-checksum (ids)
  (let ((count-2 0)
        (count-3 0))
    (dolist (id ids)
      (let ((counts (count-id-letters id)))
        (when (find 2 counts :key 'cdr)
          (incf count-2))
        (when (find 3 counts :key 'cdr)
          (incf count-3))))
    (* count-2 count-3)))

(defun compute-ids-distance (id1 id2)
  (let ((distance 0))
    (do ((i 0 (1+ i)))
        ((>= i (length id1))
         distance)
      (when (char/= (char id1 i) (char id2 i))
        (incf distance)))))

(defun find-prototype-ids (ids)
  (dolist (id1 ids)
    (dolist (id2 ids)
      (when (= (compute-ids-distance id1 id2) 1)
        (return-from find-prototype-ids (values id1 id2))))))

(defun find-ids-common-letters (id1 id2)
  (let ((common-letters (make-array (length id1) :element-type 'character
                                                 :fill-pointer 0)))
    (do ((i 0 (1+ i)))
        ((>= i (length id1))
         common-letters)
      (when (char= (char id1 i) (char id2 i))
        (vector-push (char id1 i) common-letters)))))

(defun find-prototype-ids-common-letters (ids)
  (multiple-value-call 'find-ids-common-letters (find-prototype-ids ids)))

(defun run (path)
  (let ((ids (read-file-lines path)))
    (values (compute-ids-checksum ids)
            (find-prototype-ids-common-letters ids))))
