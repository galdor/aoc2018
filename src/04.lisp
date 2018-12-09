
(defpackage :aoc2018-04
  (:use :cl :ppcre :aoc2018)
  (:export :run))

(in-package :aoc2018-04)

(defstruct event
  month
  day
  hour
  minute
  guard
  awake-p)

(defun parse-event (string)
  (ppcre:register-groups-bind
      ((#'parse-integer month)
       (#'parse-integer day)
       (#'parse-integer hour)
       (#'parse-integer minute)
       suffix)
      ("^\\[\\d+-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\] (.*)"
       string)
    (let ((event (make-event :month month
                             :day day
                             :hour hour
                             :minute minute)))
      (cond
        ((string= suffix "falls asleep")
         (setf (event-awake-p event) nil))
        ((string= suffix "wakes up")
         (setf (event-awake-p event) t))
        (t
         (ppcre:register-groups-bind ((#'parse-integer guard))
             ("Guard #(\\d+)" suffix)
           (setf (event-guard event) guard
                 (event-awake-p event) t))))
      event)))

(defun event-sort-key (event)
  (with-slots (month day hour minute) event
    (+ (* month (* 86400 30))
       (* day 86400)
       (* hour 60)
       minute)))

(defun sort-events-by-date (events)
  (sort events #'< :key 'event-sort-key))

(defstruct shift
  guard
  sleep-table)

(defun collect-shifts (events)
  (let ((current-guard nil)
        (sleep-table nil)
        (last-minute nil)
        (shifts nil))
    (dolist (event events (nreverse shifts))
      (with-slots (guard hour minute awake-p) event
        (when guard
          (setf current-guard guard
                sleep-table (make-array 60 :element-type 'bit
                                           :initial-element 0)
                last-minute nil)
          (push (make-shift :guard guard
                            :sleep-table sleep-table)
                shifts))
        (when (and (null guard) (null last-minute))
          (setf last-minute minute))
        (unless (null last-minute)
          (do ((m last-minute (1+ m)))
              ((>= m minute))
            (setf (aref sleep-table m) (if awake-p 1 0)
                  last-minute minute)))))))

(defun guards-sleep-durations (shifts)
  (let ((durations nil))
    (dolist (shift shifts (sort durations #'> :key #'cdr))
      (with-slots (guard sleep-table) shift
        (let ((nb-minutes (count 1 sleep-table))
              (pair (assoc guard durations)))
          (if pair
              (incf (cdr pair) nb-minutes)
              (push (cons guard nb-minutes) durations)))))))

(defun guards-most-asleep-minutes (shifts)
  (let ((guards-asleep-minutes nil))
    (dolist (shift shifts)
      (with-slots (guard sleep-table) shift
        (let* ((pair (assoc guard guards-asleep-minutes))
               (asleep-minutes (if pair
                                   (cdr pair)
                                   (make-array 60 :element-type 'integer
                                                  :initial-element 0))))
          (do ((minutes (shift-sleep-table shift))
               (i 0 (1+ i)))
              ((>= i 60))
            (when (= (aref minutes i) 1)
              (incf (aref asleep-minutes i) 1)))
          (unless pair
            (push (cons guard asleep-minutes) guards-asleep-minutes)))))
    (dolist (guard-asleep-minutes guards-asleep-minutes)
      (do ((i 0 (1+ i))
           (max 0)
           (max-position nil))
          ((>= i (length (cdr guard-asleep-minutes)))
           (setf (cdr guard-asleep-minutes) (list max max-position)))
        (let ((nb-minutes (aref (cdr guard-asleep-minutes) i)))
          (when (> nb-minutes max)
            (setf max nb-minutes
                  max-position i)))))
    guards-asleep-minutes))

(defun guard-most-asleep-minute (guard guards-most-asleep-minutes)
  (third (assoc guard guards-most-asleep-minutes)))

(defun run (path)
  (let* ((events (sort-events-by-date
                  (read-file-lines path 'parse-event)))
         (shifts (collect-shifts events))
         (guards-most-asleep-minutes (guards-most-asleep-minutes shifts)))
    (values
     (let* ((guards-sleep-durations (guards-sleep-durations shifts))
            (most-asleep-guard (car (first guards-sleep-durations))))
       (* most-asleep-guard
          (guard-most-asleep-minute most-asleep-guard
                                    guards-most-asleep-minutes)))
     (let ((guard-most-asleep-minute
             (first (sort guards-most-asleep-minutes #'> :key #'second))))
       (* (first guard-most-asleep-minute)
          (third guard-most-asleep-minute))))))
