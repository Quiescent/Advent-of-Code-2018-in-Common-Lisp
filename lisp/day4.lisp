;;; day4 --- My solution to day4 -*-

;;; Commentary:
;; My solution to advent of code: day4

;;; Code:

;; # PART 1:

(ql:quickload "iterate")

(in-package :iter)

(load "read-file.lisp")

(defun day4-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((simple-data     (simplify-guard-data input-elements))
         (sleep-tables    (make-sleep-tables simple-data))
         (sleepiest       (find-sleepiest-guard-id sleep-tables))
         (sleepiest-table (gethash sleepiest sleep-tables)))
    (* sleepiest (position (reduce #'max sleepiest-table) sleepiest-table))))

(defun simplify-guard-data (guard-lines)
  (iter
    (for line in (sort guard-lines #'date-less-than))
    (with current-guard)
    (if (> (length line) 5)
        (setq current-guard (car (last line)))
        (collect (cons current-guard (nth 4 line))))))

(defun date-less-than (date-1 date-2)
  (< (date-to-number date-1)
     (date-to-number date-2)))

(defun date-to-number (date)
  (destructuring-bind (y mo d h m) (subseq date 0 5)
    (+ (* y  100000000)
       (* mo 1000000)
       (* d  10000)
       (* h  100)
       m)))

(defun make-sleep-tables (simplified-data)
  (iter
    (for ((id . sleep) (nil . wake) . nil) on simplified-data by #'cddr)
    (with guard-data = (make-hash-table))
    (for sleep-table = (gethash id guard-data (make-array '(60) :initial-element 0)))
    (when (null (gethash id guard-data))
      (setf (gethash id guard-data) sleep-table))
    (iter
      (for minute from sleep below wake)
      (incf (aref sleep-table minute)))
    (finally (return guard-data))))

(defun find-sleepiest-guard-id (sleep-tables)
  (iter
    (for (id sleep-table) in-hashtable sleep-tables)
    (for total-asleep = (reduce #'+ sleep-table))
    (with most-asleep-id)
    (with most-asleep-time)
    (when (or (null most-asleep-id)
              (> total-asleep most-asleep-time))
      (setq most-asleep-id   id
            most-asleep-time total-asleep))
    (finally (return most-asleep-id))))

;; # PART 2:

(defun day4-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((simple-data     (simplify-guard-data input-elements))
         (sleep-tables    (make-sleep-tables simple-data))
         (sleepiest       (find-guard-with-sleepiest-minute sleep-tables))
         (sleepiest-table (gethash sleepiest sleep-tables)))
    (* sleepiest (position (reduce #'max sleepiest-table) sleepiest-table))))

(defun find-guard-with-sleepiest-minute (sleep-tables)
  (iter
    (for (id sleep-table) in-hashtable sleep-tables)
    (for max-sleep = (reduce #'max sleep-table))
    (with most-asleep-id)
    (with most-sleepy-minute)
    (when (or (null most-asleep-id)
              (> max-sleep most-sleepy-minute))
      (setq most-asleep-id     id
            most-sleepy-minute max-sleep))
    (finally (return most-asleep-id))))

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day4-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day4-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-numbers "day4-part-1"))
        (input-2 (file-lines-numbers "day4-part-1")))
    (format t "
Part 1: ~s
" (day4-part-1 input-1))
    (format t "
Part 2: ~s
" (day4-part-2 input-2))))

