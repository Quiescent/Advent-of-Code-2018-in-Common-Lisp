;;; day7 --- My solution to day7 -*-

;;; Commentary:
;; My solution to advent of code: day7

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")

(defpackage :day7
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :iter))

(in-package :day7)

;; # PART 1:

(defun day7-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((graph (parse-graph input-elements)))
    (find-ordering graph)))

(defun find-ordering (graph)
  (iter
    (with reversed = (reverse-graph graph))
    (with searched)
    (setf iter::candidate-nodes (sort iter::candidate-nodes #'char-lessp))
    (for node
         dfs-across-graph-without-duplicates graph
         by (find-if (lambda (node)
                       (eq (length (intersection searched
                                                 (gethash node reversed)))
                           (length (gethash node reversed))))
                     iter::candidate-nodes))
    (push node searched)
    (collect node into ordering)
    (finally (return (map 'string #'identity ordering)))))

(defun parse-graph (input-elements)
  (tuples-to-graph
   (mapcar (lambda (line)
             (cons (aref (nth 1 line) 0)
                   (aref (nth 7 line) 0)))
           input-elements)))

;; # PART 2:

(defun day7-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((graph (parse-graph input-elements)))
    (work-together graph)))

(defun char-score (char)
  (+ 60
     (cond
       ((char-equal char #\A) 1)
       ((char-equal char #\B) 2)
       ((char-equal char #\C) 3)
       ((char-equal char #\D) 4)
       ((char-equal char #\E) 5)
       ((char-equal char #\F) 6)
       ((char-equal char #\G) 7)
       ((char-equal char #\H) 8)
       ((char-equal char #\I) 9)
       ((char-equal char #\J) 10)
       ((char-equal char #\K) 11)
       ((char-equal char #\L) 12)
       ((char-equal char #\M) 13)
       ((char-equal char #\N) 14)
       ((char-equal char #\O) 15)
       ((char-equal char #\P) 16)
       ((char-equal char #\Q) 17)
       ((char-equal char #\R) 18)
       ((char-equal char #\S) 19)
       ((char-equal char #\T) 20)
       ((char-equal char #\U) 21)
       ((char-equal char #\V) 22)
       ((char-equal char #\W) 23)
       ((char-equal char #\X) 24)
       ((char-equal char #\Y) 25)
       ((char-equal char #\Z) 26))))

(defun create-workers (n)
  (map 'list
       #'identity
       (make-array (list n) :initial-element (cons 0 nil))))

(defun assign-work (workers work)
  (cond
    ((or (null workers) (null work)) workers)
    ((> (caar workers) 0)            (cons (car workers)
                                           (assign-work (cdr workers)
                                                        work)))
    (t                               (cons (cons (char-score (car work)) (car work))
                                           (assign-work (cdr workers)
                                                        (cdr work))))))

#+nil
(print (assign-work '((0) (0) (0) (0) (0))
                    (list #\A #\B #\C)))

(defun work-together (graph)
  (let* ((workers   (create-workers 5))
         (reversed  (reverse-graph graph))
         (all-nodes (iter (for (node ends) in-hashtable graph)
                          (unioning  ends into all-ends)
                          (adjoining node into all-starts)
                          (finally (return (union all-ends all-starts)))))
         (remaining all-nodes)
         (completed)
         (searched))
    (labels
        ((requirements-met (node) (eq (length (intersection completed
                                                            (gethash node reversed)))
                                      (length (gethash node reversed))))
         (tick-worker (worker) (if (> (car worker) 0)
                                   (cons (1- (car worker))
                                         (cdr worker))
                                   worker))
         (tick (times)
           (if (eq (length (intersection completed all-nodes))
                   (length all-nodes))
               (1- times)
               (progn
                 (setf workers   (map-into workers #'tick-worker workers)
                       completed (remove nil (union completed
                                                    (mapcar (lambda (worker) (cdr worker))
                                                            (remove-if-not (lambda (worker) (eq 0 (car worker)))
                                                                           workers)))))
                 (let* ((available    (sort (copy-seq (remove-if-not #'requirements-met remaining)) #'char-lessp))
                        (assigned-cnt (min (length available)
                                           (count-if (lambda (worker) (eq 0 (car worker))) workers))))
                   (setf searched (append searched (subseq available 0 assigned-cnt)))
                   (setf remaining (set-difference remaining searched))
                   (setf workers (assign-work workers available))
                   (tick (1+ times)))))))
      (tick 0))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '(("Step" "C" "must" "be" "finished" "before" "step" "A" "can" "begin.")
                   ("Step" "C" "must" "be" "finished" "before" "step" "F" "can" "begin.")
                   ("Step" "A" "must" "be" "finished" "before" "step" "B" "can" "begin.")
                   ("Step" "A" "must" "be" "finished" "before" "step" "D" "can" "begin.")
                   ("Step" "B" "must" "be" "finished" "before" "step" "E" "can" "begin.")
                   ("Step" "D" "must" "be" "finished" "before" "step" "E" "can" "begin.")
                   ("Step" "F" "must" "be" "finished" "before" "step" "E" "can" "begin.")))
        (expected-1 '())
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day7-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day7-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-words "day7-part-1"))
        (input-2 (file-lines-words "day7-part-1")))
    (format t "
Part 1: ~s
" (day7-part-1 input-1))
    (format t "
Part 2: ~s
" (day7-part-2 input-2))))

