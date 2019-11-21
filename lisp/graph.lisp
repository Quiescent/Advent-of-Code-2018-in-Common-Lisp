(ql:quickload "iterate")

(defpackage :graph
  (:use :common-lisp)
  (:use :iter)
  (:export tuples-to-graph))

(in-package :graph)

(defun tuples-to-graph (tuples)
  (let ((graph (make-hash-table :test #'equal)))
    (dolist (tuple tuples graph)
      (when (consp (cdr tuple))
        (error "Expected tuple."))
      (let ((from (car tuple))
            (to   (cdr tuple)))
        (push to (gethash from graph))))))

(defmacro-driver (FOR var DFS-ACROSS-GRAPH graph STARTING-FROM start-node)
  "Search GRAPH depth-first, starting at STARTING-FROM, binding VAR.

TODO: Supports the BY keyword which should be a one argument function,
accepting a list of the nodes available in the search now and should
produce the next node."
  (let ((current-nodes (gensym))
        (g             (gensym))
        (kwd           (if generate 'generate 'for))
        (non-default-search)
        (by))
    `(progn
       (with ,g             = ,graph)
       (with ,current-nodes = (list ,start-node))
       (while ,current-nodes)
       (initially (setq ,var (if ,(and non-default-search by)
                                 (funcall ,by ,current-nodes)
                                 (car ,current-nodes))))
       (,kwd ,var next (if ,(and non-default-search by)
                           (funcall ,by ,current-nodes)
                           (car ,current-nodes)))
       (setq ,current-nodes (delete ,var ,current-nodes))
       (setq ,current-nodes (nconc (gethash ,var ,g) ,current-nodes)))))

(let ((graph (tuples-to-graph '((#\a . #\b) (#\a . #\c) (#\c . #\d) (#\b . #\f)))))
  (iter
    (for x dfs-across-graph graph starting-from #\a)
    (print x)))
