(defpackage cpu.parser
  (:use :cl))
(in-package :cpu.parser)

(defun read-file (path)
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (loop for line = (read-line stream nil) while line collect line)))

(defun map-to-memory (lines)
  (dolist (line lines)
    (format t "~{~A~^, ~}~%" line)))

(defun translate (line)
  `(,(getf line :label) ,(getf line :function) ,(getf line :args)))

(defun parse-line (line)
  (let ((data (remove-if #'(lambda (x) (string= x "")) (uiop:split-string line :separator " ") :start 1)))
    (translate `(:label    ,(first data)
                 :function ,(cadr data)
                 :args     ,@(uiop:split-string (first (cdr (remove-if (lambda (word) (string= "" word)) (rest data)))) :separator ",")))))

(defun remove-junk (line)
  (cond
    ((string= "" (string-trim " " line))
     nil)

    ((string= "*-" (subseq line 0 2))
     nil)

    (t line)))

(let ((code (read-file #p"~/quicklisp/local-projects/cpu/examples/hello-world.asm")))
    (map-to-memory (mapcar #'parse-line (remove-if (complement #'remove-junk) code))))
