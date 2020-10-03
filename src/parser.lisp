(defpackage cpu.parser
  (:use :cl :cpu.instructions))
(in-package :cpu.parser)

(defun read-file (path)
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (loop for line = (read-line stream nil) while line collect line)))

(defun map-to-memory (lines)
  (dolist (line lines)
    (format t "~{~A~^, ~}~%" line)))

(defun translate (line)
  ; The following has a bug when using DC.B
  `(,(getf line :label)
    ,(getf line :function)
    ,@(uiop:split-string (getf line :args) :separator ",")))

(defun parse-line (line)
  (let ((data (remove-if #'(lambda (x) (string= x "")) (uiop:split-string line :separator " ") :start 1)))
    (translate `(:label   ,(car data)
                :function ,(lookup (cadr data))
                :args     ,(cdr (remove-if (lambda (word) (string= "" word)) (rest data)))))))

(defun remove-junk (line)
  (cond
    ((string= "" (string-trim " " line))
     nil)

    ((string= "*-" (subseq line 0 2))
     nil)

    (t line)))

(let ((code (read-file #p"~/quicklisp/local-projects/cpu/examples/hello-world.asm")))
    (map-to-memory (mapcar #'parse-line (remove-if (complement #'remove-junk) code))))

(lookup "end")
(lookup (lookup "div") :reverse t)
