(defpackage cpu.parser
  (:use :cl))
(in-package :cpu.parser)

(defun read-file (path)
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(dolist (line (read-file #p"~/quicklisp/local-projects/cpu/examples/hello-world.asm"))
  (format t "~A~%" line))
