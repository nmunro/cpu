(defpackage cpu
  (:use :cl
        :cpu.cpu
        :cpu.memory
        :cpu.vm
        :cpu.instructions)
  (:export #:main))
(in-package :cpu)

(defun main ()
    (let* ((cpu (make-cpu
                    :mtech1
                    `(,(make-register :d0) ,(make-register :d1) ,(make-register :d2) ,(make-register :d3)
                      ,(make-register :d4) ,(make-register :d5) ,(make-register :d6) ,(make-register :d7)
                      ,(make-register :a0) ,(make-register :a1) ,(make-register :a2) ,(make-register :a3)
                      ,(make-register :a4) ,(make-register :a5) ,(make-register :a6) ,(make-register :a7))))
           (memory (make-memory 16))
           (vm (make-vm cpu memory)))

      (format t "+-------------------------------------------------------------------------------+~%")
      (format t "| Developed by NMunro, copyright 2020                                           |~%")
      (format t "|                                                                               |~%")
      (format t "| ~A  |~%" vm)
      (format t "+-------------------------------------------------------------------------------+~%")

      (move cpu :d0 5)
      (move memory #x0 1)
      (move memory #x1 2)
      (move memory #x2 3)
      (move memory #xf 16)

      (add cpu :d1 :d0 :d0)

      (dolist (reg (registers cpu))
        (format t "~A~%" reg))

      (format t "~A~%" (locations memory))))
