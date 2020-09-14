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

      (format t "+---------------------------------------------------------------------------------+~%")
      (format t "| Developed by NMunro, copyright 2020                                             |~%")
      (format t "|                                                                                 |~%")
      (format t "| ~A  |~%" vm)
      (format t "+---------------------------------------------------------------------------------+~%")

      (move memory #x0 "hello world")
      (lea cpu #x0 :a1)
      (move cpu :d0 13)
      (trap cpu memory #xf)
      (move cpu :d0 9)

      (dolist (reg (registers cpu))
        (format t "~A~%" reg))
      (format t "~A~%" (locations memory))

      (trap cpu memory #xf)

      ; Keeps cpu idling if it wasn't halted
      (do () (nil)
        (nop))
      (format t "Done!~%")))
