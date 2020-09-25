(defpackage cpu
  (:use :cl
        :cpu.cpu
        :cpu.memory
        :cpu.vm
        :cpu.instructions)
  (:export #:main))
(in-package :cpu)

(defun main ()
  (format t "+-------------------------------------------------------------------------------------------+~%")
  (format t "|                                                                                           |~%")
  (format t "| Developed by NMunro, copyright 2020                                                       |~%")
  (format t "|                                                                                           |~%")
  (format t "+-------------------------------------------------------------------------------------------+~%")

  (let ((vm (make-vm (make-cpu :mtech1 (make-registers :a0 :a1 :a2 :a3 :a4 :a5 :a6 :a7 :d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7) 8 :mhz)
                     (make-memory 16))))
    (format t "CPU: ~A~%" (cpu vm))

    (with-vm (vm (run-speed (cpu vm)))
      (dc.b :message "hello world")
      (lea :message :a1)
      (move.b :d0 13)
      (trap #xf)
      (move.b :d0 9)

      (show-registers)
      (show-memory)

      (trap #xf))

    ; Keeps cpu idling if it wasn't halted
    (do () (nil)
      (nop))))
