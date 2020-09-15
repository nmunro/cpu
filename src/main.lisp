(defpackage cpu
  (:use :cl
        :cpu.cpu
        :cpu.memory
        :cpu.vm
        :cpu.instructions)
  (:export #:main))
(in-package :cpu)

(defun main ()
  (let ((vm (make-vm (make-cpu :mtech1 (make-registers :a0 :a1 :a2 :a3 :a4 :a5 :a6 :a7 :d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7) 8 :mhz)
                     (make-memory 4000))))
      (format t "+-------------------------------------------------------------------------------------------+~%")
      (format t "| Developed by NMunro, copyright 2020                                                       |~%")
      (format t "|                                                                                           |~%")
      (format t "| ~A    |~%" vm)
      (format t "+-------------------------------------------------------------------------------------------+~%")

      (move.b vm #x0 "hello world")
      (lea vm #x0 :a1)
      (move.b vm :d0 13)
      (trap vm #xf)
      (move.b vm :d0 9)

      (show-registers vm)
      ;(show-memory vm)

      (trap vm #xf)

      ; Keeps cpu idling if it wasn't halted
      (do () (nil)
        (nop))))
