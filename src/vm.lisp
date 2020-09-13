(defpackage cpu.vm
  (:use :cl)
  (:export #:vm
           #:make-vm))
(in-package :cpu.vm)

(defclass vm ()
  ((cpu    :initarg :cpu    :initform (error "Must provide a CPU")  :reader cpu)
   (memory :initarg :memory :initform (error "Must provide memory") :reader memory)))

(defun make-vm (cpu memory)
  (make-instance 'vm :cpu cpu :memory memory))

(defmethod print-object ((vm vm) stream)
  (print-unreadable-object (vm stream)
    (format stream "Virtual Machine: ~A/~A " (cpu vm) (memory vm))))
