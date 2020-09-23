(defpackage cpu.vm
  (:use :cl :cpu.cpu :cpu.memory)
  (:export #:vm
           #:make-vm
           #:with-vm
           #:show-registers
           #:show-memory))
(in-package :cpu.vm)

(defclass vm ()
  ((cpu    :initarg :cpu    :initform (error "Must provide a CPU")  :reader cpu)
   (memory :initarg :memory :initform (error "Must provide memory") :reader memory)))

(defun make-vm (cpu memory)
  (make-instance 'vm :cpu cpu :memory memory))

(defmethod print-object ((vm vm) stream)
  (print-unreadable-object (vm stream)
    (format stream "Virtual Machine: ~A/~A " (cpu vm) (memory vm))))

(defun show-registers (vm)
  (display-registers (cpu vm)))

(defun show-memory (vm)
  (display-memory (memory vm)))

(defmacro with-vm ((name obj) &body body)
  `(progn
    ,@(mapcar (lambda (form) (append `(,(car form) ,obj) (cdr form))) body)))
