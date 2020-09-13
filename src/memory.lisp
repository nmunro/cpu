(defpackage cpu.memory
  (:use :cl)
  (:export #:memory
           #:locations
           #:size
           #:address
           #:make-memory))
(in-package :cpu.memory)

(defclass memory ()
  ((size      :initarg :size      :initform (error "Must provide a size")    :reader size)
   (locations :initarg :locations :initform (error "Must provide locations") :reader locations)))

(defun make-memory (size)
  (let ((base-size 16))
    (make-instance 'memory :size (* 16 base-size) :locations (make-array `(,base-size ,size)))))

(defun address (memory index)
  (aref (locations memory) 0 0))

(defmethod print-object ((memory memory) stream)
  (print-unreadable-object (memory stream)
    (format stream "Memory Size: ~A bytes" (size memory))))
