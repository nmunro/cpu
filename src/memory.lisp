(defpackage cpu.memory
  (:use :cl :cpu.utils)
  (:export #:memory
           #:locations
           #:size
           #:write-memory
           #:read-memory
           #:make-memory))
(in-package :cpu.memory)

(defclass memory ()
  ((size      :initarg :size      :initform (error "Must provide a size")    :reader size)
   (locations :initarg :locations :initform (error "Must provide locations") :reader locations)))

(defun make-memory (size)
  (let ((base-size 16))
    (make-instance 'memory :size (* size base-size) :locations (make-array `(,size ,base-size 1)))))

(defmethod print-object ((memory memory) stream)
  (print-unreadable-object (memory stream)
    (format stream "Memory Size: ~A bytes" (size memory))))

(defun display-memory (mem)
  (let ((tbl (ascii-table:make-table '("Offset" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") :header (format nil "Memory: ~A bytes" (size mem)))))
    (dotimes (x (car (array-dimensions (locations mem))))
      (let ((data '()))
        (dotimes (y 16)
          (push (format nil "~A" (aref (locations mem) x y 0)) data))

        (ascii-table:add-row tbl (append `(,x) (reverse data)))))
    (ascii-table:display tbl)))

(defun read-memory (mem offset size)
  (let ((data '()))
    (dotimes (x size)
      (push (aref (locations mem) offset x 0) data))
    (format nil "~A" (reverse data))))

(defun write-memory (mem offset size data)
  (let ((hexes (mapcar (lambda (c) (char->hex c)) (coerce data 'list))))
    (dolist (hex hexes)
      (setf (aref (locations mem) offset size 0) hex)
      (if (= 15 size)
          (progn
            (setf size 0)
            (incf offset))
          (incf size)))))

(let ((mem (make-memory 16)))
  (write-memory mem 0 0 "f")
  (write-memory mem 0 15 "hi")
  (write-memory mem 2 1 "f")
  (write-memory mem 3 0 "Hi, you ok?")
  (display-memory mem)
  (format t "~A~%" (read-memory mem 3 11)))
