(defpackage cpu.memory
  (:use :cl :cpu.utils)
  (:export #:convert-data
           #:memory
           #:locations
           #:size
           #:display-memory
           #:reserve-memory
           #:retrieve-memory
           #:reclaim-memory
           #:write-memory
           #:read-string
           #:make-memory))
(in-package :cpu.memory)

(defclass memory ()
  ((size      :initarg :size      :initform (error "Must provide a size")    :reader size)
   (locations :initarg :locations :initform (error "Must provide locations") :reader locations)
   (lookup    :initarg :lookup    :initform (make-hash-table)                :accessor lookup)))

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

(defun read-string (data)
  (format nil "~A" (coerce (mapcar (lambda (c) (hex->char (parse-integer c :radix 16))) data) 'string)))

(defun reserve-memory (mem label offset address data)
  (setf (gethash label (lookup mem))
        `(,offset ,address ,(length (write-memory mem offset address data)))))

(defun retrieve-memory (mem label)
  (let* ((record (gethash label (lookup mem)))
         (offset (first record))
         (address (second record))
         (size (third record))
         (data '()))
    (dotimes (x size)
      (setf data (append data `(,(aref (locations mem) offset address 0))))
      (if (= 15 address)
        (progn
          (setf address 0)
          (incf offset))
        (incf address)))
    data))

(defun reclaim-memory (mem label)
  (remhash label (lookup mem)))

(defgeneric convert-data (data)
  (:documentation "Converts data to raw hexadecimal bits"))

(defmethod convert-data ((data number))
  (convert-data (format nil "~X" data)))

(defmethod convert-data ((data character))
  (convert-data (format nil "~A" data)))

(defmethod convert-data ((data string))
  (mapcar (lambda (c) (char->hex c)) (coerce data 'list)))

(defun write-memory (mem offset address data)
  (let ((hexes (convert-data data)))
    (dolist (hex hexes)
      (setf (aref (locations mem) offset address 0) hex)
      (if (= 15 address)
          (progn
            (setf address 0)
            (incf offset))
          (incf address)))
    hexes))

(let ((mem (make-memory 16)))
  (write-memory mem 0 0 "Hi, you ok?")
  (write-memory mem 1 0 9)
  (write-memory mem 2 0 #\N)
  (reserve-memory mem 'greeting 3 0 "Hey")
  (reserve-memory mem 'msg 7 0 "Hope you are ok!!!")
  (display-memory mem)
  (format t "~A~%" (read-string (retrieve-memory mem 'greeting)))
  (format t "~A~%" (read-string (retrieve-memory mem 'msg)))
  (reclaim-memory mem 'greeting)
  (reclaim-memory mem 'msg))
