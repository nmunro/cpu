(defpackage cpu.memory
  (:use :cl :cpu.utils)
  (:export #:convert-data
           #:find-start-address
           #:memory
           #:locations
           #:size
           #:display-memory
           #:reserve-memory
           #:retrieve-memory
           #:reclaim-memory
           #:write-memory
           #:read-string
           #:read-string-at
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

(defun read-string-at (mem address)
  (let ((obj (find-by-start-address mem address)))
    (read-string (retrieve-memory mem obj))))

(defun reserve-memory (mem label address data)
  (setf (gethash label (lookup mem))
        `(,(format nil "~X" address) ,(length (write-memory mem address data)))))

(defun retrieve-memory (mem label)
  (let* ((record (gethash label (lookup mem)))
         (start-address (parse-integer (first record) :radix 16))
         (size (second record))
         (data '()))
    (dotimes (x size)
      (multiple-value-bind (offset address)
          (get-offset-and-address start-address)
        (setf data (append data `(,(aref (locations mem) offset address 0))))
        (incf start-address)))
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

(defun write-memory (mem addr data)
  (multiple-value-bind (offset address)
      (get-offset-and-address addr)
    (let ((hexes (convert-data data)))
      (dolist (hex hexes)
        (setf (aref (locations mem) offset address 0) hex)
        (if (= 15 address)
          (progn
            (setf address 0)
            (incf offset))
          (incf address)))
    hexes)))

(defun find-by-start-address (mem address)
  ;; Loop and find the variable that starts at 'start'
  (loop for k being the hash-keys of (lookup mem)
        for v being the hash-values of (lookup mem)
        do (when (= address (parse-integer (first v) :radix 16))
            (return-from find-by-start-address k))))

(defun find-start-address (mem label)
  (parse-integer (first (gethash label (lookup mem))) :radix 16))

(defun get-offset-and-address (addr)
  (multiple-value-bind (offset address)
      (truncate addr 16)
    (values offset address)))

;(let ((mem (make-memory 16)))
;  (write-memory mem #x0 "Hi, you ok?")
;  (write-memory mem #x10 9)
;  (write-memory mem #x20 #\N)
;  (reserve-memory mem 'greeting #x30 "Hey")
;  (reserve-memory mem 'msg #x70 "Hope you are ok!!!")
; (display-memory mem)
; (format t "Debug 1: ~A~%" (retrieve-memory mem 'greeting))
; (format t "Debug 2: ~A~%" (read-string (retrieve-memory mem 'greeting)))
; (format t "Debug 3: ~A~%" (find-by-start-address mem #x30))
; (format t "Debug 4: ~X~%" (find-start-address mem 'msg))
; (format t "Debug 5: ~A~%" (read-string (retrieve-memory mem 'msg)))
; (format t "Debug 6: ~A~%" (get-address mem #x10))
; (format t "Debug 7: ~A~%" (get-data mem #x70 18))
; (format t "Debug 8: ~A~%" (read-string-at mem #x70))
; (reclaim-memory mem 'greeting)
; (reclaim-memory mem 'msg)
; (format t "Done!~%"))
