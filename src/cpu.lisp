(defpackage cpu.cpu
  (:use :cl)
  (:export #:cpu-register
           #:make-register
           #:make-registers
           #:display-registers
           #:register
           #:cpu
           #:make-cpu
           #:registers
           #:val
           #:get-register))
(in-package :cpu.cpu)

(defclass cpu-register ()
  ((name  :initarg  :name  :initform (error "Must provide a name")             :reader   name)
   (size  :initarg  :size  :initform 32                                        :reader   size)
   (val   :initarg  :val   :initform nil                                       :accessor val)))

(defun make-register (name)
  (make-instance 'cpu-register :name name))

(defun make-registers (&rest names)
  (mapcar (lambda (name) (make-register name)) names))

(defun register (item cpu)
  (dolist (register (registers cpu))
    (when (eq item (name register))
      (return-from register register))))

(defmethod print-object ((reg cpu-register) stream)
  (print-unreadable-object (reg stream)
    (format stream "Register (~A bytes): ~A -> ~A" (size reg) (name reg) (val reg))))

(defclass cpu ()
  ((name              :initarg :name              :initform (error "Must provide a name")            :reader name)
   (speed             :initarg :speed             :initform (error "Must provide a speed"            :reader speed))
   (clock-range       :initarg :clock-range       :initform :mhz                                     :reader clock-range)
   (clock-speed       :initarg :clock-speed       :initform 8                                        :reader clock-speed)
   (registers         :initarg :registers         :initform (error "Must provide registers")         :reader registers)
   (program-counter   :initarg :program-counter   :initform 0                                        :reader program-counter)
   (bit               :initarg :bit               :initform 1                                        :reader bit-length)
   (nybble            :initarg :nybble            :initform 4                                        :reader nybble-length)
   (byte              :initarg :byte              :initform 8                                        :reader byte-length)
   (word              :initarg :word              :initform 16                                       :reader word-length)
   (long-word         :initarg :long-word         :initform 32                                       :reader long-word-length)))

(defun make-cpu (name registers speed unit)
  (cond
    ((eq unit :hz)
     (make-instance 'cpu :name name :registers registers :speed (/ speed 1) :clock-range unit :clock-speed speed))

    ((eq unit :khz)
     (make-instance 'cpu :name name :registers registers :speed (/ speed 1000) :clock-range unit :clock-speed speed))

    ((eq unit :mhz)
     (make-instance 'cpu :name name :registers registers :speed (/ speed 1000000) :clock-range unit :clock-speed speed))))

(defmethod print-object ((cpu cpu) stream)
  (print-unreadable-object (cpu stream)
    (format stream "CPU: ~A (~A ~A), word-length: ~A" (name cpu) (clock-speed cpu) (clock-range cpu) (word-length cpu))))

(defun display-registers (cpu)
  (let ((tbl (ascii-table:make-table '("Name" "Value") :header "Registers")))
    (dolist (reg (registers cpu))
      (ascii-table:add-row tbl `(,(name reg) ,(val reg))))
    (ascii-table:display tbl)))
