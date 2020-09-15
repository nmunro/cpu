(defpackage cpu.instructions
  (:use :cl :cpu.cpu :cpu.memory)
  (:export #:move.b
           #:lea
           #:nop
           #:trap
           #:add
           #:sub
           #:mul
           #:div))
(in-package :cpu.instructions)

(defgeneric move.b (vm location val)
  (:documentation "Moves a value into either memory or a cpu register"))

(defmethod move.b (vm (location symbol) val)
  (setf (val (register location (cpu vm))) val))

(defmethod move.b (vm (location number) val)
  (multiple-value-bind (x y)
      (floor location)
    (setf (aref (locations (memory vm)) y x) val)))

(defun nop (vm))

(defun dc.b (vm label data)
  0)

(defun lea (vm src dest)
  (setf (val (register dest (cpu vm))) src))

(defun trap (vm trap-code)
  (when (= #xf trap-code)
    (let ((trap-task (val (register :d0 (cpu vm)))))
      (cond
        ((= 9  trap-task)
         (cl-user::quit))

        ((= 13 trap-task)
         (format t "~A~%" (address (memory vm) (val (register :d0 (cpu vm))))))

        ((= 14 trap-task)
         (format t "~A"   (address (memory vm) (val (register :d0 (cpu vm))))))))))

(defun add (vm destination source1 source2)
  (setf (val    (register destination (cpu vm)))
        (+ (val (register source1     (cpu vm)))
           (val (register source2     (cpu vm))))))

(defun sub (vm destination source1 source2)
  (setf (val    (register destination (cpu vm)))
        (- (val (register source1     (cpu vm)))
           (val (register source2     (cpu vm))))))

(defun mul (vm destination source1 source2)
  (setf (val    (register destination (cpu vm)))
        (* (val (register source1     (cpu vm)))
           (val (register source2     (cpu vm))))))

(defun div (vm destination source1 source2)
  (setf (val    (register destination (cpu vm)))
        (/ (val (register source1     (cpu vm)))
           (val (register source2     (cpu vm))))))
