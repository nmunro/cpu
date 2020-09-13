(defpackage cpu.instructions
  (:use :cl)
  (:export #:move
           #:lea
           #:nop
           #:trap
           #:add
           #:sub
           #:mul
           #:div))
(in-package :cpu.instructions)

(defgeneric move (cpu location val)
  (:documentation "Moves a value into either memory or a cpu register"))

(defmethod move (cpu (location symbol) val)
  (setf (cpu.cpu:val (cpu.cpu:register location cpu)) val))

(defmethod move (memory (location number) val)
  (multiple-value-bind (x y)
      (floor location)
    (setf (aref (cpu.memory:locations memory) y x) val)))

(defun nop ())

(defun lea (cpu src dest)
  (setf (cpu.cpu:val (cpu.cpu:register dest cpu)) src))

(defun trap (cpu memory trap-code)
  (when (= #xf trap-code)
    (let ((trap-task (cpu.cpu:val (cpu.cpu:register :d0 cpu))))
      (cond
        ((= 9  trap-task) (cl-user::quit))
        ((= 13 trap-task) (format t "~A~%" (cpu.memory:address memory (cpu.cpu:val (cpu.cpu:register :d0 cpu)))))
        ((= 14 trap-task) (format t "~A"   (cpu.memory:address memory (cpu.cpu:val (cpu.cpu:register :d0 cpu)))))))))

(defun add (cpu destination source1 source2)
  (setf (cpu.cpu:val (cpu.cpu:register destination cpu)) (+ (cpu.cpu:val (cpu.cpu:register source1 cpu)) (cpu.cpu:val (cpu.cpu:register source2 cpu)))))

(defun sub (cpu destination source1 source2)
  (setf (cpu.cpu:val (cpu.cpu:register destination cpu)) (- (cpu.cpu:val (cpu.cpu:register source1 cpu)) (cpu.cpu:val (cpu.cpu:register source2 cpu)))))

(defun mul (cpu destination source1 source2)
  (setf (cpu.cpu:val (cpu.cpu:register destination cpu)) (* (cpu.cpu:val (cpu.cpu:register source1 cpu)) (cpu.cpu:val (cpu.cpu:register source2 cpu)))))

(defun div (cpu destination source1 source2)
  (setf (cpu.cpu:val (cpu.cpu:register destination cpu)) (/ (cpu.cpu:val (cpu.cpu:register source1 cpu)) (cpu.cpu:val (cpu.cpu:register source2 cpu)))))
