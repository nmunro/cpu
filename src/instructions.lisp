(defpackage cpu.instructions
  (:use :cl :cpu.cpu :cpu.memory)
  (:export #:move.b
           #:dc.b
           #:lea
           #:nop
           #:trap
           #:add
           #:sub
           #:mul
           #:div))
(in-package :cpu.instructions)

(defparameter functions '("lea"  "move.l" "move.w" "move.b" "trap"
                          "dc.b" "add"    "abcd"   "meh"    "nop"
                          "sub"  "mul"    "div"))

(defun lookup (instruction &key (reverse nil))
  (if reverse
    (nth (parse-integer instruction :radix 16) functions)
    (format nil "~X" (position instruction functions :test #'equal))))

(defgeneric move.b (vm location val)
  (:documentation "Moves a bytes sized value into either memory or a cpu register"))

(defgeneric move.w (vm location val)
  (:documentation "Moves a word sized value into either memory or a cpu register"))

(defgeneric move.l (vm location val)
  (:documentation "Moves a long-word sized value into either memory or a cpu register"))

(defmethod move.b (vm (location symbol) val)
  (setf (val (register location (cpu vm))) val))

(defmethod move.b (vm (location number) val)
  (multiple-value-bind (x y)
      (floor location)
    (write-memory (memory vm) y x val)))

(defun nop (vm))

(defun dc.b (vm label data)
  (reserve-memory (memory vm) label #x0 data))

(defun lea (vm label dest)
  (let* ((start (find-start-address (memory vm) label)))
    (setf (val (register dest (cpu vm))) start)))

(defun trap (vm trap-code)
  (when (= #xf trap-code)
    (let ((trap-task (val (register :d0 (cpu vm)))))
      (cond
        ((= 9  trap-task)
         (cl-user::quit))

        ((= 13 trap-task)
         (let ((start-addr (val (register :a1 (cpu vm)))))
           (format t "~A~%" (read-string-at (memory vm) start-addr))))

        ((= 14 trap-task)
         (let ((start-addr (val (register :a1 (cpu vm)))))
           (format t "~A" (read-string-at (memory vm) start-addr))))))))

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
