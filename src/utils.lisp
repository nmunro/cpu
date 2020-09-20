(defpackage cpu.utils
  (:use :cl)
  (:export #:char->hex
           #:hex->char))
(in-package :cpu.utils)

(defun char->hex (char)
  (format nil "~X" (char-code char)))

(defun hex->char (hex)
  (code-char (coerce hex 'integer)))
