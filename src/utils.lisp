(defpackage cpu.utils
  (:use :cl)
  (:export #:char->hex
           #:hex->char))
(in-package :cpu.utils)

(defun char->hex (char)
  (parse-integer (write-to-string (char-code char) :base 16)))

(defun hex->char (hex)
  (code-char (coerce hex 'integer)))

(hex->char #x20)
