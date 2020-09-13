(defpackage cpu/tests/main
  (:use :cl
        :cpu
        :rove))
(in-package :cpu/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cpu)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
