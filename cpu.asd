(defsystem "cpu"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "cpu")
                 (:file "memory")
                 (:file "vm")
                 (:file "instructions")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cpu/tests"))))

(defsystem "cpu/tests"
  :author ""
  :license ""
  :depends-on ("cpu"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cpu"
  :perform (test-op (op c) (symbol-call :rove :run c)))
