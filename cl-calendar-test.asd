(defsystem "cl-calendar-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("cl-calendar"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-calendar"))))
  :description "Test system for cl-calendar"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
