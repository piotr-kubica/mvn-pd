;;;; mvn-pd-test.asd

(asdf:defsystem :mvn-pd-test
  :description "Tests for mvn-pd"
  :author "Piotr Kubica <piotr.kubica87@gmail.com>"
  :license "MIT Lincese"
  :depends-on (:mvn-pd
               :fiveam)
  :serial t
  :components ((:file "package")
               (:file "tests")))

