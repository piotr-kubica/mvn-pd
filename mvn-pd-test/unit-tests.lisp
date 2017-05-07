;;;; unit-tests.lisp

(in-package :mvn-pd-test)

(test str2keyword-test
  "str2keyword tests"
  (is (eq :|test| (str2keyword "test")) "lowercase keyword equal to string lowercase" ))


