;;;; unit-tests.lisp

(in-package :mvn-pd-test)

(test str2keyword-test
  "str2keyword coerce string to keyword"
  (is (eq :|test| (str2keyword "test")))
  (is (eq :|Test| (str2keyword "Test")))
  (is (eq ':|test| (str2keyword "test"))))

(test eq-keyword-test
  "comapre keywords ignoring case"
  (is-true (eq-keyword :a :a))
  (is-false (eq-keyword :b :a))
  (is-true (eq-keyword :A :a)))

(test find-elems-test
  "find elements in list"
  (is (eq '(4 4 4) (find-elems '(1 2 (3 (4 4)) 4 (5 (4))) 4 #'equal))))
