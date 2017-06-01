;;;; unit-tests.lisp

(in-package :mvn-pd-test)

(test str2keyword-test
  "str2keyword coerce string to keyword"
  (is (eq :|test| (str2keyword "test")))
  (is (eq :|Test| (str2keyword "Test")))
  (is (eq ':|TEST| (str2keyword "TEST"))))

(test eq-keyword-test
  "comapre keywords ignoring case"
  (is-true (eq-keyword :a :a))
  (is-false (eq-keyword :b :a))
  (is-true (eq-keyword :A :a))
  (is-true (eq-keyword :A :A)))

(test find-elems-test
  "find elements in list"
  (is (equal nil  (find-lxml-elems '() 'x)))
  (is (equal nil  (find-lxml-elems '(a b c) 'x )))
  (is (equal '((b c))  (find-lxml-elems '(b c) 'b )))
  (is (equal '((b c d))  (find-lxml-elems '(b c d) 'b )))
  (is (equal '((b c c))  (find-lxml-elems '(b c c) 'b )))
  (is (equal 'nil  (find-lxml-elems '((b) c) 'b ))) ; not valid -first elem is nested but shouldnt
  (is (equal 'nil  (find-lxml-elems '(((b)) c d) 'b ))) ; not valid - first elem is nested but shouldnt
  (is (equal 'nil  (find-lxml-elems '(a (b) c) 'b ))) ; single nested elems are not valid elems
  (is (equal 'nil  (find-lxml-elems '(a ((b)) c) 'b ))) ; single nested elems are not valid elems
  (is (equal '((b c))  (find-lxml-elems '(a (b c) d) 'b )))
  (is (equal 'nil  (find-lxml-elems '(a ((b) c) d) 'b ))) ; single nested elems are not valid elems
  (is (equal '(((b b) c))  (find-lxml-elems '(a ((b b) c) d) 'b )))
  (is (equal 'nil  (find-lxml-elems '(a ((b) c (d)) e) 'b ))) ; single nested elems are not valid elems
  (is (equal '((b c) (b d) (b e f))  (find-lxml-elems '(a (b c) (b d) (b e f)) 'b )))
  (is (equal '((b c) (b d)) (find-lxml-elems '(a (b c) (b d) ((b) e f)) 'b )))) ; single nested elems are not valid elems

(test find-nested-elems-test
  (is (equal '(()) (find-lxml-nested-elems '(()) '() )))
  (is (equal '(()) (find-lxml-nested-elems '(()) '(a) )))
  (is (equal '(()) (find-lxml-nested-elems '((a)) '() )))
  (is (equal '((a)) (find-lxml-nested-elems '((a)) '(a) )))
  (is (equal '((b c)) (find-lxml-nested-elems '((a b c)) '(b) )))
  (is (equal '((e f) (e f)) (find-lxml-nested-elems '((a b c d e f) (a (b c d e f))) '(b c d e) )))
  (is (equal '((e f) (e)) (find-lxml-nested-elems '((a b c d e f) (a (b c (d e) f))) '(b c d e) ))))

