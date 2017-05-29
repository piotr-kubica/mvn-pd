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
  (is (equal '((c))  (find-lxml-elems '(b c) 'b )))
  (is (equal '((c d))  (find-lxml-elems '(b c d) 'b )))
  (is (equal '((c c))  (find-lxml-elems '(b c c) 'b )))
  (is (equal 'nil  (find-lxml-elems '((b) c) 'b ))) ; first elem is nested but shouldnt
  (is (equal 'nil  (find-lxml-elems '((b) c d) 'b ))) ; first elem is nested but shouldnt
  (is (equal 'nil  (find-lxml-elems '(((b)) c d) 'b ))) ; first elem is nested but shouldnt
  (is (equal 'nil  (find-lxml-elems '(a (b) c) 'b )))
  (is (equal 'nil  (find-lxml-elems '(a ((b)) c) 'b )))
  (is (equal '((c))  (find-lxml-elems '(a (b c) d) 'b )))
  (is (equal '((c))  (find-lxml-elems '(a ((b) c) d) 'b )))
  (is (equal '((c (d)))  (find-lxml-elems '(a ((b) c (d)) e) 'b )))
  (is (equal '((c) (d) (e f))  (find-lxml-elems '(a (b c) (b d) ((b) e f)) 'b )))
  (is (equal '((c) (d) (e f))  (find-lxml-elems '(a (b c) (b d) ((b) e f)) 'b )))
  (is (equal '((c d))  (find-lxml-elems '(a (e (f (b c d)))) 'b )))
  (is (equal '((c d))  (find-lxml-elems '(a (e (f (b c d)))) 'b )))
  (is (equal '((d)) (find-lxml-elems (find-lxml-elems '(a (b c d)) 'b ) 'c )))
  (is (equal '((c (c))) (find-lxml-elems (find-lxml-elems '(a (b c c (c))) 'b ) 'c ))))


;; TODO nested elems real data test
;; <modules> <module></module> <module></module> </modules>
