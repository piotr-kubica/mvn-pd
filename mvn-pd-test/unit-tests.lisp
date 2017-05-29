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
  (is (equal nil  (find-lxml-elems '() 'x #'equal)))
  (is (equal nil  (find-lxml-elems '(a b c) 'x #'equal)))
  (is (equal '((c))  (find-lxml-elems '(b c) 'b #'equal)))
  (is (equal '((c d))  (find-lxml-elems '(b c d) 'b #'equal)))
  (is (equal '((c c))  (find-lxml-elems '(b c c) 'b #'equal)))
  (is (equal '((c))  (find-lxml-elems '((b) c) 'b #'equal)))
  (is (equal '((c d))  (find-lxml-elems '((b) c d) 'b #'equal)))
  (is (equal 'nil  (find-lxml-elems '(((b)) c d) 'b #'equal)))
  (is (equal 'nil  (find-lxml-elems '(a (b) c) 'b #'equal)))
  (is (equal 'nil  (find-lxml-elems '(a ((b)) c) 'b #'equal)))
  (is (equal '((c))  (find-lxml-elems '(a (b c) d) 'b #'equal)))
  (is (equal '((c))  (find-lxml-elems '(a ((b) c) d) 'b #'equal)))
  (is (equal '((c (d)))  (find-lxml-elems '(a ((b) c (d)) e) 'b #'equal)))
  (is (equal '((c) (d) (e f))  (find-lxml-elems '(a (b c) (b d) ((b) e f)) 'b #'equal)))
  (is (equal '((c) (d) (e f))  (find-lxml-elems '(a (b c) (b d) ((b) e f)) 'b #'equal)))
  (is (equal '((c d))  (find-lxml-elems '(a (e (f (b c d)))) 'b #'equal))))


      
;; (find-lxml-elems '(a (b c) (d e) (b b f g) h (i j (b l) (m n))) 'b #'equal) 
  ;; ((C) (B F G) (L))
;; (find-lxml-elems '(b c) 'b #'equal)
;; ((C))
;; MVN-PD-TEST> (find-lxml-elems '((b) b c) 'b #'equal)
;; (NIL (C))
;; MVN-PD-TEST> (find-lxml-elems '(a (b c) c) 'b #'equal)
;; ((C))
;; MVN-PD-TEST> (find-lxml-elems '(a ((b) c) c) 'b #'equal)
;; ((C))
;; MVN-PD-TEST> (find-lxml-elems '(a ((b) c) d) 'b #'equal)
;; ((C))
;; MVN-PD-TEST> (find-lxml-elems '(a (((b)) c) d) 'b #'equal)
;; NIL
;; MVN-PD-TEST>

;; (find-lxml-elems (find-lxml-elems '(a (b c) (d e) (b b f g) h (i j (b l) (m n))) 'b #'equal) 'b #'equal)
;; ((F G))
;; MVN-PD-TEST> 
