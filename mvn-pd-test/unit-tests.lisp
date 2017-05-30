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
  (is (equal (find-lxml-elems (find-lxml-elems '(a b c d) 'b ) 'c )
	     (find-lxml-nested-elems '(a b c d) '(b c))))
  (is (equal '((d)) (find-lxml-nested-elems '(a b c d) '(b c)))))


;; (find-lxml-nested-elems '((b c d)) '(c) )
;; (((C D)))
;; MVN-PD-TEST> (find-lxml-nested-elems '(a b c d) '(b c))
;; (NIL NIL NIL NIL)
;; MVN-PD-TEST> (find-lxml-nested-elems '(a b c d) '(b))
;; (NIL NIL NIL NIL)
;; MVN-PD-TEST> (find-lxml-nested-elems '(a b c d) '(c))
;; (NIL NIL NIL NIL)
;; MVN-PD-TEST> (find-lxml-nested-elems '((a b c d)) '(c))
;; (((C D)))
;; MVN-PD-TEST> (find-lxml-nested-elems '((a b c d)) '(b c))
;; (NIL)
;; MVN-PD-TEST> (find-lxml-nested-elems '((a b c d)) '(b))
;; (((B C D)))
;; MVN-PD-TEST> (find-lxml-nested-elems '((a b c d) (a b b)) '(b))
;; (((B C D)) ((B B)))


;; ((B C D) (B B))
;; MVN-PD-TEST> (find-lxml-nested-elems '((a b c d)) '(b c))
;; (((C D)))
;; MVN-PD-TEST> (find-lxml-nested-elems '((a b c d e)) '(b c d))
;; (NIL)
;; ; compiling (DEFUN FIND-LXML-NESTED-ELEMS ...)
;; MVN-PD-TEST> (find-lxml-nested-elems '((a b c d e)) '(b c d))

;; (find-lxml-nested-elems '((a b c d e f) (a (b c d e f))) '(b c d e))
;; ((E F) (E F))

;; ((D E))
;; MVN-PD-TEST> (find-lxml-nested-elems '((a b c d e f)) '(b c d e))
;; ((E F))
;; MVN-PD-TEST> (find-lxml-nested-elems '((a b c d e f) (a b c d e f)) '(b c d e))
;; ((E F) (E F))
;; MVN-PD-TEST> 


  ;; (is (equal '((b c d))  (find-lxml-elems '(a (e (f (b c d)))) 'b )))
  ;; (is (equal '((d)) (find-lxml-elems (find-lxml-elems '(a (b c d)) 'b ) 'c )))
  ;; (is (equal '((c (c))) (find-lxml-elems (find-lxml-elems '(a (b c c (c))) 'b ) 'c )))
  ;; (is (equal '((d)) (find-lxml-elems (find-lxml-elems '(a b (c d)) 'b ) 'c )))
  ;; (is (equal '((d)) (find-lxml-elems (find-lxml-elems '(a b c d) 'b ) 'c ))))

