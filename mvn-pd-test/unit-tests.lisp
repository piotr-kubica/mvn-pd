;;;; unit-tests.lisp

(in-package :mvn-pd-test)

;; equallst
;; cnt will add 1 for each found elem lsa
;; discnt will remove 1 for each found elem in lsb
;; if all elems after check have count 0 then lsa and 
;; lsb are equal (in terms of containins same elements
;; including their count but ignoring order
(defun equallst (lsa lsb)
  "checks lists equality ignoring order but checks element occurence"
  (let ((als (list)))
    ;; possible improvement: use hash instead of asoclist
    (labels ((cnt (ls)
	       (mapc (lambda (e)
		       (if (assoc e als)
			   (incf (cdr (assoc e als)))
			   (setf als (acons e 1 als))))
		     ls))
	     (discnt (ls)
	       ;; possible improvement: use loop named and escape early when element not found
	       (mapc (lambda (e)
		       (if (assoc e als)
			   (decf (cdr (assoc e als)))
			   (setf als (acons e -1 als))))
		     ls))
	     (check (ls)
	       (if (null ls)
		   t
		   (if (not (equal (cdar ls) 0))
		       nil
		       (check (cdr ls))))))
      (cnt lsa)
      (discnt lsb)
      (check als))))

(test equallst-test
  (is-true (equallst '() '()))
  (is-true (equallst '(a) '(a)))
  (is-true (equallst '(a a) '(a a)))
  (is-true (equallst '(a a b) '(a b a)))
  (is-false (equallst '(a a b) '(a b b)))
  (is-false (equallst '(a) '(b)))
  (is-false (equallst '(a) '())))


(test str2keyword-test
  "str2keyword coerce string to keyword"
  (is (eq :|test| (str2keyword "test")))
  (is (eq :|Test| (str2keyword "Test")))
  (is (eq ':|test| (str2keyword "test"))))

(test eq-keyword-test
  "comapre keywords ignoring case"
  (is-true (eq-keyword :a :a))
  (is-false (eq-keyword :b :a))
  (is-true (eq-keyword :A :a))
  (is-true (eq-keyword :A :A)))

(test find-elems-test
  "find elements in list"
  (is (equallst '(c)   (find-lxml-elems '(a (b c)) 'b #'equal) ))
  (is (equallst nil   (find-lxml-elems '(a (b (c))) 'b #'equal) ))
  (is (equallst '(c)   (find-lxml-elems '((b c) a) 'b #'equal) ))
  (is (equallst '(d)   (find-lxml-elems '((a (b d))) 'b #'equal)))
  (is (equallst '(c d) (find-lxml-elems '((b c) (a (b d))) 'b #'equal)))
  (is (equallst '(c)   (find-lxml-elems '(((b) c)) 'b #'equal)))
  (is (equallst '(c)   (find-lxml-elems '(((b d f) c)) 'b #'equal)))
  (is (equallst '(c)   (find-lxml-elems '(a b (b c) d)) 'b #'equal))))
  
