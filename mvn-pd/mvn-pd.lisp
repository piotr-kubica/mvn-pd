;;;; mvn-pd.lisp

;; https://maven.apache.org/guides/introduction/introduction-to-the-pom.html#Project_Inheritance
;; Project aggregation

(in-package :mvn-pd)

(defun str2keyword (name)
  (intern name :keyword))

(defun eq-keyword (keyword1 keyword2)
  ;; check equalisy but ignore keyword case-sensitivity
  (eq keyword1 keyword2))

;; O(n^2)
(defun find-lxml-elems (lst elem eqfun)
  (when (or (null lst)
	    (null elem)
	    (not (functionp eqfun)))
    nil)
  (let ((res (list)))
    (labels ((notemptylst (x)
	       (and (not (null x)) (listp x)))
	     (is-eq (x)
	       (funcall eqfun elem x))
	     (find-el (lst)
	       (if (notemptylst lst) ; check for end of list
		   (let ((e (car lst)))
		     ;; (break "lst: ~a" lst)
		     ;; (break "nempty lst: ~a" (notemptylst lst) )
		     ;; (break "e: ~a" e)
		     (if (notemptylst e)
			; first elem is a list, means nested elem with content or attrg
			 (progn
			   (find-el (cdr lst))
			   (if (or  (is-eq (car e))
				    (and (notemptylst (car e)) ; elem with attr
					 (is-eq (caar e))))
			       (push (cdr e) res)
			       (find-el (cdr e)))) ; continue search in nested list
			 (progn
			   ;; (break "e: ~a" e)
			   ;; (break "is-eq e: ~a" (is-eq e))
			   ;; (break "cdr lst: ~a" (cdr lst))
				(if (is-eq e) ; not a list, means element without attr and content
				    (push (cdr lst) res)
				    (find-el (cdr lst))))))))) ; continue search
      (find-el lst))
    (when (not (equal res '(nil)))
	res))) 


;;  Failure Details:
;;  --------------------------------
;;  FIND-ELEMS-TEST [find elements in list]: 
;; (FIND-LXML-ELEMS '((B) C) 'B #'EQUAL)
;;  evaluated to 
;; NIL
;;  which is not 
;; EQUAL
;;  to 
;; ((C))
;;  --------------------------------
;;  FIND-ELEMS-TEST [find elements in list]: 
;; (FIND-LXML-ELEMS '((B) C D) 'B #'EQUAL)
;;  evaluated to 
;; NIL
;;  which is not 
;; EQUAL
;;  to 
;; ((C D))




;; TODO use xml event-based parser in next version ?

;; TODO readXmlToLxml

(defun build-pom-path (modulepaths)
  (mapcar (lambda (p)
	    (make-pathname
	     :directory `(:relative ,p)
	     :name "pom"
	     :type "xml"))
	  modulepaths))


;; TODO test
;; (to-path '("../temp" "../a"))

;; obtain-module-name
;; (pathname-directory (parse-namestring "/foo/bar/baz.lisp"))

