;;;; mvn-pd.lisp

;; https://maven.apache.org/guides/introduction/introduction-to-the-pom.html#Project_Inheritance
;; Project aggregation

(in-package :mvn-pd)

(defun str2keyword (name)
  (intern name :keyword))

(defun eq-keyword (keyword1 keyword2)
  ;; check equalisy but ignore keyword case-sensitivity
  (eq keyword1 keyword2))

(defun find-lxml-elems (lst elem eqfun)
  (let ((res (list)))
    (labels ((notemptylst (x)
	       (and (not (null x)) (listp x)))
	     (eq-to-elem (x)
	       (funcall eqfun x elem))
	     (find-el (lst)
	       (mapc (lambda (e)
		       (when (notemptylst e)
			 (if (and
					; equal to element
			      (or (eq-to-elem (car e))
					; element with attributes is a list
				  (and (notemptylst (car e))
					; equal to first of list
				       (eq-to-elem (caar e))))
					; second element (value) exisits...
			      (and (not (null (cadr e)))
					; ...and is not a list
				   ;; TODO: and is not a symbol
				   (not (listp (cadr e)))))
			     
					; add to result collection
			     (push (cadr e) res)
					; otherwise search deeper
			     (find-el e))))
		     lst)))
      (when (notemptylst lst)
	(find-el lst)))
    res))

;; TODO handle nested elements like elem1 > elem2 > elem3.
;; Passed as list
;; TODO check performance against xpath
(defun find-lxml-nested-elems (lst elems)
  (let ((res (list)))
    (labels ((notemptylst (x)
	       (and (not (null x)) (listp x)))
	     ;; first list elem is always an xml-element
	     (is-elem (e x)
	       (or (equal (car e) x)
		   ;; element with attributes is a list
		   (and (notemptylst (car e))
			(equal (caar e) x))))
	     (is-content (e)
	       ;; replace with stringp ?
	       (not (or
		     (null (cadr e))
		     (listp (cadr e))
		     (symbolp (cadr e)))))
	     (find-el (lst x xrest)
	       (let ((e (car lst)))
		       (if (is-elem e x)
			   (cond ((notemptylst xrest)
				  (find-el (cdr e) (car xrest) (cdr xrest)))
				 ((is-content e)
				  (push (cadr e) res)))
			   (when (notemptylst e)
			     (find-el (cdr e) x xrest))))))
	     (when (notemptylst lst)
	       (find-el lst (car elems) (cdr elems))))
      res))

;; TODO find-lxml-elems-text
;;  should match elements by element name and element value
;; run find-lxml-elems then filter by value


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

