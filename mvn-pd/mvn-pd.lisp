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

	     (is-eq (x)
	       (funcall eqfun elem x))
	     
	     ;; (is-elem (e)
	     ;;   (or (is-eq e) ; first list elem is always an xml-element
	     ;; 	   (and (notemptylst e) ; element with attributes is a list
	     ;; 		(is-eq (car e)))))
	     
	     (find-el (lst)
	       (if (notemptylst lst) ; check for end of list
		   (let ((e (car lst)))
		     (if (notemptylst e)
					; first elem is a list, means nested elem with content or attr
			 (if (notemptylst (car e))
					; nested elem with attr
			     (if (is-eq (caar e))
				 (progn
				   (push (cdr e) res)
				   (find-el (cdr lst)))
				 (progn
				   (find-el (cdr e))
				   (find-el (cdr lst))))
					; nested elem with content
			     (if (is-eq (car e))
				 (progn
				   (push (cdr e) res)
				   (find-el (cdr lst)))
				 (progn
				   (find-el (cdr e))
				   (find-el (cdr lst)))))
			 (if (is-eq e) ; not a list, means element without attr and content
			     (push (cdr lst) res)
			     (find-el (cdr lst))))))))
      (find-el lst))
    res))


;; TODO use xml event-based parser in next version

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

