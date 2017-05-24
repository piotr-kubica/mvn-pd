;;;; mvn-pd.lisp

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
					; equal to atom element
			      (or (eq-to-elem (car e))
					; element with attributes is a list
				  (and (notemptylst (car e))
					; equal to first of list
				       (eq-to-elem (caar e))))
					; second element (value) exisits...
			      (and (not (null (cadr e)))
					; ...and is not a list
				  (not (listp (cadr e)))))
					; add to result-collection
			     (push (cadr e) res)
					; otherwise search deeper
			     (find-el e))))
		     lst)))
      (when (notemptylst lst)
	(find-el lst)))
    res))

;; TODO find-lxml-elems-text
;;  should match elements by element name and element value

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

