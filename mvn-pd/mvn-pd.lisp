;;;; mvn-pd.lisp

;; https://maven.apache.org/guides/introduction/introduction-to-the-pom.html#Project_Inheritance
;; Project aggregation

(in-package :mvn-pd)

(defun keyword->str (kw)
  "converts keyword to string"
  (when (symbolp kw)
    (symbol-name kw)))

(defun attr? (elem)
  (and (listp elem)
       (listp (car elem))))

(defun attr (elem)
  (and (attr? elem)
       (cdr (car elem))))

(defun children? (elem)
  (and (listp elem)
       (> (length elem) 1)))

(defun children (elem)
  (and (children? elem)
       (cdr elem)))

(defun name (elem)
  (cond ((attr? elem) (caar elem))
	((listp elem) (car elem))
	(t elem)))

;;; O(2^n) runtime - use xml event-based parser in next version
(defun find-lxml-el (lxml el &optional (eqfun #'equal))
  (let ((res))
    (labels ((is-eq (x)
	       (funcall eqfun el x))
	     (find-el (elem)
	       (if (is-eq (name elem))
		   (push elem res)
		   (if (children? elem)
		       (mapc #'find-el (children elem))))))
      (when (and lxml el eqfun)
	(find-el lxml)
	res))))

;; TODO use find-lxml-el
;; (defun find-lxml-nested-elems (lxml elst &optional (eqfun #'equal))
;;   "looks for netsted elements, each further element in elst is nested in previous"
;;   (labels ((find-elst (r el)
;; 	     (if (cdr el)
;; 		 (car (find-lxml-elems (find-elst r (cdr el)) (car el) eqfun))
;; 		 (car (find-lxml-elems r (car el) eqfun))))
;; 	   (find-lxml (el)
;; 	     (mapcar (lambda (r)
;; 		       (find-elst r el))
;; 		     lxml)))
;;     (find-lxml (reverse elst))))


;; TODO
;; (defun find-module-name (lxml)		
;; "returns artifactId of pom module"

	

;; TODO find dependencies
;; optionally filters by predicate that returns matched artifactId's
;; (defun find-module-deps (lxml &optional filter-p)
;;   )

;; build-dep as assoc list (module-name . dependent-module-list)
;; build-dep-from-file
;; build-deps-from-files

;; TODO
;; (defun to-dot-format (module-deps)
;;   )
