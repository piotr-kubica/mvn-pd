;;;; mvn-pd.lisp

;; https://maven.apache.org/guides/introduction/introduction-to-the-pom.html#Project_Inheritance
;; Project aggregation

(in-package :mvn-pd)

(defun str2keyword (name)
  (intern name :keyword))

(defun eq-keyword (keyword1 keyword2)
  ;; check equalisy but ignore keyword case-sensitivity
  (eq keyword1 keyword2))

;; O(2^n) runtime - use xml event-based parser in next version
(defun find-lxml-elems (lxml elem &optional (eqfun #'equal))
  (when (and lxml elem
   	     (and (listp lxml)
   		  (not (listp (car lxml)))))
    (let ((res (list)))
      (labels ((notemptylst (x)
		 (and (not (null x)) (listp x)))
	       (is-eq (x)
		 (funcall eqfun elem x))
	       (find-el (lst)
		 (if (notemptylst lst) ; check for end of list
		     (let ((e (car lst)))
		       (if (notemptylst e)
					; first elem is a list, means
					; nested elem with content or attrg
			   (progn
			     (find-el (cdr lst))
			     (if (or (and (is-eq (car e))
					  (not (null (cdr e)))) ; single nested elems are not valid elems
				     (and (notemptylst (car e)) ; elem with attr
					  (is-eq (caar e))
					  (not (null (cdar e))))) ; single nested elems are not valid elems
				 (push e res)
				 (find-el (cdr e)))) ; continue search nested elem list
			   (if (is-eq e) ; not a list, means element without attr and content
			       (push lst res)
			       (find-el (cdr lst)))))))) ; continue search
	(find-el lxml))
      (when (not (equal res '(nil)))
	res))))

;; looks for netsted elements
;; each further element in elst is nested in previous
(defun find-lxml-nested-elems (lxml elst &optional (eqfun #'equal))
  (labels ((find-elst (r el)
	     (if (cdr el)
		 (car (find-lxml-elems (find-elst r (cdr el)) (car el) eqfun))
		 (car (find-lxml-elems r (car el) eqfun))))
	   (find-lxml (el)
	     (mapcar (lambda (r)
		       (find-elst r el))
		     lxml)))
    (find-lxml (reverse elst))))


(defun get-lxml-values (lxml-search-res)
  (when lxml-search-res
    (remove-if #'null (mapcar #'cadr lxml-search-res))))


;; TODO test
;; returns artifactId of pom module
(defun find-module-name (lxml)
  (when 
      (let ((elems-nested '(project artifactId)))
	(get-lxml-values (find-lxml-nested-elems lxml elems-nested)))))
	

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
