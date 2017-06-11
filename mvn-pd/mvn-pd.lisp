;;;; mvn-pd.lisp

;; https://maven.apache.org/guides/introduction/introduction-to-the-pom.html#Project_Inheritance
;; Project aggregation

(in-package :mvn-pd)

(defun keyword->str (keyw)
  "converts string to keyword"
  (when (symbolp keyw)
    (symbol-name keyw)))

;; ((:|project| :|xmlns| "http://maven.apache.org/POM/4.0.0")
;;  ((:|modelVersion| :|atr| "a" :|btr| "b") "4.0.0") (:|groupId| "com.example")
;;  (:|artifactId| "examplePom")
;;  (:|version| :|va| ((:|vb| :|atr| "a") "text")
;;   ((:|vb| :|atr| "a") "text " :|vc|))
;;  (:|name| "Maven Pom Example")
;;  (:|dependencies|
;;   (:|dependency| (:|groupId| "junit") (:|artifactId| "junit")
;;    (:|version| "4.8") (:|scope| "test"))))

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

;;; rewrite: if first elem of list -> symbol
;;; if not first and not-symbols follows then attribute with value
;;; else empty elem (symbol after symbol)
(defun find-lxml-el (lxml el &optional (eqfun #'equal))
  (let ((res))
    (labels ((is-eq (x)
	       (funcall eqfun el x))
	     (find-el (elem)
	       (if (is-eq (name elem))
		   (push elem res)
		   (if (children? elem)
		       (mapc #'findel children)))))
      (when (and lxml el eqfun)
	(find-el lxml)
	res))))



;;; O(2^n) runtime - use xml event-based parser in next version
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


(defun find-lxml-nested-elems (lxml elst &optional (eqfun #'equal))
  "looks for netsted elements, each further element in elst is nested in previous"
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


(defun find-module-name (lxml)
  "returns artifactId of pom module"
  (when 
      (let ((elems-nested '(project artifactId)))
					; TODO add equalp with keyword->str
	;; MVN-PD-TEST> (equal "a" "A")
	;; NIL
	;; MVN-PD-TEST> (equalp "a" "A")
	;; T
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
