;;;; mvn-pd.lisp


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
  "returns true if element contains children including text nodes"
  (and (listp elem)
       (> (length elem) 1)))

(defun children (elem)
  (and (children? elem)
       (cdr elem)))

(defun children-elem? (elem)
  "returns true if element contains children excluding text nodes"
  (and (children? elem)
       (remove-if #'stringp (children elem))))

(defun children-elem (elem)
  "returns true if element contains children excluding text nodes"
  (if (children? elem)
      (remove-if #'stringp (children elem))))

(defun name (elem)
  (cond ((attr? elem) (caar elem))
	((listp elem) (car elem))
	(t elem)))

;;; O(2^n) runtime - use xml event-based parser in next version ?
(defun find-lxml-el (lxml el &key (eqfun #'equal) (max-nest (- 1)))
  "finds all elements that match el and returns as list"
  (let ((res))
    (labels ((valid-nest? (level)
	       (or (<= max-nest (- 1)) 
		   (<= 0 level max-nest)))
	     (is-eq (x)
	       (funcall eqfun el x))
	     (find-el (elem nest-level)
	       (when (valid-nest? nest-level)
		 (if (is-eq (name elem))
		     (push elem res)
		     (if (children? elem)
			 (mapc (lambda (e)
				 (find-el e (1+ nest-level)))
			       (children elem)))))))
      (when (and lxml el eqfun)
	(find-el lxml 0)
	res))))

;; TODO test
(defun find-lxml-el-children (lxml el &key (child-f #'children-elem)
					(eqfun #'equal)
					(max-nest (- 1)))
  "find elements with child nodes at given nesting level"
  (when (and lxml el eqfun max-nest)
    (remove-if #'null 
	       (mapcar child-f
		       (find-lxml-el lxml el
				     :eqfun eqfun
				     :max-nest max-nest)))))


;; TODO fix
(defun find-nested-el (lxml elems &key (eqfun #'equal))
  "looks for directly nested elements and returns them as list"
  (labels ((find-offspring (lxml el)
	     (car (find-lxml-el-with-children lxml el
					      :child-p #'children?
					      :child-f #'children-elem)))
	   (find-children (lxml el)
	     (car (find-lxml-el-with-children lxml el
					      :child-f #'children-elem
					      :max-nest 1)))
	   (find-elems (input elst)
	     (format t "~& input: ~& ~s" input)
	     (format t "~& elst: ~& ~s" elst)
	     (if elst
	       (find-elems
		(find-children input (car elst))
		(cdr elst))
	       input)))
    (when (and lxml elems eqfun)
      ;; first time search - look for elems of any nest level
      (find-elems (find-offspring lxml (car elems))  (cdr elems)))))
      
	       
			      

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
