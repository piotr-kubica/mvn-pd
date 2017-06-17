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

(defun text (elem)
  "returns element containing element value excluding other nodes"
  (and (children? elem)
       (remove-if-not #'stringp (children elem))))


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


(defun find-lxml-el-children (lxml el &key (child-f #'children-elem)
					(eqfun #'equal)
					(max-nest (- 1)))
  "find elements child nodes and returns them as list"
  (when (and lxml el eqfun max-nest)
    (remove-duplicates
     (mapcan child-f
	     (find-lxml-el lxml el
			   :eqfun eqfun
			   :max-nest max-nest)))))


(defun find-nested-el (lxml elems &key (eqfun #'equal))
  "looks for directly nested elements and returns them as list"
  (labels ((find-offspring (lxml el)
	     "first seach: decendands of any level"
	     (find-lxml-el-children lxml el :max-nest -1))
	   (find-children (lxml el)
	     "second to second-to-last search: only direct children"
	     (find-lxml-el-children lxml el :max-nest 1))
	   (last-searched? (elst)
	     "is last searched elem"
	     (null (cdr elst)))
	   (find-el (lxml el)
	     "when last search then search for full elems"
	     (find-lxml-el lxml el :max-nest 1))
	   (find-elems (ch-lst elst)
	     (if elst
		 (find-elems
		  (mapcan (lambda (cel)
			    (if (last-searched? elst)
				(find-el cel (car elst))
				(find-children cel (car elst))))
			  ch-lst)
		  (cdr elst))
		 ch-lst)))
    (when (and lxml elems eqfun)
      (find-elems (find-offspring lxml (car elems))  (cdr elems)))))


(defun find-module-name (lxml &key (group-id-p t))
  "returns groupId-artifactId of pom module"
  (let* ((art-id-el (find-nested-el lxml '(:|project| :|artifactId|) ))
	 (gr-id-el  (find-nested-el lxml '(:|project| :|groupId|) ))
	 (art-id-name (mapcan #'mvn-pd::text art-id-el))
	 (gr-id-name  (mapcan #'mvn-pd::text gr-id-el)))
    (when (and art-id-name
	       (if group-id-p
		   gr-id-name
		   t))
      (concatenate 'string
		   (if group-id-p
		       (concatenate 'string (car gr-id-name) "-"))
		   (car art-id-name)))))
	


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
