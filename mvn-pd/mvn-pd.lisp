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
  "returns true if element contains children including value? nodes"
  (and (listp elem)
       (> (length elem) 1)))

(defun children (elem)
  (and (children? elem)
       (cdr elem)))

(defun children-elem? (elem)
  "returns true if element contains children excluding value? nodes"
  (and (children? elem)
       (remove-if #'stringp (children elem))))

(defun children-elem (elem)
  "returns true if element contains children excluding value? nodes"
  (if (children? elem)
      (remove-if #'stringp (children elem))))

(defun name (elem)
  (cond ((attr? elem) (caar elem))
	((listp elem) (car elem))
	(t elem)))

(defun value? (elem)
  "returns element containing element value excluding other nodes"
  (and (children? elem)
       (remove-if-not #'stringp (children elem))))

(defun value (elem)
  ;; TODO test
  "returns element containing element value excluding other nodes"
  (if (children? elem)
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
	     "first seach: descendands of any level"
	     (find-lxml-el-children lxml el :max-nest -1))
	   (find-children (lxml el)
	     "second to second-to-last search: only direct children"
	     (find-lxml-el-children lxml el :max-nest 1))
	   (last-searched? (elst)
	     "is this the last searched elem?"
	     (null (cdr elst)))
	   (find-el (lxml el)
	     "when last search then return full elems"
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


(defun dependency-name (art-id &optional gr-id)
  (when art-id
    (concatenate 'string
		 (if gr-id
		     (concatenate 'string gr-id "-"))
		 art-id)))
		     

(defun find-module-name (lxml &key (gr-id nil))
  "returns groupId-artifactId of pom module"
  (let* ((art-id-el (find-nested-el lxml '(:|project| :|artifactId|) ))
	 (gr-id-el  (find-nested-el lxml '(:|project| :|groupId|) ))
	 (art-id-name (mapcan #'value? art-id-el))
	 (gr-id-name  (mapcan #'value? gr-id-el)))
    (when (and art-id-name
	       (if gr-id
		   gr-id-name
		   t))
      (dependency-name (car art-id-name)
		       (if gr-id
			   (car gr-id-name))))))


(defun find-dependencies (lxml)
  (find-nested-el lxml '(:|project| :|dependencies| :|dependency|)))


(defun filter-dependencies (lxml p)
  ;; TODO test
  "filters elements from dependency list by predicate"
  )
				   
  "TODO"
  ;; optionally filters by predicate that returns matched artifactId's
  ;; (defun find-module-deps (lxml &optional filter-p)
  ;;   )
;;  ))


;; (defun match-value-elem-text (lxml regex)
;;   "TODO"
;;   ;; filters elements with value? - function value?
;;   ;; filters values (see value?)
;;   )


;; (defun module-dependencies (lxml &key (gr-id nil))
;;   "TODO"
;;   ;; build-dep as assoc list (module-name . dependent-module-list)
;;   ;; build-dep-from-file
;;   ;; build-deps-from-files

;;   ) 



;; TODO
;; (defun to-dot-format (module-deps)
;;   )
