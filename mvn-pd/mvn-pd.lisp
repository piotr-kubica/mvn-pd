;;;; mvn-pd.lisp


(in-package :mvn-pd)

(defun remove-white-char (s)
  (let ((chars (coerce s 'list))
        (white-chars '(#\Space #\Newline #\Backspace #\Tab 
                       #\Linefeed #\Page #\Return #\Rubout)))
    (coerce (mapcan 
             (lambda (c) (and (not (find c white-chars)) 
                              (list c))) 
             chars) 
            'string)))

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
      (car (remove-if-not #'stringp (children elem)))))

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


(defun dependency-name (art-id &optional gr-id-p)
  (when art-id
    (concatenate 'string
		 (if gr-id-p
		     (concatenate 'string gr-id-p "-"))
		 art-id)))
		     

(defun module-name (lxml)
  "returns artifactId of pom module"
  (let* ((art-id-el (find-nested-el lxml '(:|project| :|artifactId|) ))
	 (art-id-name (mapcan #'value? art-id-el)))
    (when art-id-name
      (dependency-name (car art-id-name)))))


(defun dependencies (lxml)
  (find-nested-el lxml '(:|project| :|dependencies| :|dependency|)))


(defun module? (lxml)
  "is moudle? = has parent section"
  (find-nested-el lxml '(:|project| :|parent|)))


(defun modules (lxml)
  "returns dependent raw module names from parent pom as list"
  (let* ((mods (find-nested-el lxml '(:|project| :|modules| :|module|)))
         (mod-names (mapcan 
                     (lambda (m) (and 
                                  (value m) 
                                  (list (value m)))) 
                     mods)))
    (mapcar (lambda (m) (file-namestring (pathname m)))
            mod-names)))


(defun module-dependency-list (lxml)
  "returns all dependencies for this module as assoc list"
  `(,(module-name lxml) . ,(dependencies lxml)))


(defun project-module-list (lxml)
  ;; here module name is project parent module name
  `(,(module-name lxml) . ,(modules lxml)))


(defun project-module-dependencies (lxml dependency-list)
  "dependency-list contains structure of already computed dependencies"

  ;; remove :|dependency| and :|module|

  ;; TODO
  ;; combine...
  ;; project-module-dependencies ...with...
  ;; module-dependency-list
    
  "filters module dependencies by parent modules"
  ;; mapcan (find elem list)

  )

  
(defun to-dot-format (proj-dependencies)
  ;; TODO
  )

(defun project-dependencies-dot (pom-file-list)
;; process each file in list to output dependency-list and combine them all
;; finaly save to dot file
    )
 
