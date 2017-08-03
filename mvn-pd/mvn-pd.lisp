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
	
	     
(defun module-name (lxml &optional default-name)
  "returns artifactId of pom module"
  (let* ((art-id-el (find-nested-el lxml '(:|project| :|artifactId|) ))
	 (art-id-name (mapcan #'value? art-id-el)))
    (if art-id-name
      (dependency-name (car art-id-name))
      default-name)))


(defun dependencies (lxml)
  (mapcar #'cdr
          (find-nested-el lxml '(:|project| :|dependencies| :|dependency|))))


(defun parent-module? (lxml)
  "is parent module? = has parent section"
  (find-nested-el lxml '(:|project| :|modules| :|module|)))


(defun child-module? (lxml)
  "is parent module? = has parent section"
  (and
   (find-nested-el lxml '(:|project| :|parent|))
   (module-name lxml)))


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
  (cons (module-name lxml) (dependencies lxml)))


(defun mod-deps-containing-artifacts (module artifact-list)
  (labels ((artifactid-by-name? (el)
             (equalp (keyword->str (name el)) "artifactId"))
           (artifact-value (dep)
             (value (find-if #'artifactid-by-name? dep)))
           (module-dependend? (dep)
             (member (artifact-value dep) artifact-list :test #'equal)))
    ;; reverse because concatinating to head
    (reverse (reduce (lambda (res dep)
                       (if (module-dependend? dep)
                           (cons dep res)
                           res)) 
                     (cdr module)
                     :initial-value (list (car module))))))


(defun project-module-list (lxml)
  ;; here module name is project parent module name
  (cons (module-name lxml) (modules lxml)))


;; parse-xml-fun should return a stream, ex. 
;; s-xml:parse-xml-file 
;; or 
;; s-xml:parse-xml-string
;; 
;; read more: https://common-lisp.net/project/s-xml/S-XML.html
;; dependency-list should be list of strings or filenames
(defun read-dependencies (dependendecy-list parse-xml-fun)
  (let* ((lxml-list (mapcar 
                     (lambda (d)
                       (funcall parse-xml-fun d)) 
                     dependendecy-list))
         (parent-lxml (find-if 
                       #'parent-module? 
                       lxml-list))
         (module-lxml-list (mapcan 
                            (lambda (d) 
                              (and (child-module? d) (list d) ))
                            lxml-list)))
    (and module-lxml-list
         (cons parent-lxml module-lxml-list))))


(defun project-dependencies (dependendecy-list parse-xml-fun)
  (let ((lxml-par-and-mods (read-dependencies dependendecy-list parse-xml-fun)))
    (when lxml-par-and-mods
      (let ((par-name (module-name (car lxml-par-and-mods) "Project"))
            (deps (mapcan 
                   (lambda (d) (list (module-dependency-list d)))
                   (cdr  lxml-par-and-mods))))
        (cons par-name deps)))))


(defun project-module-dependencies (dependendecy-list parse-xml-fun)
  (let ((proj-dep (project-dependencies dependendecy-list parse-xml-fun)))
    (when proj-dep
      (let* ((project-name (car proj-dep))
             (module-deps (cdr proj-dep))
             (module-names (mapcar #'car module-deps)))
        (reverse 
         (reduce 
          (lambda (res mdep)
            (cons (mod-deps-containing-artifacts mdep module-names) res))
          module-deps
          :initial-value (list project-name)))))))
 
  
(defun to-dot-format (proj-dependencies)
  ;; TODO
  ;;  digraph { label="parent-artifact";
  ;;      ModuleA -> ModuleB; 
  ;;      ModuleA -> ModuleC; 
  ;;      ModuleB -> ModuleC;
  ;;  }

;; TODO consider (when no dependency to other Module, like ModuleB)
;; digraph { label="parent-artifact";
;;         ModuleA -> ModuleC; 
;;         ModuleB ;
;;   }
  )

(defun project-dependencies-dot (pom-file-list)
;; process each file in list to output dependency-list and combine them all
;; finaly save to dot file
    )
 
