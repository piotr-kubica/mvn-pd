;;;; mvn-pd.lisp

(in-package :mvn-pd)

(defun str2keyword (name)
  (intern name :keyword))

(defun eq-keyword (keyword1 keyword2)
  ;; check equalisty but ignore keyword case-sensitivity
  (eq keyword1 keyword2))

(defun find-elems (lst elem eqfun)
  ;; finds all elem's in list lst recursively by using compare function eqfun
  (when (car lst)
    (let ((res '()))
      (labels ((notemptylst (x)
		 (and (not (equal x '())) (listp x)))
	       (findel (lst)
		 (mapcar lst (lambda (x)
			       (when (notemptylst x)
				 (if (funcall eqfun (car x) elem)
				     (push (cadr x) res)
				     (findel (cdr x))))))))
	(findel (cdr lst)))
      res)))

;; (print (find-elems '(1 2 (3 4) 4 (5 (4))) 4 #'equal))
;; (print (s-xml:parse-xml-file "example.xml"))
;; (print "testing xml element equality")
;; (defparameter *parsed-content* (s-xml:parse-xml-file "example.xml"))


;; important !!
;; (setf s-xml:*IGNORE-NAMESPACES* t)

;; (print (eq-keyword-str (car *parsed-content*) "note" ))


;; (print "testing finding elements in xml")
;; (print (find-elems *parsed-content* "from" #'eq-keyword-str))


;; (setf *parsed-content* (s-xml:parse-xml-file "./test-graphwalk/AgisParent/pom.xml"))
;; (print (find-elems *parsed-content* "module" #'eq-keyword-str))




;; [algorithm]
;; read parent pom.xml
;; collect module elements http://www.cliki.net/SPLIT-SEQUENCE
;; create relative paths
;; read each module file from filepath
;;   collect module name
;;   create asoc list (module . (dependent modules))
;; asoc list to graphviz file

;; TODO (group-by (last-phrase module-name))
;; this means a double assoc list
;;   #1  by (last-phrase module-name)
;;     #2  by module-name


;; check if file exists
;; file-exists-p, that can both test whether a directory exists and tell you whether a given name is the name of a file or directory.
;; or
;; (probe-file "/etc/passwd")

;; give directory
;; (directory-namestring #p"/foo/bar/baz.txt") ==> "/foo/bar/"
;; (pathname "/foo/bar/baz.txt") ==> #p"/foo/bar/baz.txt"
;; (pathname-directory (pathname "/foo/bar/baz.txt")) ==> (:ABSOLUTE "foo" "bar")
;; (pathname-name (pathname "/foo/bar/baz.txt"))      ==> "baz"
;; (pathname-type (pathname "/foo/bar/baz.txt"))      ==> "txt"
;; (namestring #p"/foo/bar/baz.txt")           ==> "/foo/bar/baz.txt"
;; (directory-namestring #p"/foo/bar/baz.txt") ==> "/foo/bar/"
;; (file-namestring #p"/foo/bar/baz.txt")      ==> "baz.txt"
