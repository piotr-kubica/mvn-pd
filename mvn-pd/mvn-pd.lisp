;;;; mvn-pd.lisp

(in-package :mvn-pd)

(defun str2keyword (name)
  (intern name :keyword))

(defun eq-keyword-str (keyword name)
  (eq keyword (str2keyword name)))


(print "testing eq-keyword-str")
(print (eq-keyword-str :|a| "a"))
(print (eq-keyword-str :|a| "A"))
(print (eq-keyword-str 'NS-0:|module| "module"))


(defun find-elems (lst elem eqfun)
  (let ((res '()))     
    (labels ((findel (x)
	       (cond
		 ((funcall eqfun (car x) elem)
		  (push x res)
		  (findel (cdr x)))
		 ((and (listp (car x)) (not (equal x '())))
		  (findel (car x))
		  (findel (cdr x)))
		 ((not (equal x '()))
		  (findel (cdr x)))
		 (t '()))))
      (findel lst))
    res))

;; (print (find-elems '(1 2 (3 4) 4 (5 (4))) 4 #'equal))
;; (print (s-xml:parse-xml-file "example.xml"))
(print "testing xml element equality")
(defparameter *parsed-content* (s-xml:parse-xml-file "example.xml"))

;; important !!
(setf s-xml:*IGNORE-NAMESPACES* t)

(print (eq-keyword-str (car *parsed-content*) "note" ))


(print "testing finding elements in xml")
(print (find-elems *parsed-content* "from" #'eq-keyword-str))


(setf *parsed-content* (s-xml:parse-xml-file "./test-graphwalk/AgisParent/pom.xml"))
(print (find-elems *parsed-content* "module" #'eq-keyword-str))

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
