;;;; mvn-pd.lisp

(in-package :mvn-pd)

;;; "mvn-pd" goes here. Hacks and glory await!

(defun hello-world ()
  (format t "Hello World, ~a!" "Piotr"))

(defun str2keyword (name)
  (intern name :keyword))

(defun eq-keyword-str (keyword name)
  (eq keyword (str2keyword name)))
