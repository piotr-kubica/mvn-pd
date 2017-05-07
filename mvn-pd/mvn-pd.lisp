;;;; mvn-pd.lisp

(in-package :mvn-pd)


(defun str2keyword (name)
  (intern name :keyword))

(defun eq-keyword-str (keyword name)
  (eq keyword (str2keyword name)))
