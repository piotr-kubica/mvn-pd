;;;; mvn-pd.lisp

(in-package :mvn-pd)

(defun str2keyword (name)
  (intern name :keyword))

(defun eq-keyword (keyword1 keyword2)
  ;; check equalisy but ignore keyword case-sensitivity
  (eq keyword1 keyword2))

(defun find-lxml-elems (lst elem eqfun)
  (let ((res (list)))
    (labels ((notemptylst (x)
	       (and (not (null x)) (listp x)))
	     (eq-to-elem (x)
	       (funcall eqfun x elem))
	     (find-el (lst)
	       (mapc (lambda (e)
		       (when (notemptylst e)
					; car for element without attributes
			 (if (and
			      (or (eq-to-elem (car e))
					; caar for element with attributes
				  (and (notemptylst (car e))
				       (eq-to-elem (caar e))))
					; second element exisits
			      (not (null (cadr e))))
					; add to result-collection
			     (push (cadr e) res)
					; otherwise search deeper
			     (find-el e))))
		     lst)))
      (when (notemptylst lst)
	(find-el lst)))
    res))



;; (find-elems '(a (b c) (d e)) 'b #'equal)

;; (C)
;; MVN-PD-TEST> (find-elems '(a (b c) (d e (b f))) 'b #'equal)
			 
;; (F C)
;; MVN-PD-TEST> (find-elems '(a (b) (b c) (d e (b f))) 'b #'equal)
			 
;; (F C)
;; MVN-PD-TEST> (find-elems '(a () (b) (b c) (d e (b f))) 'b #'equal)
			 
;; (F C)
;; MVN-PD-TEST> (find-elems '(a () (b) ((b g h) c) (d e (b f))) 'b #'equal)
			 
;; (F C)
;; MVN-PD-TEST> 


