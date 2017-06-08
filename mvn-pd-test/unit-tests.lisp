;;;; unit-tests.lisp

(in-package :mvn-pd-test)

;; TODO fix! argument
(test keyword->str-test
  "keyword->str coerces keyword to string"
  (is (equal "test" (keyword->str "test" )))
  (is (equal "test" (keyword->str :|test| )))
  (is (equal "Test" (keyword->str :|Test| )))
  (is (equal "TEST" (keyword->str :test )))
  (is (equal "TEST" (keyword->str :Test )))
  (is (equal "TEST" (keyword->str ':|TEST| ))))

(test find-elems-test
  "find elements in list"
  (is (equal nil  (find-lxml-elems '() 'x)))
  (is (equal nil  (find-lxml-elems '(a b c) 'x )))
  (is (equal '((b c))  (find-lxml-elems '(b c) 'b )))
  (is (equal '((b c d))  (find-lxml-elems '(b c d) 'b )))
  (is (equal '((b c c))  (find-lxml-elems '(b c c) 'b )))
  (is (equal 'nil  (find-lxml-elems '((b) c) 'b ))) ; not valid -first elem is nested but shouldnt
  (is (equal 'nil  (find-lxml-elems '(((b)) c d) 'b ))) ; not valid - first elem is nested but shouldnt
  (is (equal 'nil  (find-lxml-elems '(a (b) c) 'b ))) ; single nested elems are not valid elems
  (is (equal 'nil  (find-lxml-elems '(a ((b)) c) 'b ))) ; single nested elems are not valid elems
  (is (equal '((b c))  (find-lxml-elems '(a (b c) d) 'b )))
  (is (equal 'nil  (find-lxml-elems '(a ((b) c) d) 'b ))) ; single nested elems are not valid elems
  (is (equal '(((b t) c))  (find-lxml-elems '(a ((b t) c) d) 'b ))) ; elem b  with attribute t
  (is (equal 'nil  (find-lxml-elems '(a ((b) c (d)) e) 'b ))) ; single nested elems are not valid elems
  (is (equal '((b c) (b d) (b e f))  (find-lxml-elems '(a (b c) (b d) (b e f)) 'b )))
  (is (equal '((b c) (b d)) (find-lxml-elems '(a (b c) (b d) ((b) e f)) 'b )))) ; single nested elems are not valid elems

(test find-nested-elems-test
  (is (equal '(()) (find-lxml-nested-elems '(()) '() )))
  (is (equal '(()) (find-lxml-nested-elems '(()) '(a) )))
  (is (equal '(()) (find-lxml-nested-elems '((a)) '() )))
  (is (equal '((a)) (find-lxml-nested-elems '((a)) '(a) )))
  (is (equal '((b c)) (find-lxml-nested-elems '((a b c)) '(b) )))
  (is (equal '((e f) (e f)) (find-lxml-nested-elems '((a b c d e f) (a (b c d e f))) '(b c d e) )))
  (is (equal '((e f) (e)) (find-lxml-nested-elems '((a b c d e f) (a (b c (d e) f))) '(b c d e) ))))

(test get-lxml-values
  (is (equal nil (get-lxml-values '() )))
  (is (equal nil (get-lxml-values '(()) )))
  (is (equal nil (get-lxml-values '((a)) )))
  (is (equal '(b d) (get-lxml-values '((a b) (c d)) )))
  (is (equal '(b d) (get-lxml-values '((a b) (c d) (e)) )))
  (is (equal '(b d (f g)) (get-lxml-values '((a b) (c d) (e (f g))) ))))

(test module-name-test
  (setf s-xml:*ignore-namespaces* t)
  (let* ((iss (make-string-input-stream
       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
        <project xmlns=\"http://maven.apache.org/POM/4.0.0\">
    		<modelVersion atr=\"a\" btr=\"b\">4.0.0</modelVersion>
    		<groupId>com.example</groupId>
    		<artifactId>examplePom</artifactId>
    		<version>0.1-SNAPSHOT</version>
	<name>Maven Pom Example</name>
	<dependencies>
	        <dependency>
			<groupId>junit</groupId>
			<artifactId>junit</artifactId>
			<version>4.8</version>
			<scope>test</scope>
		</dependency>
        </dependencies>
        </project>"))
	(lxml (s-xml:parse-xml iss)))
    (is-true (input-stream-p iss))
    (print lxml)
    (format t "~%find lxml elems")
    (print (find-lxml-elems lxml "project"
				   #'(lambda (e x)
				       (equal e (keyword->str x)))))
    (format t "~%find lxml nested elems")
    (print (find-lxml-nested-elems lxml '("artifactId")
				   #'(lambda (e x)
				       (equal e (keyword->str x)))))
;;    (is (equal "examplePom" (find-module-name lxml)))
    ))
   


