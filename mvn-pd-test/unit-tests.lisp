;;;; unit-tests.lisp

(in-package :mvn-pd-test)

(test keyword->str-test
  "keyword->str coerces keyword to string"
  (is (equal nil (keyword->str "test" )))
  (is (equal "test" (keyword->str :|test| )))
  (is (equal "Test" (keyword->str :|Test| )))
  (is (equal "TEST" (keyword->str :test )))
  (is (equal "TEST" (keyword->str :Test )))
  (is (equal "TEST" (keyword->str ':|TEST| ))))

(test find-lxml-el-test
  (let ((lxml
	 '((:|project| :|xmlns| "http://maven.apache.org/POM/4.0.0")
	   ((:|modelVersion| :|atr| "a" :|btr| "b") "4.0.0") (:|groupId| "com.example")
	   (:|artifactId| "examplePom")
	   (:|version| :|va| ((:|vb| :|atr| "a") "text2")
	    ((:|vb| :|atr| "a") "text" :|vc|))
	   (:|name| "Maven Pom Example")
	   (:|dependencies|
	    (:|dependency| (:|groupId| "junit") (:|artifactId| "junit")
	     (:|version| "4.8") (:|scope| "test")))) ))
    
    (is (equal nil (find-lxml-el lxml :|notexisting|)))
    (is (equal nil (find-lxml-el nil :|name|)))
    (is (equal '(((:|vb| :|atr| "a") "text" :|vc|) ((:|vb| :|atr| "a") "text2"))
	       (find-lxml-el lxml :|vb|)))
    (is (equal '(:|vc|)
	       (find-lxml-el lxml :|vc|)))
    (is (equal nil
	       (find-lxml-el lxml :|atr|))) ))


(test find-lxml-el-integration-test
  (setf s-xml:*ignore-namespaces* t)
  (let* ((sis (make-string-input-stream
       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
        <project>
	  <name>Maven Pom Example</name>
	  <name>Maven Pom Example 2</name>
        </project>"))
	(lxml (s-xml:parse-xml sis)))
    (is-true (input-stream-p sis))
    (print lxml)
    (is (equal '((:|name| "Maven Pom Example 2") (:|name| "Maven Pom Example"))
	       (find-lxml-el lxml :|name|)))
    (is (equal nil
	       (find-lxml-el lxml :|Name|)))))

;; TODO rewrite with realworld example
;; (test find-nested-elems-test
;;   (is (equal '(()) (find-lxml-nested-elems '(()) '() )))
;;   (is (equal '(()) (find-lxml-nested-elems '(()) '(a) )))
;;   (is (equal '(()) (find-lxml-nested-elems '((a)) '() )))
;;   (is (equal '((a)) (find-lxml-nested-elems '((a)) '(a) )))
;;   (is (equal '((b c)) (find-lxml-nested-elems '((a b c)) '(b) )))
;;   (is (equal '((e f) (e f)) (find-lxml-nested-elems '((a b c d e f) (a (b c d e f))) '(b c d e) )))
;;   (is (equal '((e f) (e)) (find-lxml-nested-elems '((a b c d e f) (a (b c (d e) f))) '(b c d e) ))))


;; TEST data
;;
;;        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
;;         <project xmlns=\"http://maven.apache.org/POM/4.0.0\">
;;     		<modelVersion atr=\"a\" btr=\"b\">4.0.0</modelVersion>
;;     		<groupId>com.example</groupId>
;;     		<artifactId>examplePom</artifactId>
;;     		<version>
;;                     <va></va>
;;                     <vb atr=\"a\">text</vb>
;;                     <vb atr=\"a\">text <vc></vc></vb>
;;                 </version>
;; 	<name>Maven Pom Example</name>
;; 	<dependencies>
;; 	        <dependency>
;; 			<groupId>junit</groupId>
;; 			<artifactId>junit</artifactId>
;; 			<version>4.8</version>
;; 			<scope>test</scope>
;; 		</dependency>
;;         </dependencies>
;;         </project>"))



