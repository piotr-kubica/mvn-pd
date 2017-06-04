;;;; unit-tests.lisp

(in-package :mvn-pd-test)

(test str2keyword-test
  "str2keyword coerce string to keyword"
  (is (eq :|test| (str2keyword "test")))
  (is (eq :|Test| (str2keyword "Test")))
  (is (eq ':|TEST| (str2keyword "TEST"))))

(test eq-keyword-test
  "comapre keywords ignoring case"
  (is-true (eq-keyword :a :a))
  (is-false (eq-keyword :b :a))
  (is-true (eq-keyword :A :a))
  (is-true (eq-keyword :A :A)))

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
      (let ((s (make-string-output-stream)))
	;; (format s "test")
	;; (is (equal "test" (get-output-stream-string s)))
	(format s
       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
        <project xmlns=\"http://maven.apache.org/POM/4.0.0\">
    		<modelVersion>4.0.0</modelVersion>
    		<groupId>com.domain.example</groupId>
    		<artifactId>example-pom</artifactId>
    		<version>0.1-SNAPSHOT</version>
	<name>Maven Pom Example</name>
	<dependencies>
	        <dependency>
			<groupId>junit</groupId>
			<artifactId>junit</artifactId>
			<version>4.8</version>
			<scope>test</scope>
		</dependency>
        </dependencies>")
	
	;; (is (equal "example-pom" (find-module-name (parse-lxml s))))

	;; use
	;; (s-xml:parse-xml stream &key (output-type :lxml))   function
	;;        Parse a character stream as XML and generate a DOM of output-type, defaulting to :lxml
	
	))
	
