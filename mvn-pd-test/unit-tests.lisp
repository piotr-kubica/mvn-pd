;;;; unit-tests.lisp

(in-package :mvn-pd-test)

(defun is-equal (a b)
  (is (equal a b)))

(setf s-xml:*ignore-namespaces* t)

(defparameter *parent-lxml*
  (s-xml:parse-xml
   (make-string-input-stream
    "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">
	<modelVersion>4.0.0</modelVersion>
	<groupId>com.example</groupId>
	<artifactId>parent-artifact</artifactId>
	<packaging>pom</packaging>
	<version>1.1.1</version>
	<prerequisites>
		<maven>3.2.1</maven>
	</prerequisites>
	<modules>
	  <module>../ModuleA</module>
   	  <module>../ModuleB</module>
	  <module>../ModuleC</module>
	</modules>
	<dependencies>
	  <dependency>
	  <groupId>junit</groupId>
	  <artifactId>junit</artifactId>
	  <scope>test</scope>
	  </dependency>
	</dependencies>
     </project>"
    )))

(defparameter *module-lxml*
  (s-xml:parse-xml
   (make-string-input-stream
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
        <project xmlns=\"http://maven.apache.org/POM/4.0.0\">
    		<modelVersion atr=\"a\" btr=\"b\">4.0.0</modelVersion>
    		<groupId>com.example</groupId>
    		<artifactId>examplePom</artifactId>
    		<version>1.7.8</version>
	<name>Maven Pom Example</name>
	<dependencies>
	        <dependency>
			<groupId>junit</groupId>
			<artifactId>junit</artifactId>
			<version>4.8</version>
			<scope>test</scope>
		</dependency>
	        <dependency>
			<groupId>mockito</groupId>
			<artifactId>mockito</artifactId>
			<version>1.9.2</version>
			<scope>test</scope>
		</dependency>
        </dependencies>
        </project>")))


(test keyword->str-test
  "keyword->str coerces keyword to string"
  (is-equal nil (mvn-pd::keyword->str "test" ))
  (is-equal "test" (mvn-pd::keyword->str :|test| ))
  (is-equal "Test" (mvn-pd::keyword->str :|Test| ))
  (is-equal "TEST" (mvn-pd::keyword->str :test ))
  (is-equal "TEST" (mvn-pd::keyword->str :Test ))
  (is-equal "TEST" (mvn-pd::keyword->str ':|TEST| )))

(test children-elem?-test
  (is-false (mvn-pd::children-elem? nil ))
  (is-false (mvn-pd::children-elem? :|vb| ))
  (is-false (mvn-pd::children-elem? '((:|vb| :|atr| "a") "text") ))
  (is-false (mvn-pd::children-elem? '(:|vb| "text") ))
  (is-true (mvn-pd::children-elem? '((:|vb| :|atr| "a") "text" :|vc|) ))
  (is-true (mvn-pd::children-elem? '((:|vb| :|atr| "a") :|vc|) ))
  (is-true (mvn-pd::children-elem? '(:|vb| :|vc|) )))

(test value-test
      (is-equal "elem value" (mvn-pd::value '(:|elem| "elem value")))
      (is-equal nil (mvn-pd::value '(:|elem|)))
      (is-equal nil (mvn-pd::value '())))

(test children-elem-test
  (is-equal nil (mvn-pd::children-elem nil ))
  (is-equal nil (mvn-pd::children-elem :|a| ))
  (is-equal nil (mvn-pd::children-elem '((:|a| :|atr| "a") "text") ))
  (is-equal nil (mvn-pd::children-elem '(:|a| "text") ))
  (is-equal '(:|c|) (mvn-pd::children-elem '((:|a| :|atr| "a") "text" :|c|) ))
  (is-equal '(:|c|) (mvn-pd::children-elem '((:|b| :|atr| "a") :|c|) ))
  (is-equal '((:|c| :|d|) :|e|) (mvn-pd::children-elem '(:|b| (:|c| :|d|) :|e|) )))


;; setup common data for lxml tests
(setf s-xml:*ignore-namespaces* t)
(defparameter +sis+ (make-string-input-stream
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
        <r ns=\"http://maven.apache.org/POM/4.0.0\">
	  <a atr1=\"a\" atr2=\"b\">text</a>
          <b></b>
          <c>example<f></f></c>
          <d>
            <b atr=\"b\">text-b</b>
            <c atr=\"c\">text-c<e></e></c>
            <d></d>
          </d>
        </r>"))
(defparameter +lxml+ (s-xml:parse-xml +sis+))


(test data-lxml-test
  (is-true (input-stream-p +sis+))
  (is-equal +lxml+ 
            '((:|r| :|ns| "http://maven.apache.org/POM/4.0.0")
              ((:|a| :|atr1| "a" :|atr2| "b") "text")
              :|b|
              (:|c| "example" :|f|)
              (:|d|
               ((:|b| :|atr| "b") "text-b")
               ((:|c| :|atr| "c") "text-c"
                :|e|)
               :|d|)))
  (print +lxml+))


(test find-lxml-el-test
    (is-equal nil (find-lxml-el +lxml+ :|notexisting|))
    (is-equal nil (find-lxml-el nil :|r|))

    (is-equal (find-lxml-el +lxml+ :|a|)
              '(((:|a| :|atr1| "a" :|atr2| "b") "text")))
    
    (is-equal (find-lxml-el +lxml+ :|b|)
              '(((:|b| :|atr| "b") "text-b") :|b|))
    
    (is-equal (find-lxml-el +lxml+ :|b| :max-nest 1)
              '(:|b|))

    (is-equal (find-lxml-el +lxml+ :|b| :max-nest 0)
              nil)
    
    (is-equal (find-lxml-el +lxml+ :|e|)
              '(:|e|))
    
    (is-equal (find-lxml-el +lxml+ :|atr|)
              nil))


(test find-lxml-el-children
  (is-equal (mvn-pd::find-lxml-el-children +lxml+ :|a|)
            nil)
    
  (is-equal (mvn-pd::find-lxml-el-children +lxml+ :|b|)
            nil)

  (is-equal (mvn-pd::find-lxml-el-children +lxml+ :|c|)
            '(:|e| :|f|) )

  (is-equal (mvn-pd::find-lxml-el-children '(:|b| (:|c| :|d|) :|e|) :|b|)
            '((:|c| :|d|) :|e| ))

  (is-equal (mvn-pd::find-lxml-el-children +lxml+ nil)
            nil)

  (is-equal (mvn-pd::find-lxml-el-children +lxml+ :|notExisting|)
            nil)

  (is-equal (mvn-pd::find-lxml-el-children +lxml+ :|c| :max-nest 1)
            '(:|f|) ))


(test find-nested-el-test

  (is-equal (find-nested-el nil '(:|b| :|c|))
            nil)

  (is-equal (find-nested-el +lxml+ nil)
            nil)

  (is-equal (find-nested-el '(:|b| (:|c| :|d|) :|e|) '(:|b| :|c|))
            '((:|c| :|d|)) )

  (is-equal (find-nested-el +lxml+ '(:|d| :|c|))
            '(((:|c| :|atr| "c") "text-c" :|e|)) )
  
  (is-equal (find-nested-el +lxml+ '(:|r| :|d| :|c|))
            '(((:|c| :|atr| "c") "text-c" :|e|)) )
  
  (is-equal (find-nested-el +lxml+ '(:|r| :|c|))
            '((:|c| "example" :|f|) ((:|c| :|atr| "c") "text-c" :|e|)) ))
	

(test find-lxml-el-integration-test
  (let* ((sis (make-string-input-stream
       "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
        <project>
	  <name>Maven Pom Example</name>
	  <name>Maven Pom Example 2</name>
        </project>"))
	(lxml (s-xml:parse-xml sis)))
    (is-true (input-stream-p sis))
    ;; (print lxml)
    (is-equal '((:|name| "Maven Pom Example 2") (:|name| "Maven Pom Example"))
              (find-lxml-el lxml :|name|))
    (is-equal nil
              (find-lxml-el lxml :|Name|))))


(test integration-tests
  
  (is-equal (mvn-pd::module-name *module-lxml*)
            "examplePom")

  (is-equal (mvn-pd::dependencies *module-lxml*)
            '((:|dependency|
               (:|groupId| "junit")
               (:|artifactId| "junit")
               (:|version| "4.8")
               (:|scope| "test"))
              (:|dependency|
               (:|groupId| "mockito")
               (:|artifactId| "mockito")
               (:|version| "1.9.2")
               (:|scope| "test"))) )

  (is-equal (mvn-pd::module-dependency-list *module-lxml*)
            '("examplePom"
              (:|dependency|
               (:|groupId| "junit")
               (:|artifactId| "junit")
               (:|version| "4.8")
               (:|scope| "test"))
              (:|dependency|
               (:|groupId| "mockito")
               (:|artifactId| "mockito")
               (:|version| "1.9.2")
               (:|scope| "test"))) )

  (is-equal (mvn-pd::modules *parent-lxml*)
            '("ModuleA" "ModuleB" "ModuleC"))
      
  (is-equal (mvn-pd::project-module-list *parent-lxml*)
            '("parent-artifact" "ModuleA" "ModuleB" "ModuleC"))
	     
  (is-equal (mvn-pd::project-module-dependencies *parent-lxml*)
            '(("parent-artifact")
              ("../ModuleA" . (((:|groupId| "junit")
                                (:|artifactId| "junit")
                                (:|version| "4.8")
                                (:|scope| "test"))
                               ((:|groupId| "com.example")
                                (:|artifactId| "ModuleB")
                                (:|version| "1.0"))))
              ("../ModuleB" . (((:|groupId| "com.example")
                                (:|artifactId| "ModuleC")
                                (:|version| "1.0"))))
              ("../ModuleC" . '() )))


)
