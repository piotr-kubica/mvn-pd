;;;; unit-tests.lisp

(in-package :mvn-pd-test)

(defun is-equal (a b)
  (is (equal a b)))

(defparameter *pom-parent*
    "<project xmlns=\"http://maven.apache.org/POM/4.0.0\">
	<modelVersion>4.0.0</modelVersion>
	<groupId>com.example</groupId>
	<artifactId>parent-artifact</artifactId>
	<packaging>pom</packaging>
	<version>0.1</version>
	<prerequisites>
		<maven>3.2.1</maven>
	</prerequisites>
	<modules>
	  <module>../ModuleA</module>
   	  <module>../ModuleB</module>
	  <module>../ModuleC</module>
	  <module></module>
	</modules>
	<dependencies>
	  <dependency>
	  <groupId>junit</groupId>
	  <artifactId>junit</artifactId>
	  <scope>test</scope>
	  </dependency>
	</dependencies>
     </project>")

(defparameter *pom-module-a*
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <project xmlns=\"http://maven.apache.org/POM/4.0.0\">
      <modelVersion atr=\"a\" btr=\"b\">4.0.0</modelVersion>
      <groupId>com.example</groupId>
      <artifactId>ModuleA</artifactId>
      <version>0.1</version>
      <name>Maven Pom Example</name>
      <parent>
        <groupId>com.example</groupId>
        <artifactId>parent-artifact</artifactId>
        <version>0.1</version>
        <relativePath>../Parent</relativePath> 
      </parent>
      <dependencies>
        <dependency>
          <groupId>junit</groupId>
          <artifactId>junit</artifactId>
          <version>4.8</version>
          <scope>test</scope>
        </dependency>
        <dependency>
          <groupId>com.example</groupId>
          <artifactId>ModuleB</artifactId>
          <version>0.1</version>
        </dependency>
        <dependency>
          <groupId>com.example</groupId>
          <artifactId>ModuleC</artifactId>
          <version>0.1</version>
        </dependency>
      </dependencies>
      </project>")

(defparameter *pom-module-b*
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <project xmlns=\"http://maven.apache.org/POM/4.0.0\">
      <modelVersion atr=\"a\" btr=\"b\">4.0.0</modelVersion>
      <groupId>com.example</groupId>
      <artifactId>ModuleB</artifactId>
      <version>0.1</version>
      <name>Maven Pom Example</name>
      <parent>
        <groupId>com.example</groupId>
        <artifactId>parent-artifact</artifactId>
        <version>0.1</version>
        <relativePath>../Parent</relativePath> 
      </parent>
      <dependencies>
        <dependency>
          <groupId>com.example</groupId>
          <artifactId>ModuleC</artifactId>
          <version>0.1</version>
        </dependency>
      </dependencies>
    </project>")

(defparameter *pom-module-c*
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <project xmlns=\"http://maven.apache.org/POM/4.0.0\">
      <modelVersion atr=\"a\" btr=\"b\">4.0.0</modelVersion>
      <groupId>com.example</groupId>
      <artifactId>ModuleC</artifactId>
      <version>0.1</version>
      <name>Maven Pom Example</name>
      <parent>
        <groupId>com.example</groupId>
        <artifactId>parent-artifact</artifactId>
        <version>0.1</version>
        <relativePath>../Parent</relativePath> 
      </parent>
    </project>")

(setf s-xml:*ignore-namespaces* t)

(defun xml-stream-to-lxml (xml-string)
  (s-xml:parse-xml
   (make-string-input-stream xml-string)))

(defparameter *pom-parent-lxml* (xml-stream-to-lxml *pom-parent*))
(defparameter *pom-module-a-lxml* (xml-stream-to-lxml *pom-module-a*))
(defparameter *pom-module-b-lxml* (xml-stream-to-lxml *pom-module-b*))
(defparameter *pom-module-c-lxml* (xml-stream-to-lxml *pom-module-c*))

(test remove-white-char-test
  ;; new line
  (is-equal (mvn-pd::remove-white-char "a b 
	  c") "abc")
  ;; space
  (is-equal (mvn-pd::remove-white-char " a b  c") "abc")

  (is-equal 
   (mvn-pd::remove-white-char "digraph { label=\"parent-artifact\";
       ModuleA -> ModuleB; 
       ModuleA -> ModuleC; 
       ModuleB -> ModuleC;
    }")
   "digraph{label=\"parent-artifact\";ModuleA->ModuleB;ModuleA->ModuleC;ModuleB->ModuleC;}"))

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
               :|d|))))


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
	

(test find-lxml-el-test
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


(test module-name-test
  (is-equal (mvn-pd::module-name *pom-module-a-lxml*)
            "ModuleA")
  (is-equal (mvn-pd::module-name *pom-parent-lxml*)
            "parent-artifact")
  (is-equal (mvn-pd::module-name nil "Default Name")
            "Default Name"))

(test depenencies-test
  (is-equal (mvn-pd::dependencies *pom-module-a-lxml*)
            '(((:|groupId| "junit")
               (:|artifactId| "junit")
               (:|version| "4.8")
               (:|scope| "test"))
              ((:|groupId| "com.example")
               (:|artifactId| "ModuleB")
               (:|version| "0.1"))
              ((:|groupId| "com.example")
               (:|artifactId| "ModuleC")
               (:|version| "0.1")))))

(test module-dependencies-list-test
  (is-equal (mvn-pd::module-dependency-list *pom-module-a-lxml*)
            '("ModuleA"
              ((:|groupId| "junit")
               (:|artifactId| "junit")
               (:|version| "4.8")
               (:|scope| "test"))
              ((:|groupId| "com.example")
               (:|artifactId| "ModuleB")
               (:|version| "0.1"))
              ((:|groupId| "com.example")
               (:|artifactId| "ModuleC")
               (:|version| "0.1"))))
  (is-equal (mvn-pd::module-dependency-list *pom-module-c-lxml*)
            '("ModuleC")))

(test parent-module?-test
  (is-true (mvn-pd::parent-module? *pom-parent-lxml*))
  (is-false (mvn-pd::parent-module? *pom-module-a-lxml*)))

(test child-module?-test
  (is-false (mvn-pd::child-module? *pom-parent*))
  (is-true (mvn-pd::child-module? *pom-module-a-lxml*)))

(test modules-test  
  (is-equal (mvn-pd::modules *pom-parent-lxml*)
            '("ModuleA" "ModuleB" "ModuleC")))
      
(test project-module-list-test
  (is-equal (mvn-pd::project-module-list *pom-parent-lxml*)
            '("parent-artifact" "ModuleA" "ModuleB" "ModuleC")))

(test read-dependencies-test
  ;; parent and modules present
  (is-equal (list-length 
             (mvn-pd::read-dependencies 
              (list *pom-parent* *pom-module-a* *pom-module-b* *pom-module-c*)
              #'s-xml:parse-xml-string))
            4)
  ;; only modules present - ok, still makes sense to display
  (is-equal (list-length 
             (mvn-pd::read-dependencies 
              (list *pom-module-a* *pom-module-b* *pom-module-c*)
              #'s-xml:parse-xml-string))
            4)
  ;; only parent present - makes no sense to display
  (is-equal (list-length 
             (mvn-pd::read-dependencies 
              (list *pom-parent*)
              #'s-xml:parse-xml-string))
            0)
  ;; none present - forget it!
  (is-equal (list-length 
             (mvn-pd::read-dependencies 
              (list)
              #'s-xml:parse-xml-string))
            0))
	     

(test mod-deps-containing-artifacts-test
  (is-equal (mvn-pd::mod-deps-containing-artifacts
             '("ModuleA" 
               ((:|groupId| "junit")
                (:|artifactId| "junit")
                (:|version| "4.8")
                (:|scope| "test"))
               ((:|groupId| "com.example")
                (:|artifactId| "ModuleB")
                (:|version| "0.1"))
               ((:|groupId| "com.example")
                (:|artifactId| "ModuleC")
                (:|version| "0.1")))

             '("ModuleA" "ModuleB" "ModuleC"))

            '("ModuleA" 
              ((:|groupId| "com.example")
               (:|artifactId| "ModuleB")
               (:|version| "0.1"))
              ((:|groupId| "com.example")
               (:|artifactId| "ModuleC")
               (:|version| "0.1"))))
  
  (is-equal (mvn-pd::mod-deps-containing-artifacts
             '("ModuleA" 
               ((:|groupId| "junit")
                (:|artifactId| "junit")
                (:|version| "4.8")
                (:|scope| "test"))
               ((:|groupId| "com.example")
                (:|artifactId| "ModuleB")
                (:|version| "0.1")))
             '("ModuleC"))
            '("ModuleA"))

  (is-equal (mvn-pd::mod-deps-containing-artifacts
             '("ModuleA")
             nil)
            '("ModuleA"))

  (is-equal (mvn-pd::mod-deps-containing-artifacts
             '("ModuleA")
             '("ModuleC"))
            '("ModuleA")))

(test project-dependencies-test
  (is-equal (mvn-pd::project-dependencies 
             (list *pom-parent* *pom-module-a* *pom-module-b* *pom-module-c*)
             #'s-xml:parse-xml-string)
            '("parent-artifact"
              ("ModuleA" 
               ((:|groupId| "junit")
                (:|artifactId| "junit")
                (:|version| "4.8")
                (:|scope| "test"))
               ((:|groupId| "com.example")
                (:|artifactId| "ModuleB")
                (:|version| "0.1"))
               ((:|groupId| "com.example")
                (:|artifactId| "ModuleC")
                (:|version| "0.1")))
              ("ModuleB" 
               ((:|groupId| "com.example")
                (:|artifactId| "ModuleC")
                (:|version| "0.1")))
              ("ModuleC"))))

;; shows only dependencies between modules
(test project-module-dependencies-test  
  (is-equal (mvn-pd::project-module-dependencies 
             (list *pom-parent* *pom-module-a* *pom-module-b* *pom-module-c*)
             #'s-xml:parse-xml-string)
            '("parent-artifact"
              ("ModuleA" 
               ((:|groupId| "com.example")
                (:|artifactId| "ModuleB")
                (:|version| "0.1"))
               ((:|groupId| "com.example")
                (:|artifactId| "ModuleC")
                (:|version| "0.1")))
              ("ModuleB" 
               ((:|groupId| "com.example")
                (:|artifactId| "ModuleC")
                (:|version| "0.1")))
              ("ModuleC"))))

(defparameter *project-dependencies-1* 
  '("parent-artifact"
    ("ModuleA" 
     ((:|groupId| "com.example")
      (:|artifactId| "ModuleC")
      (:|version| "0.1")))
    ("ModuleB")))

(defparameter *project-dependencies-2* 
  '("parent-artifact"
    ("ModuleA" 
     ((:|groupId| "com.example")
      (:|artifactId| "ModuleB")
      (:|version| "0.1"))
     ((:|groupId| "com.example")
      (:|artifactId| "ModuleC")
      (:|version| "0.1")))
    ("ModuleB" 
     ((:|groupId| "com.example")
      (:|artifactId| "ModuleC")
      (:|version| "0.1")))
    ("ModuleC")))

(test to-dot-format-test
  (is-equal (mvn-pd::to-dot-format *project-dependencies-1*)
            (mvn-pd::remove-white-char 
             "digraph { 
               label=\"parent-artifact\";
               ModuleA -> ModuleC; 
               ModuleB ;
             }"))
  (is-equal (mvn-pd::to-dot-format *project-dependencies-2*)
            (mvn-pd::remove-white-char 
             "digraph { 
                label=\"parent-artifact\";
                ModuleA -> ModuleB; 
                ModuleA -> ModuleC; 
                ModuleB -> ModuleC;
             }")))


       ;; TODO graphviz dot file
       ;; digraph
       ;; http://www.graphviz.org/content/dot-language
       ;; http://graphs.grevian.org/example
  
