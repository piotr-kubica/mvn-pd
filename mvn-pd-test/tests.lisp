;;;; tests.lisp

(in-package :mvn-pd-test)

(defun is-equal (a b)
  (is (equal a b)))

(defun remove-white-char (s)
  (let ((chars (coerce s 'list))
        (white-chars '(#\Space #\Newline #\Backspace #\Tab 
                       #\Linefeed #\Page #\Return #\Rubout)))
    (coerce (mapcan 
             (lambda (c) (and (not (find c white-chars)) 
                              (list c))) 
             chars) 
            'string)))

(test remove-white-char-test
  ;; new line
  (is-equal (remove-white-char "a b 
	  c") "abc")
  ;; space
  (is-equal (remove-white-char " a b  c") "abc")

  (is-equal 
   (remove-white-char "a
       b
           c d     
 
    ")
   "abcd"))

(defun is-equal-stripped (a b)
  (is (and (stringp a) (stringp b)
           (equal (remove-white-char a) (remove-white-char b)))))


(defun write-variable-content-to-file (v)
  (when (boundp v)
    (let ((filename (string-downcase (remove #\* (symbol-name v)))))
      (format t "~&Writing file \"~a~a\" ~&" (truename ".") filename)
      (with-open-file (s filename
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
        (format s "~a" (eval v))))))

(defun read-file-to-string (filename)
  (format t "~&Reading file ~a~a~&" (truename ".") filename)
  (with-open-file (stream filename)
    (format nil "~{~a~}" 
            (loop for line = (read-line stream nil)
               while line
               collect line))))

(setf s-xml:*ignore-namespaces* t)


(defun xml-stream-to-lxml (xml-string)
  (s-xml:parse-xml
   (make-string-input-stream xml-string)))

;; mvn-pd tests

(defparameter *pom-parent*
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <project xmlns=\"http://maven.apache.org/POM/4.0.0\">
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


(defparameter *pom-parent-lxml* (xml-stream-to-lxml *pom-parent*))
(defparameter *pom-module-a-lxml* (xml-stream-to-lxml *pom-module-a*))
(defparameter *pom-module-b-lxml* (xml-stream-to-lxml *pom-module-b*))
(defparameter *pom-module-c-lxml* (xml-stream-to-lxml *pom-module-c*))

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
(defparameter *example-pom-1* (make-string-input-stream
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

(defparameter *lxml-2*
  '((:|project| :|xmlns| "namespace" )
    (:|modelVersion| "1.0")
    (:|parent| 
     (:|groupId| "com.example") 
     (:|artifactId| "ParentPom")
     (:|version| "1.0") 
     (:|relativePath| "../ParentPom"))
    (:|artifactId| "ArtifactId") 
    (:|packaging| "jar")
    (:|dependencies|
     (:|dependency| 
      (:|groupId| "org.someliv")
      (:|artifactId| "annotations")))))

(defparameter *lxml-1* (s-xml:parse-xml *example-pom-1*))


(test data-lxml-test
  (is-true (input-stream-p *example-pom-1*))
  (is-equal *lxml-1* 
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
    (is-equal nil (mvn-pd::find-lxml-el *lxml-1* :|notexisting|))
    (is-equal nil (mvn-pd::find-lxml-el nil :|r|))

    (is-equal (mvn-pd::find-lxml-el *lxml-1* :|a|)
              '(((:|a| :|atr1| "a" :|atr2| "b") "text")))
    
    (is-equal (mvn-pd::find-lxml-el *lxml-1* :|b|)
              '(((:|b| :|atr| "b") "text-b") :|b|))
    
    (is-equal (mvn-pd::find-lxml-el *lxml-1* :|b| :max-nest 1)
              '(:|b|))

    (is-equal (mvn-pd::find-lxml-el *lxml-1* :|b| :max-nest 0)
              nil)
    
    (is-equal (mvn-pd::find-lxml-el *lxml-1* :|e|)
              '(:|e|))
    
    (is-equal (mvn-pd::find-lxml-el *lxml-1* :|atr|)
              nil)

    (is-equal (mvn-pd::find-lxml-el *lxml-2* :|artifactId| :max-nest 1)
              '((:|artifactId| "ArtifactId")) ))


(test find-lxml-el-children
  (is-equal (mvn-pd::find-lxml-el-children *lxml-1* :|a|)
            nil)
    
  (is-equal (mvn-pd::find-lxml-el-children *lxml-1* :|b|)
            nil)

  (is-equal (mvn-pd::find-lxml-el-children *lxml-1* :|c|)
            '(:|e| :|f|) )

  (is-equal (mvn-pd::find-lxml-el-children '(:|b| (:|c| :|d|) :|e|) :|b|)
            '((:|c| :|d|) :|e| ))

  (is-equal (mvn-pd::find-lxml-el-children *lxml-1* nil)
            nil)

  (is-equal (mvn-pd::find-lxml-el-children *lxml-1* :|notExisting|)
            nil)

  (is-equal (mvn-pd::find-lxml-el-children *lxml-1* :|c| :max-nest 1)
            '(:|f|) ))


(test find-nested-el-test

  (is-equal (mvn-pd::find-nested-el nil '(:|b| :|c|))
            nil)

  (is-equal (mvn-pd::find-nested-el *lxml-1* nil)
            nil)

  (is-equal (mvn-pd::find-nested-el '(:|b| (:|c| :|d|) :|e|) '(:|b| :|c|))
            '((:|c| :|d|)) )

  (is-equal (mvn-pd::find-nested-el *lxml-1* '(:|d| :|c|))
            '(((:|c| :|atr| "c") "text-c" :|e|)) )
  
  (is-equal (mvn-pd::find-nested-el *lxml-1* '(:|r| :|d| :|c|))
            '(((:|c| :|atr| "c") "text-c" :|e|)) )
  
  (is-equal (mvn-pd::find-nested-el *lxml-1* '(:|r| :|c|))
            '((:|c| "example" :|f|)))

  (is-equal (mvn-pd::find-nested-el *lxml-2* '(:|project| :|artifactId|))
            '((:|artifactId| "ArtifactId"))) )
	

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
              (mvn-pd::find-lxml-el lxml :|name|))
    (is-equal nil
              (mvn-pd::find-lxml-el lxml :|Name|))))


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


(test dependency-value-test
  (is-equal (mvn-pd::dependency-value 
             '((:|groupId| "junit")
               (:|artifactId| "junit")
               (:|version| "4.8")
               (:|scope| "test"))
             "artifactId")
            "junit")
  (is-equal (mvn-pd::dependency-value 
             '((:|groupId| "junit")
               (:|artifactId| "junit")
               (:|version| "4.8")
               (:|scope| "test"))
             "")
            nil)
  (is-equal (mvn-pd::dependency-value 
             '()
             "artifactId")
            nil))


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
  (is-equal-stripped
   (mvn-pd::to-dot-format *project-dependencies-1*)
   "digraph { 
          label=\"parent-artifact\";
          ModuleA -> ModuleC; 
          ModuleB ;
    }")
  (is-equal-stripped
   (mvn-pd::to-dot-format *project-dependencies-2*)
   "digraph { 
          label=\"parent-artifact\";
          ModuleA -> ModuleB; 
          ModuleA -> ModuleC; 
          ModuleB -> ModuleC;
          ModuleC;
     }"))

(test project-dependencies-dot-integration-test
  ;; TODO logs created integrations test
  ;; truncate log file before writing
  ;; truncate -s 0 mvn-pd.log

  ;; test setup - write modules to sparate files
  ;; given
  (let ((modules `(*pom-parent* 
                   *pom-module-a* 
                   *pom-module-b* 
                   *pom-module-c*)))
    (mapc (lambda (m)
            (write-variable-content-to-file m))
          modules)
    ;; when
    (mvn-pd:project-dependencies-dot '("pom-parent" "pom-module-a"
                                       "pom-module-b" "pom-module-c"))
    ;; then
    (is-equal-stripped
     (read-file-to-string "mvn-pd-output")
     "digraph { 
          label=\"parent-artifact\";
          ModuleA -> ModuleB; 
          ModuleA -> ModuleC; 
          ModuleB -> ModuleC;
          ModuleC;
     }")))

