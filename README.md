# mvn-pd

The goal of this project is to help visualize Maven project module dependencies by creating _dot_ file from _pom_ files which speficy a modular project in the Maven build tool. Based on the _dot_ file you will be able to create a visual representation of the dependecy graph of your project by creating an image of graph with the help of _graphviz_.

### Installation

1. Checkout repository (ex. ~/mvn-pd/)

2. You will also need to install external dependencies:
    * **sbcl**, a Common Lisp compiler (includes _asdf_ by default)
    * **quicklisp**, a library manager for Common Lisp
    * **graphviz** (optional), tool for creating a visual representation (.png, .jpg, etc.) of the dependency graph

### Preparing pom files

To create a dot file you will need your pom files (parent pom and module pom's) to be processed.
The find-copy-rename-pom.sh will help you gather all pom files in one directory.
https://github.com/piotr-kubica/mvn-pd/blob/master/find-copy-rename/find-copy-rename-pom.sh

### Run mvn-pd

Given your:
- Maven pom files are located in ~/example-pom/ 
- mvn-pd repository path is ~/mvn-pd/
- quicklisp path is ~/quicklisp/

run command from bash

```
sbcl \
     --no-userinit --no-sysinit --non-interactive \
     --load ~/quicklisp/setup.lisp \
     --eval '(progn (push #p"~/mvn-pd/mvn-pd/" asdf:*central-registry*) (ql:quickload "mvn-pd") )' \
     --eval '(mvn-pd:project-dependencies-dot (directory "~/example-pom/*"))'
```
> **NOTE**: It this does't work. Run commands interactively from sbcl REPL.

You should see following or similar output:
```
[pio@fcenvy example-pom]$ sbcl \
>      --no-userinit --no-sysinit --non-interactive \
>      --load ~/quicklisp/setup.lisp \
>      --eval '(progn (push #p"~/mvn-pd/mvn-pd/" asdf:*central-registry*) (ql:quickload "mvn-pd") )' \
>      --eval '(mvn-pd:project-dependencies-dot (directory "~/example-pom/*"))'
This is SBCL 1.3.16, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
To load "mvn-pd":
  Load 1 ASDF system:
    mvn-pd
; Loading "mvn-pd"

[482] 2017.09.12(GMT+1) 20:28:14  INFO:  *** Starting mvn-pd ***
[482] 2017.09.12(GMT+1) 20:28:14  INFO:  Reading file "/home/pio/example-pom/pom-module-a" 
[497] 2017.09.12(GMT+1) 20:28:14  INFO:  Reading file "/home/pio/example-pom/pom-module-b" 
[498] 2017.09.12(GMT+1) 20:28:14  INFO:  Reading file "/home/pio/example-pom/pom-module-c" 
[498] 2017.09.12(GMT+1) 20:28:14  INFO:  Reading file "/home/pio/example-pom/pom-parent" 
[498] 2017.09.12(GMT+1) 20:28:14  INFO:  Reading completed. Files: 4, Modules: 0, Parent-modules: 0. 
[498] 2017.09.12(GMT+1) 20:28:14  INFO:  Writing dependencies to output file: "mvn-pd-output"
[498] 2017.09.12(GMT+1) 20:28:14  INFO:  *** mvn-pd finished ***
[pio@fcenvy example-pom]$ ls
mvn-pd.log  mvn-pd-output  pom-module-a  pom-module-b  pom-module-c  pom-parent

```


### Example

Given example pom modules: 

_pom-module-a_
```
<?xml version="1.0" encoding="UTF-8"?>
    <project xmlns="http://maven.apache.org/POM/4.0.0">
      <modelVersion atr="a" btr="b">4.0.0</modelVersion>
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
      </project>
```

_pom-module-b_
```<?xml version="1.0" encoding="UTF-8"?>
    <project xmlns="http://maven.apache.org/POM/4.0.0">
      <modelVersion atr="a" btr="b">4.0.0</modelVersion>
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
    </project>

```


_pom-module-c_
```<?xml version="1.0" encoding="UTF-8"?>
    <project xmlns="http://maven.apache.org/POM/4.0.0">
      <modelVersion atr="a" btr="b">4.0.0</modelVersion>
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
    </project>

```


_pom-parent_
(Note that _ModuleC_ is not present)
```
<?xml version="1.0" encoding="UTF-8"?>
  <project xmlns="http://maven.apache.org/POM/4.0.0">
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
	  <module></module>
	</modules>
	<dependencies>
	  <dependency>
	  <groupId>junit</groupId>
	  <artifactId>junit</artifactId>
	  <scope>test</scope>
	  </dependency>
	</dependencies>
  </project>
     
```

run (from sbcl REPL)
```lisp
(mvn-pd:project-dependencies-dot '("pom-parent" "pom-module-a"
                                   "pom-module-b" "pom-module-c"))				       
```
				       
to get output:

_mvn-pd-output_
```
digraph { 
     label="parent-artifact";
     ModuleA -> ModuleB;
     ModuleA -> ModuleC;
     ModuleB -> ModuleC;
     ModuleC;
}
```
which you can turn into a _png_ image with graphviz
by running command
```
dot -Tpng mvn-pd-output -o graph.png

```
![graph.png](https://github.com/piotr-kubica/mvn-pd/blob/master/example-output/graph.png)

_mvn-pd.log_
```
[6961797] 2017.09.11(GMT+1) 23:55:20  INFO:  *** Starting mvn-pd ***
[6961798] 2017.09.11(GMT+1) 23:55:20  INFO:  Reading file "pom-parent" 
[6961798] 2017.09.11(GMT+1) 23:55:20  INFO:  Reading file "pom-module-a" 
[6961798] 2017.09.11(GMT+1) 23:55:20  INFO:  Reading file "pom-module-b" 
[6961799] 2017.09.11(GMT+1) 23:55:20  INFO:  Reading file "pom-module-c" 
[6961799] 2017.09.11(GMT+1) 23:55:20  INFO:  Reading parent module "parent-artifact" 
[6961799] 2017.09.11(GMT+1) 23:55:20  INFO:  Reading module "ModuleA" 
[6961799] 2017.09.11(GMT+1) 23:55:20  INFO:  Reading module "ModuleB" 
[6961799] 2017.09.11(GMT+1) 23:55:20  INFO:  Reading module "ModuleC" 
[6961799] 2017.09.11(GMT+1) 23:55:20  INFO:  Reading completed. Files: 4, Modules: 3, Parent-modules: 1. 
[6961799] 2017.09.11(GMT+1) 23:55:20  INFO:  Writing dependencies to output file: "mvn-pd-output"
[6961799] 2017.09.11(GMT+1) 23:55:20  INFO:  *** mvn-pd finished ***
```
