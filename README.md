# mvn-pd

The goal of this project is to help visualize Maven project module dependencies by creating _dot_ file from _pom_ files which speficy a modular project in the Maven build tool. Based on the _dot_ file you will be able to create a visual representation of the dependecy graph of your project by creating an image of graph with the help of _graphviz_.

### Preparing pom files

To create a dot file you will need your pom files (parent pom and module pom's) to be processed.
The find-copy-rename-pom.sh will help you gather all pom files in one directory.
https://github.com/piotr-kubica/mvn-pd/blob/master/find-copy-rename/find-copy-rename-pom.sh

### mvn-pd installation

1. Checkout repository (ex. ~/mvn-pd/)

2. You will also need to install external dependencies:
    * sbcl (asdf by default included)
    * quicklisp
    * graphviz (to create a visual representation of the dependency graph .png, .jpg, etc.)

### Run mvn-pd

Given 
- your Maven pom files are located in ~/example-pom/ 
- your mvn-pd repository path is ~/mvn-pd/
- and your quicklisp path is ~/quicklisp/

run command from bash

```
sbcl \
     --no-userinit --no-sysinit --non-interactive \
     --load ~/quicklisp/setup.lisp \
     --eval '(progn (push #p"~/mvn-pd/" asdf:*central-registry*) (ql:quickload "mvn-pd") )' \
     --eval '(mvn-pd::project-dependencies-dot (directory "~/example-pom/*"))'
```

### Example

- [ ] TODO step by step example (with example pom's included)
