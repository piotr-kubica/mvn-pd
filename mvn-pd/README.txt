This is the stub README.txt for the "mvn-pd" project.

Checkout repository (ex. ~/mvn-pd/)

Install dependencies:
* sbcl
* quicklisp

Run mvn-pd
Given Maven pom files in ~/example-pom/ run command from bash

sbcl --no-userinit --no-sysinit --non-interactive \
     --load ~/quicklisp/setup.lisp \
     --eval '(progn (push #p"~/mvn-pd/" asdf:*central-registry*) (ql:quickload "mvn-pd") )' \
     --eval '(mvn-pd::project-dependencies-dot (directory "~/example-pom/*"))'
