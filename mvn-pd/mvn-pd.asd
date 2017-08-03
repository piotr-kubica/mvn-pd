;;;; mvn-pd.asd

(asdf:defsystem :mvn-pd
  :description "Visualize Maven project dependencies"
  :author "Piotr Kubica <piotr.kubica87@gmail.com>"
  :license "MIT License"
  :depends-on (:s-xml)
  :serial t
  :components ((:file "package")
               (:file "mvn-pd")))

