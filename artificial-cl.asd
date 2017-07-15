;;;; artificial-cl.asd

(asdf:defsystem #:artificial-cl
  :description "Code and solutions to Paradigms of AI Programing"
  :author "Ignacy Moryc"
  :license "Public Domain"
  :serial t
  :components ((:file "package")
               (:file "artificial-cl")
               (:file "ch2-random-lang")))
