;;;; binoxxo.asd

(asdf:defsystem #:binoxxo
  :description "Solving binoxxo puzzles"
  :author "Martin Buchmann <Martin.Buchmann@gmail.com>"
  :license "Free for all"
  :depends-on (#:alexandria #:uiop)
  :serial t
  :components ((:file "package")
               (:file "debug")
               (:file "binoxxo")
               (:file "binoxxo-tests")))

