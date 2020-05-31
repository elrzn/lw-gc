;;;; lw-gc.asd

(asdf:defsystem #:lw-gc
  :description "Describe lw-gc here"
  :author "Eric Lorenzana"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "lw-gc")))
