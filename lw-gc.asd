;;;; lw-gc.asd

(asdf:defsystem #:lw-gc
  :description "Graphical interface for GC tuning."
  :author "Eric Lorenzana"
  :license "ISC"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "lw-gc")))
