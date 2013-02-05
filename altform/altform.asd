;;;; altform.asd

(asdf:defsystem #:altform
  :serial t
  :description "Describe altform here"
  :author "Jason Aeschliman <j.aeschliman@gmail.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:com.clearly-useful.protocols
               #:com.clearly-useful.generic-collection-interface
               #:named-readtables)
  :components ((:file "package")
               (:file "hooks")
               (:file "redefs")
               (:file "qq")
               (:file "base")
               (:file "classes")
               (:file "dbind")
               (:file "altform")))

