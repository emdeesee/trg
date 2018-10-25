(asdf:defsystem #:trg
  :description "A pleasant Sirpinski gasket display."
  :author "Michael Cornelius <michael@ninthorder.com>"
  :license "GPL-3.0-or-later"
  :version "0.0.1"
  :serial t
  :depends-on (:ltk)
  :components ((:file "package")
               (:file "trg")))
