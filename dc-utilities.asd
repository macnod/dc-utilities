;;;; dc-utilities.asd

(asdf:defsystem :dc-utilities
  :description "Functions that I use in most of my programs."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:cl-ppcre :yason :ironclad)
  :serial t
  :components ((:file "package")
               (:file "dc-utilities")))

