;;;; oit.asd

(asdf:defsystem #:oit
  :description "Describe oit here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:cepl.sdl2 :rtg-math :nineveh :skitter)
  :serial t
  :components ((:file "package")
               (:file "main")))
