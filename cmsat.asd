;;;; +----------------------------------------------------------------+
;;;; | CMSAT                                                          |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:cmsat
  :description "Lisp bindings for cryptominisat5."
  :author "death <github.com/death>"
  :license "MIT"
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi #:cffi-libffi)
  :serial t
  :components
  ((:file "packages")
   (:cffi-grovel-file "grovel")
   (:file "cmsat")))
