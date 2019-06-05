;;;; +----------------------------------------------------------------+
;;;; | CMSAT                                                          |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:cmsat
  :description "Cryptominisat5 bindings for Common Lisp."
  :author "death <github.com/death>"
  :license "MIT"
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi #:cffi-libffi)
  :serial t
  :components
  ((:file "packages")
   (:cffi-grovel-file "grovel")
   (:file "cmsat")))
