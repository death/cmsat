;;;; +----------------------------------------------------------------+
;;;; | CMSAT                                                          |
;;;; +----------------------------------------------------------------+

(defpackage #:cmsat
  (:use #:cl)
  (:import-from
   #:cffi
   #:define-foreign-library
   #:use-foreign-library
   #:defcstruct
   #:defcfun
   #:defctype
   #:with-foreign-object
   #:mem-aptr
   #:foreign-slot-value)
  (:export
   #:solve))
