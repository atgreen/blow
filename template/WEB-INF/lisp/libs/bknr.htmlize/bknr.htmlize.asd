;;; -*- lisp -*-

;;;; ASDF system definition file for HTMLIZE

(asdf:defsystem :bknr.htmlize
  :components ((:file "packages")
	       (:file "reader")
	       (:file "htmlize")
  	       (:file "hyperspec")))
