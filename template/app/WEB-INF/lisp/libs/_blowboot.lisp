;;; ----------------------------------------------------------*- lisp -*------
;;; _blowboot.lisp - startup file for BLOW
;;; Copyright (C) 2007, 2008, 2010 Anthony Green <green@spindazzle.org>
;;; 
;;; This file is part of BLOW - A Web Application Framework in LISP.
;;;
;;; BLOW is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; BLOW is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with BLOW; see the file COPYING.LESSER.  If not,
;;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;;; Fifth Floor, Boston, MA 02110-1301 USA.
;;;
;;; --------------------------------------------------------------------------

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :asdf))

(defparameter *blow-home-directory*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defun reread-blow-asdf-registry ()
  "Go through all dirs in the blow home and register them into the asdf:*central-registry*."
  (flet ((push-all (systems-dir)
           (dolist (dir-candidate
                     (directory (concatenate 'string (namestring systems-dir) "*")))
             ;; skip dirs starting with a _
	     (if (eq (pathname-name dir-candidate) nil) ; true only if a directory
		 (let ((name (car (last (pathname-directory dir-candidate)))))
		   (unless (equal #\_ (elt name 0))
		     (pushnew dir-candidate asdf:*central-registry* :test 'equal)))))))
    ;; here, we probably may even (setf asdf:*central-registry* nil)
    (push-all *blow-home-directory*)))

;; Tell asdf to only look in our special directories
(reread-blow-asdf-registry)
(pushnew (car 
	  (directory 
	   (concatenate 'string (namestring *blow-home-directory*) ".."))) 
	 asdf:*central-registry* :test 'equal)
(pushnew *blow-home-directory* asdf:*central-registry* :test 'equal)

(asdf:oos 'asdf:load-op :jfli)
(jfli:def-java-class "java.io.PrintWriter")
(jfli:def-java-class "javax.servlet.http.HttpServletRequest")
(jfli:def-java-class "javax.servlet.http.HttpServletResponse")
(jfli:def-java-class "org.spindazzle.BlowServlet")
(asdf:oos 'asdf:load-op :webapp)

(terpri)
(write-line "*** blow booted ***")
(terpri)

