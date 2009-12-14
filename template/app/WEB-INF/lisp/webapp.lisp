;;; ----------------------------------------------------------*- lisp -*------
;;; webapp.lisp - A Sample BLOW Web Application  
;;; Copyright (C) 2007 Anthony Green <green@spindazzle.org>
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

;; Every application must define a package called "webapp".  The
;; package declaration identifies which Lisp packages are used by the
;; web app.  At a minimum, this currently must include cl, blow and
;; cl-who.

;; The package declaration also must export a single function:
;; MAKE-USER-SESSION, which is described below.

(defpackage :webapp
  (:use :cl 
	:blow
	:cl-who
	:bknr.htmlize
	:cl-emb)
  (:export :make-user-session))

(in-package :webapp)

;; MAKE-USER-SESSION is called be the BLOW framework once per session.
;; The resulting object can be of any type.  Whatever it is, it will
;; be passed to each page generating fuction.

(defun make-user-session ()
  (random 1000))

;;; Define two page generators.  Page generators return a string
;;; containing HTML output.

(def-who-page page-hello-world-random (request response session)
  (:html (:head (:title "BLOW: Lisp On Web")
		(:link :rel "stylesheet" :type "text/css" :href "/demo/style.css"))
	 (:body
	  (:p "Hello World!  This web application blows.")
	  (:p (fmt "Session: ~A" session))
	  (:p (fmt "Random Number: ~A" (random 1000))))))

(defvar *DEFAULT-ERROR-401* #'page-hello-world-random)

(def-html-page page-htmlize-lisp-source (request response session)
  (let ((filename (concatenate 'string (namestring cl-user::*blow-home-directory*) "../../../.." 
			       (|javax.servlet.http|::httpservletrequest.getrequesturi request))))
    (with-output-to-string (out)
			   (let ((*standard-output* out))
			     (htmlize-file filename)))))

(defun page-safe-htmlize-lisp-source (request response session)
  (let ((filename (concatenate 'string (namestring cl-user::*blow-home-directory*) "../../../.." 
			       (|javax.servlet.http|::httpservletrequest.getrequesturi request))))
    (cond ((probe-file filename)
	   (page-htmlize-lisp-source request response session))
	  (t
	   (funcall *DEFAULT-ERROR-401* request response session)))))

;;; Register the three generators on the *DISPATCH-LIST* (exported
;;; from the BLOW package).

(push (create-prefix-dispatcher "/demo/hello" #'page-hello-world-random)
      *DISPATCH-LIST*)

(push (create-regex-dispatcher ".*\.lisp$" #'page-safe-htmlize-lisp-source)
      *DISPATCH-LIST*)

