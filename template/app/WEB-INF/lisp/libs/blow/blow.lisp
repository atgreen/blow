;;; ----------------------------------------------------------*- lisp -*------
;;; blow.lisp - A Web Application Framework in Lisp
;;; Copyright (C) 2007, 2008, 2009 Anthony Green <green@spindazzle.org>
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
;;; Some fragments of code identified below as being copied from
;;; hunchentoot are distributed under the terms described in
;;; COPYING.hunchentoot.
;;;
;;; --------------------------------------------------------------------------

(in-package :blow)

;;; --------------------------------------------------------------------------
;;; We need to map file extensions to mime types in order to serve up
;;; static content properly.  Let's load a hashtable up from the
;;; standard mime.types file (a public domain text file copied from
;;; the mailcap package on Fedora 12).
;;;
;;; This section of code was lifted from 
;;;   http://clocc.sourceforge.net/clocc/src/donc/http.lisp
;;; which is Copyright (c) 2000 CS3, All rights reserved.  And
;;; distrubuted under the terms of the GNU Lesser General Public
;;; License (LGPL) which can be found at
;;;   http://www.gnu.org/copyleft/lesser.html
;;; as clarified by the Franz AllegroServe Prequel which can be found at
;;; http://AllegroServe.sourceforge.net/license-allegroserve.txt 
;;; with the obvious substitutions:
;;; - replace "Franz Inc." with "CS3"
;;; - replace references to "AllegroServe" with "this program" or a name
;;;   for this program.
;;; --------------------------------------------------------------------------

(defvar *mime-types* (make-hash-table :test 'equal))
;; in case there's no mime types file we'll put in a few common cases
(setf (gethash "html" *mime-types*) "text/html"
      (gethash "htm" *mime-types*) "text/html"
      (gethash "jpeg" *mime-types*) "image/jpeg"
      (gethash "jpg" *mime-types*) "image/jpeg"
      (gethash "gif" *mime-types*) "image/gif")

(defvar *mime-type-file* 
  (concatenate 'string (namestring cl-user::*blow-home-directory*) 
	       "blow/mime.types"))

(defun whitespace-p (x) (member x '(#\tab #\space)))
(defun read-mime-types ()
  (with-open-file (f *mime-type-file*)
    (let (line type pos1 pos2)
      (loop while (setf line (read-line f nil nil)) do
	   (setf pos1 (position-if-not 'whitespace-p line))
	   (when (and pos1 (not (eql #\# (char line pos1))))
	     (setf pos2 (position-if 'whitespace-p line :start pos1))
	     (setf type (subseq line pos1 pos2)) ;; nil ok
	     (loop while (and pos2
			      (setf pos1 (position-if-not
					  'whitespace-p line :start pos2)))
		do (setf pos2 (position-if
			       'whitespace-p line :start pos1))
		  (setf (gethash (string-downcase
				  (subseq line pos1 pos2))
				 *mime-types*)
			(string-downcase type))))))))
(read-mime-types)


;;; --------------------------------------------------------------------------
;;; Macros for the various page types
;;; --------------------------------------------------------------------------

(defmacro def-who-page (name parms &body body)
  "Returns a string representing html content computed by BODY."
  (let ((response (cadr parms))
	(response-sym (gensym "A"))
	(writer-sym (gensym "B")))
    `(defun ,name (,@parms) 
       (let ((,response-sym ,response))
	 (|javax.servlet.http|::httpservletresponse.setcontenttype ,response-sym "text/html")
	 (let ((,writer-sym (|javax.servlet.http|::httpservletresponse.getwriter ,response-sym)))
	   (|java.io|::printwriter.println ,writer-sym
	    (with-html-output-to-string (*standard-output* nil :prologue t)
					,@body))
	   (|java.io|::printwriter.close ,writer-sym))))))

(defmacro def-html-page (name parms &body body)
  "Returns a string representing html content computed by BODY."
  (let ((response (cadr parms))
	(response-sym (gensym "C"))
	(writer-sym (gensym "D")))
    `(defun ,name (,@parms) 
       (let ((,response-sym ,response))
	 (|javax.servlet.http|::httpservletresponse.setcontenttype ,response-sym "text/html")
	 (let ((,writer-sym (|javax.servlet.http|::httpservletresponse.getwriter ,response-sym)))
	   (|java.io|::printwriter.println ,writer-sym (progn ,@body))
	   (|java.io|::printwriter.close ,writer-sym))))))

;;; --------------------------------------------------------------------------
;;; Handler to serve static content.  Gets mime type from filename
;;; extension.
;;; --------------------------------------------------------------------------

(defun page-serve-static-content (request response session)
  (let ((output-stream (|javax.servlet.http|::httpservletresponse.getoutputstream response))
	(filename (concatenate 'string (namestring cl-user::*blow-home-directory*) "../../.." 
			       (|javax.servlet.http|::httpservletrequest.getrequesturi request))))
    ; Strip off the filename extension so we can set the proper
    ; mime-type.  Default to text/plain.
    (|javax.servlet.http|::httpservletresponse.setcontenttype 
			 response
			 (gethash (string-downcase (pathname-type filename)) 
				  *mime-types* "text/plain"))
    (|org.spindazzle|::blowservlet.servestaticcontent filename output-stream)))

;;; --------------------------------------------------------------------------
;;; The default 404 Error Page.
;;; --------------------------------------------------------------------------

(def-who-page page-default-page (request response session)
  (:html (:head (:title "BLOW: Lisp On Web"))
	 (:body
	  (:p "TODO: This is supposed to be a 404 Error page."))))

(defmacro request-get-uri (request)
  `(|javax.servlet.http|::httpservletrequest.getrequesturi ,request))

;;; --------------------------------------------------------------------------
;;; Service HTTP requests.
;;; --------------------------------------------------------------------------

(defun service-http-get-request (request response session)
  "Dispatches *REQUEST* based upon rules in the DISPATCH-TABLE.
This method provides the default Hunchentoot behavior."
  (loop for dispatcher in *DISPATCH-LIST*
	for action = (progn (print dispatcher) (funcall dispatcher request response session))
	when action return (progn (print dispatcher) (funcall action request response session))
	finally (page-default-page request response session)))

;;; --------------------------------------------------------------------------
;;; Functions to create various kinds of page dipatchers.
;;; --------------------------------------------------------------------------

;; Copied from hunchentoot.
(defun create-prefix-dispatcher (prefix page-function)
  "Creates a dispatch function which will dispatch to the
function denoted by PAGE-FUNCTION if the file name of the current
request starts with the string PREFIX."
  (lambda (request response session)
    (let ((path (request-get-uri request)))
      (let ((mismatch (mismatch path prefix
				:test #'char=)))
	(and (or (null mismatch)
		 (>= mismatch (length prefix)))
	     page-function)))))

;; Copied from hunchentoot.
(defun create-regex-dispatcher (regex page-function)
  "Creates a dispatch function which will dispatch to the
function denoted by PAGE-FUNCTION if the file name of the current
request matches the CL-PPCRE regular expression REGEX."
  (let ((scanner (create-scanner regex)))
    (lambda (request response session)
      (let ((path (request-get-uri request)))
	(and (scan scanner path)
	     page-function)))))

(defun create-conditional-dispatcher (condition page-function)
  (lambda (request response session)
    (and (funcall condition request response session)
	 page-function)))

(def-html-page page-process-template (request response session)
  (execute-emb 
   (pathname (concatenate 'string (namestring cl-user::*blow-home-directory*) "../../.." 
			  (|javax.servlet.http|::httpservletrequest.getrequesturi request)))))

;;; --------------------------------------------------------------------------
;;; *DISPATCH-LIST* is a list of dispatch handlers.  They are processed in 
;;; order on every request until we have a match. 
;;;
;;; Applications should (push ...) their own dispatchers for RESTful 
;;; processing.
;;; --------------------------------------------------------------------------

(defvar *DISPATCH-LIST* 

  (list 
   ; Handle .blo files.  These are essentially .html files with embedded lisp.
   (create-conditional-dispatcher 
    #'(lambda (request response session)
	(let ((fname (concatenate 'string (namestring cl-user::*blow-home-directory*) "../../.." 
				  (|javax.servlet.http|::httpservletrequest.getrequesturi request))))
	  (and fname 
	       (let ((fname-length (length fname)))
		 (string= ".blo" fname :start2 (- fname-length 4) :end2 fname-length))
		  (probe-file fname))))
    #'page-process-template)

   ; Handle static content.  Just push the bits out to the user.
   (create-conditional-dispatcher
    #'(lambda (request response session)
	(let ((fname (concatenate 'string (namestring cl-user::*blow-home-directory*) "../../.." 
				  (|javax.servlet.http|::httpservletrequest.getrequesturi request))))
	  (and fname (probe-file fname))))
    #'page-serve-static-content)

   ; This is the 404 handler.
   (create-prefix-dispatcher "/" #'page-default-page)))

