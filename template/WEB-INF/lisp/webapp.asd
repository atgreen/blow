;;; ----------------------------------------------------------*- lisp -*------
;;; webapp.asd - A Sample BLOW Web Application  
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

;;;; ASDF system definition file for our webapp

(asdf:defsystem :webapp
  :description "My Demo Web Application"
  :author "Anthony Green <green@spindazzle.org>"
  :licence "BSD (sans advertising clause)"
  :version "0.0"
  :depends-on (:blow :bknr.htmlize)
  :serial t
  :components ((:file "webapp"))
  :properties ((version "0.0")))

