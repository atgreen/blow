;;; ----------------------------------------------------------*- lisp -*------
;;; packages.lisp - Package defintion file for BLOW
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

(in-package :cl-user)

(defpackage :blow
  (:use :cl :cl-who :cl-ppcre :cl-emb)
  (:export :create-conditional-dispatcher
	   :create-prefix-dispatcher
	   :create-regex-dispatcher
	   :def-html-page
	   :def-who-page
	   :service-http-get-request
	   :*DISPATCH-LIST*))

(pushnew :blow *features*)