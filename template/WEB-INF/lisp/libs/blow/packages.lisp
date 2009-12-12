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

(defpackage :blow-gen          
  (:use :common-lisp :jfli))

(in-package :blow-gen)

(def-java-class "com.hp.hpl.jena.rdf.model.Model")
(def-java-class "com.hp.hpl.jena.rdf.model.ModelFactory")
(def-java-class "com.hp.hpl.jena.rdf.model.Statement")
(def-java-class "com.hp.hpl.jena.rdf.model.Property")
(def-java-class "com.hp.hpl.jena.rdf.model.Resource")
(def-java-class "com.hp.hpl.jena.rdf.model.StmtIterator")
(def-java-class "com.hp.hpl.jena.rdf.model.RDFNode")
(def-java-class "com.hp.hpl.jena.rdf.model.Literal")
(def-java-class "com.hp.hpl.jena.graph.Triple")
(def-java-class "com.hp.hpl.jena.graph.Node")
(def-java-class "com.hp.hpl.jena.graph.FrontsNode")

;;Jena-ARQ
(def-java-class "com.hp.hpl.jena.query.Query")
(def-java-class "com.hp.hpl.jena.query.QueryFactory")
(def-java-class "com.hp.hpl.jena.query.QueryExecutionFactory")
(def-java-class "com.hp.hpl.jena.query.QueryExecution")
(def-java-class "com.hp.hpl.jena.query.ResultSet")
(def-java-class "com.hp.hpl.jena.query.QuerySolution")

(def-java-class "com.hp.hpl.jena.sparql.syntax.ElementGroup")

(defpackage :blow
  (:use :cl :cl-who :cl-ppcre :cl-emb :jfli :java
	"com.hp.hpl.jena.rdf.model"
	"com.hp.hpl.jena.graph"
	"com.hp.hpl.jena.query"
	"com.hp.hpl.jena.sparql.syntax")
  (:export :create-conditional-dispatcher
	   :create-prefix-dispatcher
	   :create-regex-dispatcher
	   :def-html-page
	   :def-who-page
	   :service-http-get-request
	   :*DISPATCH-LIST*))

(pushnew :blow *features*)