;;; ----------------------------------------------------------*- lisp -*------
;;; rdf.lisp - A Web Application Framework in Lisp
;;; Copyright (C) 2007, 2008 Anthony Green <green@spindazzle.org>
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
;;; abcl-web which is distributed under LGPL license, with
;;; clarifications from LLGPL applicable.
;;;
;;; --------------------------------------------------------------------------

(in-package :blow)

(defparameter *rdf-model* (ModelFactory.createDefaultModel))

(defconstant +urn-ibash+ "urn:x-ibash:")
(defconstant +urn-ibash-prop+ "urn:x-ibash:prop:")
(defconstant +urn-ibash-obj+ "urn:x-ibash:obj:")

(defparameter *rdf-default-namespace* +urn-ibash+)
(defparameter *rdf-default-prop-namespace* +urn-ibash-prop+)
(defparameter *rdf-default-obj-namespace* +urn-ibash-obj+)

(defun rdf-resolve-prop (prop)
  "resolve prop to Jena2 prop."
  (etypecase prop
    (symbol (or (get prop :rdf-prop)
		(setf (get prop :rdf-prop)
		      (Model.createProperty *rdf-model* *rdf-default-prop-namespace*
					    (symbol-name prop)))))
    (list (Model.createProperty *rdf-model* (first prop) (second prop)))
    (java-object prop)
    (string (Model.createProperty *rdf-model* "" prop))))

(defun rdf-resolve-val (&optional v)
  "resolve a value, by default it thinks it's a literal, if it's an URL, it 
should be explicitly marked with rdf-resource"
  (typecase v
    (java-object v)
    (null (Model.createResource *rdf-model*)) ;; create blank node. null should precede symbol    
    (string (rdf-literal v))
    (symbol (Model.createResource *rdf-model*
				  (concatenate 'string *rdf-default-obj-namespace*
					       (symbol-name v))))
    (t (rdf-typed-literal v))))

(defun rdf-new-blank-object () (rdf-resolve-val))

(defun rdf-resource (v) (Model.createResource *rdf-model* v))

(defun rdf-literal (str) (Model.createLiteral *rdf-model* str))

(defun rdf-typed-literal (v) (Model.createTypedLiteral *rdf-model* v))

(defpackage rdfp)

(defun rdf-bind-property-to-symbol (symbol property)
  (setf (get prop :rdf-prop) (rdf-resolve-property property)))

(defmacro def-rdf-property (pname &optional namespace name)
  (let* ((rdfp (find-package :rdfp))
	 (sym (intern (symbol-name pname) rdfp)))
    (export sym rdfp)
    (when (and namespace name)
      (rdf-bind-property-to-symbol pname (list namespace name)))
    `(progn
       (defun ,sym (subj) (rdf-prop ,pname subj))
       (defun (setf ,sym) (value subj)
	 (setf (rdf-prop ,pname subj) value)))))

(defun rdf-prop-raw (prop subj)
  "get Jena2 object of a prop"
  (let* ((r-prop (rdf-resolve-prop prop))
	 (r-subj (rdf-resolve-val subj))
	 (stat (Model.getProperty *rdf-model* r-subj r-prop)))
    (when stat (Statement.getObject stat))))

(defconstant +rdf-literal-class+ (jclass "com.hp.hpl.jena.rdf.model.Literal"))

(defun rdf-unbox (v)
  (when v
    (if (Class.isInstance +rdf-literal-class+ v)
	(unbox (Literal.getValue v))
      v)))

(defun rdf-prop (prop subj)
  "get unboxed value of property"
  (rdf-unbox (rdf-prop-raw prop subj)))

(defun (setf rdf-prop) (value prop subj)
  (let* ((r-subj (rdf-resolve-val subj))
	 (r-prop (rdf-resolve-prop prop))
	 (r-val (rdf-resolve-val value))
	 (stat (Model.getProperty *rdf-model* r-subj r-prop)))
    (if stat 
	(Statement.changeObject stat r-val)
      (Model.add *rdf-model*
		 (Model.createStatement *rdf-model* r-subj r-prop r-val)))
    value))

(defun rdf-add-statement (subject property object)
  (Model.add *rdf-model*
	     (Model.createStatement *rdf-model*
				    (rdf-resolve-val subject)
				    (rdf-resolve-prop property)
				    (rdf-resolve-val object))))

(defconstant +jena-node-class+ (jclass "com.hp.hpl.jena.graph.Node"))
(defconstant +jena-fronts-node-class+ (jclass "com.hp.hpl.jena.graph.FrontsNode"))

(defun rdf-as-node (v)
  (cond 
   ((Class.isInstance +jena-node-class+ v) v)
   ((Class.isInstance +jena-fronts-node-class+ v) (FrontsNode.asNode v))
   (t (error "cannot convert to node %s" v))))

(defun rdf-add-query-egroup-triple-raw (egroup s p o)
  (ElementGroup.addTriplePattern
   egroup
   (Triple.new (rdf-as-node s) (rdf-as-node p) (rdf-as-node o))))
			    
(defun rdf-add-query-egroup-triple (egroup s p o)
  (rdf-add-query-egroup-triple-raw egroup
				   (rdf-resolve-val s)
				   (rdf-resolve-prop p)
				   (rdf-resolve-val o)))

(defun rdf-make-select-query (&key (query (QueryFactory.make)) (query-star t))
  (Query.setQueryType query *Query.queryTypeSelect*)
  (when query-star (Query.setQueryResultStar query t))
  query)

(defun rdf-make-query (constraints options)
  "creates a query for constraints that are kinda plist:  prop1 val1 prop2 val2"
  (let* ((query (rdf-make-select-query))
	 (egroup (ElementGroup.new))
	 (objvar (Node.createVariable "O")))
    (loop for (prop val) on constraints by #'cddr
      do (rdf-add-query-egroup-triple egroup objvar prop val))
    (when (getf options :order-by-balance)
      (let ((balance-var (Node.createVariable "B")))
	(rdf-add-query-egroup-triple egroup objvar :balance balance-var)	
	(Query.addOrderBy query balance-var -1)))
    (Query.setQueryElement query egroup)
    query))

(defun rdf-execute-query (query)
  (QueryExecution.execSelect
   (QueryExecutionFactory.create query *rdf-model*)))

(defun rdf-get-query-result (rs varname)
  (when (ResultSet.hasNext rs)
    (rdf-unbox (QuerySolution.get (ResultSet.nextSolution rs) varname))))

(defun rdf-get-query-results (rs varname)
  (loop while (ResultSet.hasNext rs)
    collect (rdf-unbox (QuerySolution.get (ResultSet.nextSolution rs) varname))))

(defun rdf-select-objects (constraints &optional options)
  (rdf-get-query-results
   (rdf-execute-query (rdf-make-query constraints options))
   "O"))

(defun rdf-make-query-for-triples (triples)
  (let ((query (rdf-make-select-query))
	(egroup (ElementGroup.new)))
    (loop for (s p o) in triples
      do (rdf-add-query-egroup-triple egroup s p o))
    (Query.setQueryElement query egroup)
    query))

(defun rdf-query-fn (tlst result-fun)
  (let* ((vars (make-hash-table))
	 (v-triples
	  (loop for tr in tlst
	    collect (loop for v in tr
		      when (and (%variable-sym-p v) (not (gethash v vars)))
		      do (setf (gethash v vars)
			       (Node.createVariable (%variable-name v)))
		      collect (or (gethash v vars) v))))
	 (query (rdf-make-query-for-triples v-triples)))
    (funcall result-fun (rdf-execute-query query) "QQQ")))

(defun %variable-name (v)
  (if (eq v '?)
      "QQQ"
    (subseq (symbol-name v) 1)))
    

(defun %variable-sym-p (v)
  (and (symbolp v) (eql (char (symbol-name v) 0) #\?)))

(defun %quote-variable (v)
  (if (%variable-sym-p v)
      (list 'quote v)
    v))

(defun quote-triples (triples)
  (loop for (s p o) in triples
    for s* = (%quote-variable s)
    for p* = (%quote-variable p)
    for o* = (%quote-variable o)
    collect `(list ,s* ,p* ,o*)))

(defun rdf-query-macro (rs-fun triples)
  `(rdf-query-fn (list ,@(quote-triples triples))
		 (function ,rs-fun)))

(defmacro rdf-query-1 (&rest triples)
  (rdf-query-macro 'rdf-get-query-result triples))

(defmacro rdf-query (&rest triples)
  (rdf-query-macro 'rdf-get-query-results triples))

(defmacro rdf-do-query ((&rest triples) &body body)
  (let ((vars (loop for (s p o) in triples
			     when (%variable-sym-p s)
			     collect s
			     when (%variable-sym-p s)
			     collect o))
	(rss (gensym))
	(sol (gensym)))
  `(rdf-query-fn 
    (list ,@(quote-triples triples))
    (function 
     (lambda (,rss ,(gensym))
       (loop while (ResultSet.hasNext ,rss)
	 for ,sol = (ResultSet.nextSolution ,rss)
	 do (let ,(loop for v in vars
		    collect
		    `(,v (rdf-unbox (QuerySolution.get ,sol ,(%variable-name v)))))
	      ,@body)))))))
