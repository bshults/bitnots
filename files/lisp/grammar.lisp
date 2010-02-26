;;;    IPR: a theorem prover for mathematics
;;;    Copyright (C) 1996  Benjamin Shults
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;    See the file COPYING for the full text of the GNU General
;;;    Public License.
;;;
;;;    See the file README for information on how to contact the author.
;;;

(in-package :ipr)

;;; This file implements grammars for equality and rewriting.

(defstruct (nonterminal
	    (:print-function
	     (lambda (n s p)
	       (declare (ignore p))
	       (format s "~%[NT ~s]"
		       (node-number n))))
	    (:conc-name nil))
  (node-number 0 :type (integer 0 *))
  (prods-from nil :type list)
  (prods-to nil :type list)
  next-find 
  ;; The type is :class if there is a class-term in this partition.
;;;  (type nil :type symbol)
  ;; This is a representative term.
;;;  (term nil :type term)
  )

(defstruct (prod
	    (:print-function
	     (lambda (prod s p)
	       (declare (ignore p))
	       (format s "
~s --> ~s~@[~%~*(with conditions or bindings)~]"
		       (prod-nonterm prod) (prod-rhs prod)
		       (or (prod-bindings prod) (prod-cond prod))))))
  ;; Phrase constructor
  (p-const nil :type symbol)
  lhs					; :type nonterminal
  ;; RHS is a list of nonterminals
  (rhs nil :type list)			; right-hand-side
  htable-entry
  ;; The below were added by me.
  (bindings nil :type list)		; bindings needed
  ;; is a list of pairs of the form:
  ;;   :theorem thm
  ;;     (where thm is the thm for which the production was made)
  ;;   :involved-thm formula
  ;;     (where formula is the formula for which the production was made)
  (conds nil :type list)			; conditions
  )

;;; A grammar needs to be a hash-table type thing.  But, using
;;; hash-tables makes it hard to expand them down a branch without
;;; changing the original.  Maybe the conditions can take care of
;;; that?

(defmacro nonterms (grammar)
  `(gethash :number ,grammar))

(defun make-grammar ()
  (let ((hash (make-hash-table :test 'equal)))
    ;; :number is the number of nonterminals.
    (setf (nonterms hash) 0)))

(defun make-production (grammar &key rhs nonterm bindings conds)
  ;; This function should call make-prod and also add the production
  ;; to a hashtable.
  )

(defvar *kb-grammar* (make-grammar)
  "List of productions from the knowledge base.")

(defun intern-term (term grammar
			 &aux (nt (make-nonterm
				   :node-number
				   (incf (nonterms grammar))))
			 prod)
  ;; This is all wrong?  I need to see if the production already
  ;; exists.
  (if (consp term)
      (if (and *hol* (eq (car term) 'the-class-of-all))
	  (let ((prod (make-production grammar :nonterm nt
				       :p-const
				       'the-class-of-all)))
	    (setf (term-type nt) :class)
	    (push prod (prods-to nt)))
	(let* ((rhs (mapcar #'intern-term (cdr term)))
	       (prod (make-production grammar :rhs rhs :nonterm nt
				      :p-const (car term))))
	  (push prod (prods-to nt))))
    (let* ((nt (make-nonterm :node-number (incf (nonterms grammar))))
	   (prod (make-production grammar :nonterm nt
				  :p-const term)))
      (setf (term-type nt) :variable)
      (push prod (prods-to nt))))
  nt)

;;; Plans:

;;; For KB.

;;; Create a KB grammar from equalities in the conclusions of theorems
;;; (including term definitions.)  These productions will have
;;; conditions saying that the other formulas in the theorem must be
;;; used as appropriate.

;;; For class terms.

;;; When one side of an equality is a class term, a new nonterminal
;;; should be made for that class.  That nonterminal should contain
;;; the information that it is a class.

(defun make-kb-grammar (&aux prod)
  ;; Find the theorems which have = in the conclusions.
  (dolist (thm *kb-theorems*)
    (let (thm=s)
      ;; For each of the = in the conclusions...
      (dolist (= (dolist (conc (conclusions thm) thm=s)
		   (when (eq '= (car conc))
		     (push conc thm=s))))
	;; create a production with the most general conditions
	;; possible and push the production into *kb-grammar*.
	;; 
	;; Make nonterminals and productions for any class terms and
	;; put them into the kb-grammar.  So, in this sense, class
	;; terms are atomic.
	(cond ((eq 'the-class-of-all (caadr conc))
	       (push (make-production
		      *kb-grammar*
		      :rhs (cadr conc)
		      :nonterm (make-nonterminal-for (cadr conc))
		      :conds (list :theorem thm :involved-thm conc))
		     *kb-grammar*)
	       (when (eq 'the-class-of-all (caaddr conc))
		 (push (make-production
			*kb-grammar*
			:rhs (caddr conc)
			:nonterm (make-nonterminal-for (caddr conc))
			:conds (list :theorem thm :involved-thm conc))
		       *kb-grammar*)))
	      ((eq 'the-class-of-all (caaddr conc))
	       (push (make-production
		      *kb-grammar*
		      :rhs (caddr conc)
		      :nonterm (make-nonterminal-for (caddr conc))
		      :conds (list :theorem thm :involved-thm conc))
		     *kb-grammar*)))
	;; If there is a class term, make it the nonterminal of the
	;; main nonterminal being added.
	;; 
	;; 
	;; 
	;; 
	;; 
	(push (make-prod :rhs 
			 :nonterm 
			 ;; the conds must include a mention of the
			 ;; kb-theorem
			 :conds (list :theorem thm)
			 )
	      *kb-grammar*)))))

;;; Make equality substitutions:
;;;  Go through equality formulas on branch
;;;   see if both sides are congruent to a class
;;;   if so, then make the substitutions and apply extensionality.
;;;  Go through the member formulas on branch
;;;   see if the rhs is congruent to a class.
;;;   if so, then make the substitution and apply comprehension.
;;; 
;;; See if a term is congruent to a class.
;;;  What to do about variables?
;;;  The problem is that (f x (a)) and (f (b) (a)) will not give me
;;;  the same nonterminal because the nonterminals for x and (b) are
;;;  not the same.
;;;  I could have a single nt for variables so that they all match but
;;;  this would not solve the problem.
;;;  I would hate to march through all of the nonterminals in the
;;;  grammar to find one 
;;; 
;;; Maybe I only need to use single-step equalities.  Say, I only make
;;; a substitution if there is a single equality which does what I
;;; need.  Yeah.
;;; 
;;; 
;;; 
;;; 
;;; 

