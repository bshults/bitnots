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

;;; This file handles definitions of terms and predicates for the
;;; knowledge base.  It also handles "definitions" of theorems and
;;; axioms, etc.  Included in this file is the definition of the
;;; gamma-inverse rule.

;;; So finally we decide that the user must load those theorems
;;; manually which he wants to use.  The computer will tell him
;;; which terms and preds and undefined and when at a loss, ask
;;; whether he wants to load some more.

;;; bound-variables in definitions (and in theorems) will have to be
;;; substituted for new bound variables unique (internally) to the
;;; system.

;;; Theorem Creation:

(defmacro def-theorem (name statement &body body)
  `(def-theorem-fun ',name ',statement ',body))

(defmacro def-axiom (name statement &body body)
  `(def-theorem-fun ',name ',statement ',body))

(defun def-theorem-fun (name statement body)
  ;; Mark formulas which contain iota terms.
  (declare (symbol name) (list statement body))
  (let ((*making-theorems* t) (*alphas* nil)
	;; =s is not used anywhere
	;; (*=s* nil)
	(*betas* nil)
	(*deltas* nil) (*gammas* nil))
    (when (not *kb-setup*)
      (let ((*ipr-batch-mode* t)) (kb-setup-basics)))
    ;; what needs to happen below is that we need to make a list of
    ;; all formulas in the TRANSLATIONS list and make kb-formulas
    ;; out of them, then make theorems out of those.
    (let* ((translations (forms-of-theorem statement)))
      (some #'(lambda (e)
		(when (eq (kb-theorem-orig-name e) name)
		  (warn-user "Replacing the theorems named ~a ~
already in knowledge base." name) t)) *kb-theorems*)
;;;      (dolist (sequent translations)
;;;	;; I may need to instantiate (:internal t) here in case
;;;	;; there has been unification.  Or perhaps I should prohibit
;;;	;; rigid unification in break-down.
;;;;;;	(setf (goals sequent) (mapcar #'formula (goals sequent)))
;;;;;;	(setf (hypotheses sequent)
;;;;;;	  (mapcar #'formula (hypotheses sequent)))
;;;	;; maybe look for subsumed theorems before installation.
;;;	)
      (kb-install-theorem-group translations name body))))

;;; so we don't need to internalize bound variables in theorems or
;;; definitions because we do a safely-internalize when they are
;;; instantiated.  We do still need to be sure that any remaining
;;; bound variables in theorems are replaced by free parameters.

(defun forms-of-theorem (formula)
  ;; This returns a list of theorems each being a sequent which is a
  ;; consequent of FORMULA.
  (setq *transform-proof* (make-sequent))
  (setf (new-goals *transform-proof*)
    (mapcar #'(lambda (f) (make-formula f :birth-place *transform-proof*))
	    (reduce-d formula)))
  (break-down))

(defun break-down ()
  ;; also I want this to do more occurrence checking so that
  ;; quantifiers are removed whenever possible.  Even look past
  ;; 'implies for the possibility of pushing quantifiers in.  This
  ;; should resemble math-prove but it should delete proofs that are
  ;; done.
  (while (or *alphas* *betas* *deltas*)
    (and *alphas* (pop-all-alphas))
    (and *betas* (prove-beta))
    (and *deltas* (prove-gamma-inverse)))
  ;; could be more efficient.
  (leaves *transform-proof*))

(defun prove-gamma-inverse ()
  (while *deltas*
    (let ((copy (copy-list *deltas*)))
      (declare (list copy))
      (setq *deltas* nil)
      (mapc #'(lambda (form)
		(declare (type formula form))
		(remove-quant-step form)
		;; Why not make the rule include "side-step" or call the
		;; function by the rule?
;;;		(setf (used form) '(t))
		)
	    copy)))
  t)

;;; I do want something like nicen to get rid of sequents that are done.

(defun remove-quant-step (formula)
  ;; this resets the formula getting rid of the outermost quantifier
  ;; and replacing the bound-vars with free vars.  check-formula does
  ;; something similar but replaces free variables with constants.
  ;; I might want to rename variables.
  (setf (formula formula) (caddr (formula formula)))
  (other-formula-stuff formula)
  (when *new-alpha*
    (reducer (birth-place formula)) (setq *new-alpha* nil)))

(defmacro def-predicate ((pred . pattern) definition &body body)
  `(def-predicate-fun ',pred ',pattern ',definition ',body))

(defun def-predicate-fun (pred pattern definition body)
  ;; Mark formulas containing iota terms.
  (when (not *kb-setup*)
    (let ((*ipr-batch-mode* t)) (kb-setup-basics)))
  (let ((format (cdr (assoc 'format body))))
    (push (cons pred format) *pred-formats*))
  (when (and (not *format-only*)
	     (check-syntax :pred pred pattern definition body))
    ;; this is not putting a *thm.. variable in for those variables
    ;; already free in the definition.@%#$
    (kb-install-pred pred (cons pred pattern) definition body)
    ;; do some work to ensure that you don't just go back and forth.
    (def-theorem-fun pred `(iff ,(cons pred pattern) ,definition) 
      `((validity definition)
	   ,@body))))

(defmacro def-term ((term . pattern) definition &body body)
  `(def-term-fun ',term ',pattern ',definition ',body))

(defun def-term-fun (term pattern definition body)
  ;; Mark formulas containing iota terms.
  (when (not *kb-setup*)
    (let ((*ipr-batch-mode* t))
      (kb-setup-basics)))
  (let ((format (cdr (assoc 'format body))))
    (push (cons term format) *term-formats*))
  (when (and (not *format-only*)
	     (check-syntax :term term pattern definition body))
    (kb-install-term term (cons term pattern) definition body)
    ;; benji @%#$ put this in the theory grammar!!!
    (when (member term *unknown-terms*)
      (install-definition-of-term term))))

;;; See if safe-replacement-made-p or some such would take the place
;;; of safely-internalize.
;;;(defun definition-of-pred (formula)
;;;  (let* ((definition*
;;;	     ;; move this into kb-interface and replace with
;;;	     ;; descriptive function name this returns a list of ( (
;;;	     ;; pattern definition ) ... )
;;;	     (kb-predicate-pattern-definition-lists formula))
;;;	 (pattern (caar definition*)) (definition (cadar definition*))
;;;	 (alist (make-simple-alist pattern formula)))
;;;    (safely-internalize-formula
;;;     (instantiate-formula definition alist))))

;;;(defun definition-of-term (term)
;;;  (let* ((definition*
;;;	     ;; move this into kb-interface and replace with
;;;	     ;; descriptive function name
;;;	     (kb-term-pattern-definition-lists term))
;;;	 (pattern (caar definition*)) (definition (cadar definition*))
;;;	 (alist (make-simple-alist pattern term)))
;;;    (safely-internalize-term 
;;;     (instantiate-term definition alist))))

(defun make-simple-alist (patt1 patt2 &key (test 'eql) &aux alist)
  ;; called by expand-term-definition
  ;; this is not recursive so it is very special to the definition situation.
  "This takes two patterns PATT1 and PATT2 and makes an alist of the
differences.  The arguments of PATT1 should be tested equal by TEST.
Keys are taken from PATT1, values from PATT2.  It does not check that
the cars of PATT1 and PATT2 match.  PATT1 and PATT2 should be terms."
  (do ((key (cdr patt1) (cdr key))
       (value (cdr patt2) (cdr value)))
      ((null key) alist)
    (let ((value2 (cdr (assoc (car key) alist :test test))))
      (if value2 (when (not (term-equal value2 (car value)))
		   (error " Mismatched patterns in make-simple-alist~%PATT1: ~S
PATT2: ~s~%" patt1 patt2))
	;; otherwise add it to the alist
	(push (cons (car key) (car value)) alist)))))

;;; Currently there is something silly in this: how can we search all
;;; terms if we haven't loaded anything yet.  So I need to load a kb
;;; of file-names associated with theories, terms and predicates.
(defun install-definition-of-term (const)
  ;; %@#$ benji. these need to be called when theorems are used.
  ;; anytime a new term becomes part of the theorem, its definition
  ;; should be installed.
  (setq *unknown-terms* (delete const *unknown-terms*))
  (push const *defined-terms*)
;;;  (when (definitional-equality const)
;;;    (push (definitional-equality const) *theory-equalities*))
;;;;;;    (add-to-grammar (definitional-equality const) *theory-grammar*)
;;;;;;    (push const *unknown-terms*)
  )

(defun install-definition-of-predicate (const)
  (declare (ignore const))
  t)

