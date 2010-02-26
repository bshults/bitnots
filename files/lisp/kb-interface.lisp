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

;;; This file has more code for the interface between the prover and
;;; the knowledge base.

;;; Be sure to be careful to remove sequents once they cannot be
;;; accessed by ipr anymore.  I need a good function for doing that.

(defun kb-reset-basics ()
  (reset-kb)
  (setq *kb-setup* nil)
;;;  (setq *theorems* nil)
  (setq *unknown-predicates*
    (append *defined-predicates* *unknown-predicates*))
  (setq *defined-predicates* nil)
  (setq *unknown-terms* (append *defined-terms* *unknown-terms*))
  (setq *defined-terms* nil)
  (kb-setup-basics))

(defun tell-ipr-basics ()
  (reset-kb)
  ;; this may want to put in some standard theorems or definitions
  )

(defun reset-kb ()
  (setq *kb-predicates* nil)
  (setq *kb-terms* nil)
  (setq *kb-theorems* nil)
  )

(defun kb-setup-basics ()
  (reset-kb)
  (setq *kb-setup* t))
;;;  (when (not *kb-setup*)
;;;    (if (or *ipr-batch-mode*	; (not *interacting*)
;;;	    (get-value-from-emacs
;;;	     :function '(lambda () (y-or-n-p "Load from file? "))))
;;;	(progn (reset-kb) (load-kb "kb-setup" *kb-directory*))
;;;	(progn (tell-ipr-basics) (dump-kb "kb-setup" *kb-directory*)))
;;;    (setq *kb-setup* t)))

(defun kb-install-theorem-group (sequents name body)
  (declare (list sequents body) (symbol name))
  (mapc #'(lambda (sequent)
	    (kb-install-theorem sequent name body)) sequents))

(defun kb-install-theorem (sequent name body)
  ;; make this so that if this name has already been used then the
  ;; existing theorems by this name are removed and replaced by these.
  (declare (type proof sequent) (symbol name) (list body))
  (push (make-kb-theorem :orig-name name
			 ;; do better than this.
			 :name (gentemp (symbol-name name))
			 :string (or (cadr (assoc 'string body)) "")
			 :format (or (cadr (assoc 'format body)) "")
			 :conclusions (formulas (goals sequent))
			 :suppositions (formulas (hypotheses sequent)))
	*kb-theorems*))

(defun kb-install-pred (pred pattern definition body)
  (declare (ignore pred pattern definition body))
  ())

(defun kb-get-term-equality (term)
  ;; this returns a formula of the form (= pattern definition)
  (declare (symbol term))
  (car (member-if #'(lambda (e)
		      (declare (symbol e))
		      (eq (kb-term-name e) term)) *kb-terms*)))

(defun kb-install-term (term pattern definition body)
  (declare (ignore body) (symbol term))
  (push (make-kb-term :name term
		      :pattern pattern
		      :definition definition) *kb-terms*))

(defun kb-term-pattern-definition-lists (term)
  (declare (symbol term))
  (let ((the-term (car
		   (member-if #'(lambda (kb-term)
				  (declare (symbol kb-term))
				  (eql term (kb-term-name kb-term)))
			      *kb-terms*))))
    (declare (symbol the-term))
    (cons (kb-term-pattern the-term) (kb-term-definition the-term))))

;;;(defun kb-ratings-of-theorems (c-goals c-hyps &aux return)
;;;  ;; this returns a list of ratings which must all be lists
;;;  (declare (list c-goals c-hyps return))
;;;  (dolist (kb-thm *kb-theorems* return)
;;;    (let ((d (rate-theorems-applicability kb-thm c-goals c-hyps)))
;;;      (when (listp d)
;;;	(if (and (numberp (caddr d)) (zerop (caddr d)) (cadadr d))
;;;	    (return (push d return))
;;;	  (push d return))))))

(defmacro kb-conclusions (thm)
  `(kb-theorem-conclusions ,thm))

(defmacro kb-hypotheses (thm)
  `(kb-theorem-suppositions ,thm))

(defun show-theorem-in-lisp (theorem)
  (format t "~%Theorem: ~s.~%Hypotheses:~%~S~%Conclusions:~%~S~%"
	  (kb-theorem-name theorem)
	  (kb-theorem-suppositions theorem)
	  (kb-theorem-conclusions theorem)))

