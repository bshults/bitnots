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

;;; This file implements the first-order logic delta-rule.  It also
;;; splices formulas as high in the tree as possible.

;;; Consider   delta-rule

(defun consider (formula)
  (declare (type formula formula))
  (consider9 formula))

(defun consider9 (formula &aux (proof (birth-place formula)))
  ;; This could all be made more efficient by collecting the new
  ;; formulas then copying them into each of the leaves.
  (if (sign formula)
      (let ((junct (copy-list (formula formula)))
	    (declarations (cadr (formula formula)))
	    ;; The new skolem function symbol could be made up here or
	    ;; I could make a different one for each branch.  The
	    ;; former is faster in applying the step.  I don't see any
	    ;; immediate advantage to the latter.
	    ;; 
	    ;; Benji!  Don't creat the external name of the skolem
	    ;; function until it needs to be output.
	    ;; 
	    ;; Instead of make-skolem-funtion-symbol here, call
	    ;; get-skolem-function-sybol and send it the formula.  Let
	    ;; get-skolem-function-sybol check to see if the formula
	    ;; has already a skolem function associated with it.  If
	    ;; so, then just use that one.
	    ;; 
	    ;; Maybe even associate a skolem function symbol to a
	    ;; delta formula when the delta formula is created.  Also
	    ;; associate the new formula with the skolem function in
	    ;; place?  Check with the delta++ paper for details.
	    ;; 
	    ;; Ok.  The rule is that they should be equal up to
	    ;; variable renaming.  This includes bound and free
	    ;; variables.  So I could use a hash table if I first
	    ;; translated each formula into some variable-normal form,
	    ;; replacing the first variable with _v1, the second with
	    ;; _v2, etc.  Then use equal-hash-table with these
	    ;; formulas as keys.
	    (term (cons (get-skolem-function-symbol (formula formula))
			(unbound-vars-in-formula (formula formula))))
;;;	    (term (cons (make-skolem-function-symbol
;;;			 :ext-name (ext-name (caar (cadr (formula formula)))))
;;;			(unbound-vars-in-formula (formula formula))))
	    )
	(setf (caddr junct) (replace-unbound-occurrences-in-formula
			     term (caar declarations) (caddr junct)))
	(if (cdr declarations) (setf (cadr junct) (cdr declarations))
	  (setq junct (caddr junct)))
	(let ((new-junct (make-formula-from-old junct formula)))
	  (let ((sub (make-sequent
		      :new-hyps (list new-junct) :subproofs
		      (subproofs proof) :plan (plan proof) :whither
		      (whither proof) :superproof proof :depth
		      (1+ (depth proof)))))
	    (set-splits proof sub)
;;;	    (when (split proof)
;;;	      (setf (split sub) (split proof))
;;;	      (setf (split-node (split proof)) sub)
;;;	      (setf (split proof) nil))
	    (setf (birth-place new-junct) sub)
	    (setf (whither proof) :d)
	    (push sub (used formula))
	    (setf (plan proof)
	      `((consider ,(externalize-formula (formula formula))
			  hyp ,(externalize-term term))))
	    (when *new-alpha* (reduce-formula sub) (setq *new-alpha* nil))
	    (let ((reps (replacements new-junct)))
	      (if reps
		  (mapc #'(lambda (pr)
			    (setf (superproof pr) sub)
			    (map-proof
			     ;; I could move this condition outside the next
			     ;; iteration.
			     #'(lambda (p) (incf (depth p))
				 (when (options p)
				   (setf (new-hyps-above p)
				     (append (new-hyps-above p) reps)))) pr))
			(subproofs sub))
		(mapc #'(lambda (pr)
			  (setf (superproof pr) sub)
			  (map-proof
			   #'(lambda (p) (incf (depth p))
			       (when (options p)
				 (push new-junct (new-hyps-above p)))) pr))
		      (subproofs sub))))
	    (setf (subproofs proof) (list sub)))))
    (let ((junct (copy-list (formula formula)))
	  (declarations (cadr (formula formula)))
	  ;; The new skolem function symbol could be made up here or
	  ;; I could make a different one for each branch.  The
	  ;; former is faster in applying the step.  I don't see any
	  ;; immediate advantage to the latter.
	  (term (cons (get-skolem-function-symbol (formula formula))
		      (unbound-vars-in-formula (formula formula)))))
;;;	  (term (cons (make-skolem-function-symbol
;;;		       :ext-name (ext-name (caar (cadr (formula formula)))))
;;;		      (unbound-vars-in-formula (formula formula))))
      (setf (caddr junct) (replace-unbound-occurrences-in-formula
			   term (caar declarations) (caddr junct)))
      (if (cdr declarations) (setf (cadr junct) (cdr declarations))
	(setq junct (caddr junct)))
      (let ((new-junct (make-formula-from-old junct formula)))
	(let ((sub (make-sequent
		    :new-goals (list new-junct) :plan (plan proof) :whither
		    (whither proof) :subproofs (subproofs proof) :superproof
		    proof :depth (1+ (depth proof)))))
	  (setf (birth-place new-junct) sub)
	  (set-splits proof sub)
;;;	  (when (split proof)
;;;	    (setf (split sub) (split proof))
;;;	    (setf (split-node (split proof)) sub)
;;;	    (setf (split proof) nil))
	  (push sub (used formula))
	  (setf (whither proof) :d)
	  (setf (plan proof)
	    `((consider ,(externalize-formula (formula formula))
			goal ,(externalize-term term))))
	  (when *new-alpha* (reduce-formula sub) (setq *new-alpha* nil))
	  (let ((reps (replacements new-junct)))
	    (if reps
		(mapc #'(lambda (pr)
			  (setf (superproof pr) sub)
			  (map-proof
			   #'(lambda (p) (incf (depth p))
			       (when (options p)
				 (setf (new-goals-above p)
				   (append (new-goals-above p) reps)))) pr))
		      (subproofs sub))
	      (mapc #'(lambda (pr)
			(setf (superproof pr) sub)
			(map-proof
			 #'(lambda (p) (incf (depth p))
			     (when (options p)
			       (push new-junct (new-goals-above p)))) pr))
		    (subproofs sub))))
	  (setf (subproofs proof) (list sub))))))
  t)

