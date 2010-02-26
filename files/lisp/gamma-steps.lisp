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



;;; This file implements the first-order logic gamma-rules.  It does

;;; something unusual in that it splices the new formula as high as

;;; possible.



(defun tableaux-gamma-rule (proof)

  ;; Called by PROVE-GAMMA.

  ;; Why not intersect *gammas* with hyps and goals.

  (declare (type proof proof))

  (let* ((least (least-finder (remove-if-not

			       #'(lambda (f) (and (eq 'show-there-is (rule f))

						  (eligible? f)))

			       (append (hypotheses proof) (goals proof))))))

    (declare (type (or null formula) least))

    ;; If none of these are gammas, then 1> if (and *theory* *kb-theorems*)

    ;; go on to math [I like this.] or 2> go to some sequent which has a

    ;; gamma-formula.

    (when least

      (incf (copies least))

      (show-there-is9 least)

      (when (and *equality* *new-equals*)

	(new-equality-formula (birth-place least)) (setq *new-equals* nil))

      t)))



(defun sequent-gamma-rule-here (proof)

  ;; Called by PROVE-GAMMA.

  ;; Why not intersect *gammas* with hyps and goals.

  (declare (type proof proof))

  (let* ((least (least-finder (remove-if-not

			       #'(lambda (f) (and (eq 'show-there-is (rule f))

						  (eligible? f)))

			       (append (hypotheses proof) (goals proof))))))

    (declare (type (or null formula) least))

    ;; If none of these are gammas, then 1> go on to math [I like

    ;; this.] or 2> go to some sequent which has a gamma-formula.

    (when least

      (setq *high-proof* proof)

      ;; Maybe pop it out of *now-gammas* if this makes it too high.

      ;; I.e. keep track of eligible gammas separately from gammas.

      (incf (copies least))

      (show-there-is9 least))))



(defun eligible? (obj)

  ;; Is it permissible to remove the quantifier?  Might check reject

  ;; stuff in the future.

  (declare (type formula obj))

;;;  (< (copies obj) (* *copy-max* *copy-max*))

  (< (copies obj) *copy-max*))



(defun least-finder (list)

  (declare (list list))

  (when list

    (dotimes (num (* *copy-max* *copy-max*))

      (let ((return (some #'(lambda (elt) (declare (type formula elt))

				    (and (= (copies elt) num) elt)) list)))

	(declare (type (or null formula) return))

	(when return (return return))))))



;;; I thought about having *copy-max* provides both minumum and

;;; maximum of the possible values of copies.  But I want the max to

;;; be the square of *copy-max* so that some formulas can lag behind

;;; while others are used more frequently.



;;; So if the user thinks that a particular formula should be expanded

;;; more times than other formulas then he can just decrease the value

;;; of (copies obj-formula).  I would also like to be able to pick

;;; only certain variables in a formula for extra expansion.  Make a

;;; command for this.



;;; Show-there-is    gamma-rule



(defun show-there-is9 (formula &aux (proof (birth-place formula)))

  (let ((junct (copy-list (formula formula)))

	(declarations (cadr (formula formula)))

	;; The new param could be made up here or I could make a

	;; different one for each branch.  The former is faster in

	;; applying the step.  The latter allows more unifiers.  This

	;; is a place where a constant like *make-lots-of-variables*

	;; would make a lot of difference.  I think that it makes

	;; little difference in the number of compatible

	;; substitutions.  If the variables are all the same, the

	;; init-subs are still created.

	(param (make-parameter

		:ext-name

		(intern

		 (subseq (symbol-name

			  (ext-name (caar (cadr (formula formula))))) 1))

		:var (caar (cadr (formula formula))))))

    (setf (caddr junct) (replace-unbound-occurrences-in-formula

			 param (caar declarations) (caddr junct)))

    (if (cdr declarations) (setf (cadr junct) (cdr declarations))

      (setq junct (caddr junct)))

    (if (sign formula)

	(let* ((new-junct

		(make-formula-from-old junct formula :universals

				       (cons param (universals formula))))

	       (sub (make-sequent

		     :new-hyps (list new-junct) :plan (plan proof)

		     :subproofs (subproofs proof) :superproof proof

		     :whither (whither proof) :depth (1+ (depth proof)))))

	  (set-splits proof sub)

	  (setf (birth-place new-junct) sub)

	  (setf (whither proof) :c)

	  (setf (plan proof)

	  `((show-there-is ,(externalize-formula (formula formula))

			   hyp ,(ext-name param))))

	  (when *new-alpha* (reduce-formula sub) (setq *new-alpha* nil))

	  (let ((reps (replacements new-junct)))

	    (if reps (mapc #'(lambda (pr) (setf (superproof pr) sub)

			       (map-proof

				#'(lambda (p) (incf (depth p))

				    (when (options p)

				      (setf (new-hyps-above p)

					(append (new-hyps-above p) reps))))

				pr)) (subproofs sub))

	      (mapc #'(lambda (pr) (setf (superproof pr) sub)

			(map-proof

			 #'(lambda (p) (incf (depth p))

			     (when (options p)

			       (push new-junct (new-hyps-above p)))) pr))

		    (subproofs sub))))

	  (setf (subproofs proof) (list sub)))

      (let* ((new-junct

	      (make-formula-from-old junct formula :universals

				     (cons param (universals formula))))

	     (sub (make-sequent

		   :new-goals (list new-junct) :plan (plan proof)

		   :whither (whither proof) :subproofs (subproofs proof)

		   :superproof proof :depth (1+ (depth proof)))))

	(set-splits proof sub)

	(setf (birth-place new-junct) sub)

	(leaves proof)

	(setf (whither proof) :c)

	(setf (plan proof)

	  `((show-there-is ,(externalize-formula (formula formula))

			   goal ,(ext-name param))))

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

	(setf (subproofs proof) (list sub)))))

  t)



