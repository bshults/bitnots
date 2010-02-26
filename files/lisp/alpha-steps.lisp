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



;;; This file implements the ordinary first-order logic alpha-rules.

;;; There are a few interesting things about how it is done.  For

;;; instance, the new formula is spliced into the tree as high as

;;; possible.



;;; Promote   alpha-rule



(defun promote (formula)

  (declare (type formula formula))

  (promote9 formula))



(defun promote9 (formula &aux (proof (birth-place formula)))

  (let ((new-formula1 (make-formula-from-old (caddr (formula formula))

					     formula))

	(new-formula2 (make-formula-from-old (cadr (formula formula)) formula

					     :sign t)))

    ;; Need to put the info from the previous subproofs of proof in this.

    (let ((sub (make-sequent

		:new-hyps (list new-formula2)

		:new-goals (list new-formula1) :superproof proof

		:depth (1+ (depth proof)) :plan (plan proof)

		:whither (whither proof) :subproofs (subproofs proof))))

      (setf (birth-place new-formula1) sub

            (birth-place new-formula2) sub)

      (setf (plan proof) `((promote ,(externalize-formula (formula formula)))))

      (set-splits proof sub)

      (push sub (used formula))

      (setf (whither proof) :a)

      (when *new-alpha*

        (reduce-formula sub)

        (setq *new-alpha* nil))

      (mapc #'(lambda (pr)

                (setf (superproof pr) sub)

		(map-proof

		 ;; If reducer has made replacements, then put the

		 ;; replacements into juncts-above instead of

		 ;; new-formulas.  Don't forget to (INCF (DEPTH P)).

		 (let ((rep1 (replacements new-formula1))

		       (rep2 (replacements new-formula2)))

		   (if rep1 (if rep2

				#'(lambda (p)

				    (when (options p)

				      (setf (new-goals-above p)

					(append (new-goals-above p) rep1))

				      (setf (new-hyps-above p)

					(append (new-goals-above p) rep2)))

				    (incf (depth p)))

			      #'(lambda (p)

				  (when (options p)

				    (setf (new-goals-above p)

				      (append (new-goals-above p) rep1))

				    (push new-formula2 (new-hyps-above p)))

				  (incf (depth p))))

		     (if rep2 #'(lambda (p)

				  (when (options p)

				    (push new-formula1 (new-goals-above p))

				    (setf (new-hyps-above p)

				      (append (new-hyps-above p) rep2)))

				  (incf (depth p)))

		       #'(lambda (p)

			   (when (options p)

			     (push new-formula1 (new-goals-above p))

			     (push new-formula2 (new-hyps-above p)))

			   (incf (depth p))))))

                 pr))

	    (subproofs sub))

      (setf (subproofs proof) (list sub)))))



;;; Flip   alpha-rule



(defun flip (formula)

  (flip9 formula))



(defun flip9 (formula &aux (proof (birth-place formula)) (neg t))

  ;; It seems that it won't do much good to do this as high as

  ;; possible as long as alpha rules are always applied before any

  ;; beta or theorem application rules.  Ah, but you can't guarantee

  ;; that because theorems can be applied later, introducing alpha

  ;; formula up high.

  (let ((not-flip (make-formula-from-old

		   (if neg (cadr (formula formula))

		     `(not ,(formula formula)))

		   formula :sign (not (sign formula)))))

    (let ((sub (make-sequent

		:new-hyps (and (not (sign formula)) (list not-flip))

		:new-goals (and (sign formula) (list not-flip))

		:superproof proof :plan (plan proof)

		:subproofs (subproofs proof) :whither (whither proof)

		:depth (1+ (depth proof)))))

      (setf (birth-place not-flip) sub)

      (set-splits proof sub)

      (push sub (used formula))

      (setf (whither proof) :a)

      (setf (plan proof) `((flip ,(externalize-formula (formula formula)))))

      (when *new-alpha* (reduce-formula sub) (setq *new-alpha* nil))

      (mapc #'(lambda (pr)

		(setf (superproof pr) sub)

		(map-proof

		 ;; If reducer has made replacements, then put the

		 ;; replacements into juncts-above instead of

		 ;; new-formulas.  Don't forget to (INCF (DEPTH P)).

		 (let ((reps (copy-list (replacements not-flip))))

		   (if reps

		       (if (sign not-flip)

			   #'(lambda (p) (when (options p)

					   (setf (new-hyps-above p)

					     (append (new-hyps-above p) reps)))

			       (incf (depth p)))

			 #'(lambda (p) (when (options p)

					 (setf (new-goals-above p)

					   (append (new-goals-above p) reps)))

			     (incf (depth p))))

		     (if (sign not-flip)

			 #'(lambda (p) (when (options p)

					 (push not-flip (new-hyps-above p)))

			     (incf (depth p)))

		       #'(lambda (p) (when (options p)

				       (push not-flip (new-goals-above p)))

			   (incf (depth p)))))) pr))

	    (subproofs sub))

      (setf (subproofs proof) (list sub)))))



