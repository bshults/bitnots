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



;;; This file implements the ordinary first-order logic beta-rules.  I

;;; have a rule for if-then-else implemented which only branches two

;;; ways.  Also, I experimented with splicing splits as high as

;;; possible depending on the free variables.  I don't think I ever

;;; got it to work properly.



;;; And-split    beta-rule



(defun and-split (formula)

  (declare (type formula formula))

  (if *high-splitting*			; high-splitting is not tested.

					; so make it nil.

      (let ((vars (variables-involved-in-theorem-split

		   (cdr (formula formula)))))

	;; What's the rational here?

	(if vars (mapc #'(lambda (pf) (and-split9 pf formula vars))

		       (leaves (birth-place formula)))

	  (and-split-high formula)))

    (mapc #'(lambda (pf) (and-split9 pf formula nil))

	  (leaves (birth-place formula)))))



(defun and-split9 (proof formula vars)

  (let* ((incnum 0)

	 (cjuncts (mapcar

		   #'(lambda (form) (incf incnum)

		       (make-formula-from-old form formula :universals nil))

		   (if *renaming-universals*

		       (cdr (rename-universal-variables-in-formula

			     (formula formula)))

		     (cdr (formula formula)))))

	 subs)

    (setf (split proof) (make-split :node proof :vars vars))

    (mapc #'(lambda (cjunct)

	      (let ((sub (make-sequent

			  :new-goals (list cjunct) :superproof proof

			  :depth (1+ (depth proof)) :index incnum)))

		(push sub subs)

		(push sub (used formula))

		(push (split proof) (splits cjunct))

		(setf (birth-place cjunct) sub)

		(decf incnum)))

	  (reverse cjuncts))

    (setf (whither proof) :b)

    (setf (plan proof) `((and-split ,(externalize-formula (formula formula)))))

    (setf (subproofs proof) subs)))



(defun and-split-high (formula &aux (proof (birth-place formula)))

;;; Here is an example which shows a problem with this code.

;;;(iff (implies (and (p) (implies (q) (r)))

;;;       (s))

;;;     (and (or (not (p)) (q) (s))

;;;	  (or (not (p)) (not (r)) (s))))

  (let* ((incnum 0)

	 (cjuncts (mapcar

		   #'(lambda (form) (incf incnum)

		       (make-formula-from-old form formula :universals nil))

		   (if *renaming-universals*

		       (cdr (rename-universal-variables-in-formula

			     (formula formula)))

		     (cdr (formula formula)))))

	 (h-proofs (leaves-to-var-split proof)))

    (mapc #'(lambda (prf &aux

			 ;; One problem with this is that the cjuncts

			 ;; have been pushed into *alphas*, *deltas*,

			 ;; etc.  Why am I copying these formulas

			 ;; anyway?

			 (rcjs (nreverse

				(mapcar

				 #'(lambda (f)

				     (if (eq prf (car h-proofs)) f

				       (let ((cop (copy-formula f)))

					 (case (rule f)

					   ((iff back-chain

						 if or-split and-split)

;;;					    (setq *betas* (delete f *betas*))

					    (push cop *betas*))

					   ((flip promote)

;;; I commented this stuff out not knowing what I was doing.

;;; I haven't tested if it should be there or not.

;;;					    (setq *alphas* (delete f *alphas*))

					    (push cop *alphas*))

					   (show-there-is

;;;					    (setq *gammas* (delete f *gammas*))

					    (push cop *gammas*))

					   (consider

;;;					    (setq *deltas* (delete f *deltas*))

					    (push cop *deltas*)))

					 (car (setf (step-args cop)

						(list cop))))))

				 cjuncts)))

			 (inc incnum)

			 ;; Make incnum-1 copies of the tree under

			 ;; each h-proof.

			 (copies

			  (let (cops)

			    (dotimes (incer (1- inc)

					    (cons (cons (split prf)

							(subproofs prf)) cops))

			      (push (copy-subproofs prf) cops))))

			 subs (split (make-split :node prf)))

	      ;; construct the new nodes.

	      (mapc #'(lambda (cjunct copy)

			(let ((sub (make-sequent

				    :new-goals (list cjunct) :superproof prf

				    :subproofs (cdr copy)

				    :depth (1+ (depth prf))

				    :whither (whither prf) :plan (plan prf)

				    :index inc)))

			  (push sub subs)

			  (push sub (used formula))

			  (and (car copy) (setf (split sub) (car copy)

						(split-node (car copy)) sub))

			  (setf (birth-place cjunct) sub)

			  (push split (splits cjunct))

			  (when *new-alpha*

			    (reduce-formula sub)

			    (setq *new-alpha* nil))

			  (let ((reps (replacements cjunct)))

			    (if reps

				(mapc

				 #'(lambda (pr) (setf (superproof pr) sub)

				     (map-proof

				      #'(lambda (p)

					  (when (options p)

					    (setf (new-goals-above p)

					      (append (new-goals-above p)

						      reps)))

					  (incf (depth p))) pr))

				 (subproofs sub))

			      (mapc 

			       #'(lambda (pr) (setf (superproof pr) sub)

				   (map-proof

				    #'(lambda (p)

					(when (options p)

					  (push cjunct (new-goals-above p)))

					(incf (depth p))) pr))

			       (subproofs sub))))

			  (decf inc))) rcjs copies)

	      (setf (split prf) split)

	      (setf (subproofs prf) subs)

	      (setf (whither prf) :b)

	      (setf (plan prf)

		`((and-split ,(externalize-formula (formula formula))))))

	  h-proofs)))



;;; Iff    beta-rule



(defun iff (formula)

  (declare (type formula formula))

  (iff8 formula))



(defun iff8 (formula &aux (proof (birth-place formula)))

  (declare (type formula formula) (type proof proof))

  (mapc #'(lambda (pf) (iff9 pf formula))

	(leaves proof)))



(defun iff9 (proof formula)

  (if (sign formula)

      (let (subs

	    (juncts

	     ;; The renaming of universals may take place when

	     ;; show-there-is5 is called.  In that case, it would not

	     ;; be needed here.

	     (if *renaming-universals*

		 (cdr (rename-universal-variables-in-formula

		       (formula formula))) (cdr (formula formula)))))

	(setf juncts

	  (append (mapcar

		   #'(lambda (cj) (declare (list cj))

		       (make-formula-from-old cj formula :universals nil

					      :sign nil)) juncts)

		  (mapcar #'(lambda (cj) (declare (list cj))

			      (make-formula-from-old

			       cj formula :universals nil)) juncts)))

	(setf (split proof) (make-split :node proof))

	(do* ((njuncts juncts (cddr njuncts)) (incnum 2 (decf incnum))

	      (new-hyps nil njuncts)

	      (gls (list (car njuncts) (cadr njuncts)) nil))

	    ((null njuncts) subs)

	  (let ((sub (make-sequent

		      :new-hyps new-hyps :new-goals gls :superproof proof

		      :depth (1+ (depth proof)) :index incnum)))

	    (push sub subs)

	    (push sub (used formula))

	    (push (split proof) (splits (car njuncts)))

	    (push (split proof) (splits (cadr njuncts)))

	    (setf (birth-place (car njuncts)) sub

		  (birth-place (cadr njuncts)) sub)))

	(setf (plan proof)

	  `((iff ,(externalize-formula (formula formula)) hyp)))

	(setf (whither proof) :b)

	(setf (subproofs proof) subs))

    (let (subs

	  (juncts

	   (if *renaming-universals*

	       (cdr (rename-universal-variables-in-formula

		     (formula formula))) (cdr (formula formula)))))

      ;; Rewrite this to go ahead and apply both promote rules here.

      ;; If I do that, then we need to make two copies of each

      ;; formula because they will have different signs.

      (setf juncts

	(append (mapcar #'(lambda (dj) (declare (list dj))

			    (make-formula-from-old dj formula :universals nil))

			juncts)

		(mapcar #'(lambda (cj) (declare (list cj))

			    (make-formula-from-old cj formula :universals nil

						   :sign t)) juncts)))

      (setf (split proof) (make-split :node proof))

      (do ((cj (list (fourth juncts) (caddr juncts)) (cdr cj))

	   (dj juncts (cdr dj))

	   (incnum 2 (decf incnum)))

	  ((zerop incnum) subs)

	(let ((sub (make-sequent

		    :new-hyps (list (car cj)) :new-goals (list (car dj))

		    :superproof proof :depth (1+ (depth proof)) :index

		    incnum)))

	  (push sub subs)

	  (push sub (used formula))

	  (push (split proof) (splits (car cj)))

	  (push (split proof) (splits (car dj)))

	  (setf (birth-place (car cj)) sub (birth-place (car dj)) sub)))

      (setf (plan proof)

	`((iff ,(externalize-formula (formula formula)) goal)))

      (setf (whither proof) :b)

      (setf (subproofs proof) subs))))



;;; Or-split    beta-rule



(defun or-split (formula)

  (declare (type formula formula))

  (or-split8 formula))



(defun or-split8 (formula &aux (proof (birth-place formula)))

  (mapc #'(lambda (pf) (or-split9 pf formula))

	(leaves proof)))



(defun or-split9 (proof formula)

  (let ((djuncts (mapcar

		  #'(lambda (frm)

		      (declare (list frm))

		      (make-formula-from-old frm formula :universals nil))

		  (cdr (if *renaming-universals*

			   (rename-universal-variables-in-formula

			    (formula formula)) (formula formula)))))

	(incnum (length (cdr (formula formula)))) subs)

    (setf (split proof) (make-split :node proof))

    (mapcar #'(lambda (djunct)

		(let ((sub (make-sequent

			    :new-hyps (list djunct) :superproof proof

			    :depth (1+ (depth proof)) :index incnum)))

		  (push sub subs)

		  (push sub (used formula))

		  (setf (birth-place djunct) sub)

		  (push (split proof) (splits djunct))

		  (decf incnum))) (reverse djuncts))

    (setf (whither proof) :b)

    (setf (plan proof) `((or-split ,(externalize-formula (formula formula)))))

    (setf (subproofs proof) subs)))



;;; If-split    beta-rule



(defun if-split (formula)

  (declare (type formula formula))

  (if-split8 formula))



(defun if-split8 (formula &aux (proof (birth-place formula)))

  (declare (type formula formula) (type proof proof))

  (mapc #'(lambda (pf) (if-split9 pf formula))

	(leaves proof)))



(defun if-split9 (proof formula)

  (if (sign formula)

      (let* ((ren (if *renaming-universals*

		      (rename-universal-variables-in-formula

		       (formula formula)) (formula formula)))

	     (pred1 (make-formula-from-old (cadr ren) formula :universals nil))

	     (pred2 (make-formula-from-old (cadr ren) formula :universals nil

					   :sign nil))

	     (f1 (make-formula-from-old (caddr ren) formula :universals nil))

	     (f2 (make-formula-from-old (cadddr ren) formula :universals nil))

	     (subs (list

		    (make-sequent

		     :new-hyps (list pred1 f1) :superproof proof

		     :depth (1+ (depth proof)) :index 1)

		    (make-sequent

		     :new-hyps (list f2) :new-goals (list pred2)

		     :superproof proof

		     :depth (1+ (depth proof)) :index 2))))

	(declare (list subs))

	(setf (split proof) (make-split :node proof))

	(setf (birth-place f1) (car subs) (birth-place f2) (cadr subs)

	      (birth-place pred1) (car subs) (birth-place pred2) (cadr subs))

	(setf (used formula) (nconc (used formula) (copy-list subs)))

	(push (split proof) (splits f1))

	(push (split proof) (splits f2))

	(push (split proof) (splits pred1))

	(push (split proof) (splits pred2))

	(setf (whither proof) :b)

	(setf (plan proof)

	  `((if ,(externalize-formula (formula formula)) hyp)))

	(setf (subproofs proof) subs))

    (let* ((ren (if *renaming-universals*

		    (rename-universal-variables-in-formula (formula formula))

		  (formula formula)))

	   (pred1 (make-formula-from-old (cadr ren) formula :universals nil

					 :sign t))

	   (pred2 (make-formula-from-old (cadr ren) formula :universals nil))

	   (f1 (make-formula-from-old (caddr ren) formula :universals nil))

	   (f2 (make-formula-from-old (cadddr ren) formula :universals nil)))

      (let ((subs (list

		   (make-sequent

		    :new-hyps (list pred1) :new-goals (list f1)

		    :superproof proof

		    :depth (1+ (depth proof)) :index 1)

		   (make-sequent

		    :new-goals (list pred2 f2) :superproof proof

		    :depth (1+ (depth proof)) :index 2))))

	(setf (birth-place pred1) (car subs) (birth-place f1) (car subs)

	      (birth-place pred2) (cadr subs) (birth-place f2) (cadr subs))

	(setf (used formula) (nconc (used formula) (copy-list subs)))

	(setf (split proof) (make-split :node proof))

	(push (split proof) (splits f1))

	(push (split proof) (splits f2))

	(push (split proof) (splits pred1))

	(push (split proof) (splits pred2))

	(setf (whither proof) :b)

	(setf (plan proof)

	  `((if ,(externalize-formula (formula formula)) goal)))

	(setf (subproofs proof) subs)))))



;;; Back-chain   beta-rule



(defun back-chain (formula)

  (declare (type formula formula))

  (back-chain8 formula))



(defun back-chain8 (formula &aux (proof (birth-place formula)))

  (mapc #'(lambda (pf) (back-chain9 pf formula)) (leaves proof)))



(defun back-chain9 (proof formula)

  (let* ((new-goal-form (if *renaming-universals*

			    (rename-universal-variables-in-formula

			     (formula formula)) (formula formula)))

	 (new-hyp (make-formula-from-old (caddr new-goal-form) formula

					 :universals nil))

	 (new-goal (make-formula-from-old

		    (cadr new-goal-form) formula :universals nil :sign nil))

	 (subs (list (make-sequent

		      :new-goals (list new-goal) :superproof proof :depth

		      (1+ (depth proof)) :index 1)

		     (make-sequent

		      :new-hyps (list new-hyp) :depth (1+ (depth proof))

		      :index 2 :superproof proof))))

    (declare (list subs))

    (setf (split proof) (make-split :node proof))

    (setf (used formula) (nconc (used formula) (copy-list subs)))

    (setf (birth-place new-goal) (car subs) (birth-place new-hyp) (cadr subs))

    (push (split proof) (splits new-goal))

    (push (split proof) (splits new-hyp))

    (setf (plan proof)

      `((back-chain ,(externalize-formula (formula formula)))))

    (setf (whither proof) :b)

    (setf (subproofs proof) subs)))



