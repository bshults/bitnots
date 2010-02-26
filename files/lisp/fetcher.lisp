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

;;; This file implements the strategies for selecting theorems to
;;; apply.

(defconstant *base-rank* 10)

(defun rate-option (option proof apps-to-branch)
  (rate-option-perm option)
  (rate-option-temp option proof apps-to-branch))

(defun option-rank (option)
  (+ (option-perm-rank option)
     (option-temp-rank option)))

;;; Check the depth of BINDINGS and deprecate use of deep bindings (a lot).
(defun rate-option-perm (option)
  ;; Need to keep track of the theorems slot in formulas and the
  ;; kb-searched slot.
  (let* ((bindings
	  (remove-if #'(lambda (p &aux (str (symbol-name (car p))))
			 (or (< (length str) 6)
			     (not (string= "*PARAM" (subseq str 0 6)))))
		     (option-subst option)))
	 (length-bindings (length bindings))
	 (depth-bindings
	  ;; This is the average depth.
	  (if (zerop length-bindings) 0
	    (/ (reduce #'+ (mapcar #'(lambda (p) (term-depth (cdr p)))
				   bindings))
	       length-bindings)))
	 ;; Benji, instead of this try the length of it with
	 ;; duplicates in the cdr removed.
	 (length-subst (length (option-subst option)))
	 (depth-subst
	  ;; This is the average depth.
	  (if (zerop length-subst) 0
	    (/ (reduce #'+ (mapcar #'(lambda (p) (term-depth (cdr p)))
				   (option-subst option)))
	       length-subst)))
	 (option-match (null bindings)))
    ;; Make the rank of an option a structure which contains info
    ;; about this stuff.  Since the info which determines the rank
    ;; changes, this info may have to be recalculated.
    (setf (option-perm-rank option)
      (+ (* *base-rank*
	    (case (option-baddies option)
	      (0 (if option-match 1000 100))
	      (1 -17)
	      (2 -40)
	      (t (* -25 (option-baddies option)))))
	 (* (* *base-rank* 8) (option-goodies option))
	 (* (* *base-rank* 9)
	    (count-if #'(lambda (f) (eq '= (car f)))
		      (option-bad-goals option)))
	 (* (* *base-rank* 6)
	    (count-if #'(lambda (f) (eq '= (car f)))
		      (option-bad-hyps option)))
	 (* (* *base-rank* -3) depth-bindings)
	 (* (* *base-rank* -3) depth-subst)
	 ;; I think it is a little bit stupid to unify a sequent
	 ;; junct with more than one theorem junct.  No, let's
	 ;; make it a lot stupid.
	 (* (* -80 *base-rank*)
	    (do ((ggs (option-good-goals option) (cdr ggs))
		 (num 0 (if (member (formula (car ggs)) (cdr ggs)
				    :test #'form-equal :key #'formula)
			    ;; maybe use form-equal.  (was using 'eq)
			    (1+ num) num)))
		((null ggs)
		 (do ((ghs (option-good-hyps option) (cdr ghs))
		      (num num (if (member (formula (car ghs)) (cdr ghs)
					   :test #'form-equal
					   :key #'formula)
				   ;; maybe use form-equal.  (was using 'eq)
				   (1+ num) num)))
		     ((null ghs) num)))))
	 ))))

(defun rate-option-temp (option proof apps-to-branch)
  (let* ((similar-applications
	  ;; how many times has this theorem been applied on this
	  ;; branch with the same theorem-formulas matching?
	  (let ((goals (option-bad-goals option))
		(hyps (option-bad-hyps option)))
	    ;; Applications to branch should be put as high as
	    ;; possible.  And they are no longer inherited.
	    (do ((apps apps-to-branch (cdr apps))
		 (inc 0 (1+ inc))
		 (num 0 (let ((gs (option-bad-goals (car apps)))
			      (hs (option-bad-hyps (car apps))))
			  ;; These are #'eq to the originals.  I.e. no
			  ;; subst has been applied, etc.  And, they
			  ;; are also ordered by their original order
			  ;; in the kb-theorem.
			  (if (and (list-equal gs goals :test #'eq)
				   (list-equal hs hyps :test #'eq))
			      (1+ num) num))))
		((null apps) (cons num inc)))))
	 ;; I want to prefer formulas with shorter theorems lists,
	 ;; older formulas.
	 ;; 
	 ;; I want these to include used delta formulas but not gamma
	 ;; formulas.
	 (c-goals (spec-goals proof))
	 (c-hyps (spec-hypotheses proof))
	 (bad-match
	  ;; A bad-match may not be used.  I may want to make this
	  ;; even more careful.  I.e. check for subsumption.  I am
	  ;; checking for subsumption enven though that is too strong.
	  ;; Really, this should check for form-equal to remain more
	  ;; complete.
	  (or (intersects
	       c-goals (option-bad-hyps option) :test
	       #'(lambda (a b)
		   (let ((subst (compatible-substitutions
				 (option-subst option) (bindings a))))
		     (and (listp subst)
			  (listp (formula-subsumes
				  (instantiate-formula (formula a) subst)
				  (instantiate-formula b subst) nil))))))
	      (intersects
	       c-hyps (option-bad-goals option) :test
	       #'(lambda (a b)
		   (let ((subst (compatible-substitutions
				 (option-subst option) (bindings a))))
		     (and (listp subst)
			  (listp (formula-subsumes
				  (instantiate-formula (formula a) subst)
				  (instantiate-formula b subst) nil)))))))))
    (setf (option-temp-rank option)
      ;; Put stuff about the age of the formulas in here.  Take the
      ;; depth of PROOF and divide it into the depth of each good
      ;; formula.  Take the average of these and multiply by -20 or
      ;; so.
      (+ (if bad-match (* *base-rank* -1000) 0)
	 (+ (* (* -25 *base-rank*) (car similar-applications))
;;;	    (* (* -1 *base-rank*) (cdr similar-applications))
;;; The below was what won before, I think.  Yes, I'm sure of it.
	    (* (* -20 *base-rank*) (cdr similar-applications))
	    ;; I'm now thinking that I want to prefer deep formulas.
	    ;; the shallow formulas will always be there.
	    (* (* 5 *base-rank*)
	       ;; this is the average normed depth of the formulas.
	       (/ (reduce #'+ (mapcar #'(lambda (f) (depth (birth-place f)))
				      (append (option-good-goals option)
					      (option-good-hyps option))))
		  (* (option-goodies option) (depth proof))))
	    (* (* 10 *base-rank*)
	       ;; this is the max normed depth of the formulas.
	       (/ (reduce #'max (mapcar #'(lambda (f) (depth (birth-place f)))
					(append (option-good-goals option)
						(option-good-hyps option))))
		  (* (depth proof))))
	    )))
    (+ (option-perm-rank option) (option-temp-rank option))))

