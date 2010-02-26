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





;;; This file implements restricted paramodulation.





;;; Maybe I only need to use single-step equalities.  Say, I only make


;;; a substitution if there is a single equality which does what I


;;; need.  Yeah.





;;; Keep track of all equalities and memberships (subsets?) on a


;;; branch and keep track of known equalities which involve exactly


;;; one class term.  When it is time, see if a one-step substitution


;;; can be made.


;;; 


;;; 


;;; 


;;; 


;;; 


;;; 


;;; 


;;; 





(defun make-new-comprehensions (&aux subst)


  ;; The problem here is to apply only those rp-productions which are


  ;; on the proper branch.


  (do ((mem-targ *member-targets* (cdr mem-targ)))


      ;; I will need to keep track of the productions which have


      ;; already been used on each target formula.


      ((null mem-targ))


    (let ((targ (caddr (formula (car mem-targ)))))


      (dolist (prod *thm-rp-productions*)


	;; No need to test for in-tree here.


	(when (listp (setq subst (unify-terms (rp-prod-trigger prod)


					      targ)))


	  (


	   )))


      (let ((birth (birth-place (car mem-targ))))


	(dolist (prod *rp-productions*)


	  (cond ((in-tree birth (birth-place (rp-prod-formula prod)))


		 (splice-comp-into (car mem-targ) prod birth))


		((in-tree (birth-place (rp-prod-formula prod)) birth)


		 (splice-comp-into


		  (car mem-targ) prod


		  (birth-place (rp-prod-formula prod))))))))))





(defun splice-comp-into (target prod birth &aux subst


				(targ (caddr (formula target))))


  "TARGET is the target formula, TARG is the term to be replaces by


the production PROD."


  (when (and (listp (setq subst (compatible-substitutions


				 (bindings (rp-prod-formula prod))


				 (bindings target))))


	     (listp (setq subst (unify-terms (rp-prod-trigger prod)


					     targ subst))))


    (let ((form (make-formula-from-old


		 (instantiate-term


		  (list 'a-member-of (cadar (formula target))


			(rp-prod-class-term prod)) subst)


		 target :bindings subst :copies (copies target)


		 :splits (delete-duplicates


			  (append (splits target)


				  (copy-list (splits (rp-prod-formula prod)))))


		 :parents-of (list target (rp-prod-formula prod)))))


      ;; It will be pushed into *comprehensions* automatically.


;;;	      (setf (birth-place form) (birth-place target))


      ;; Apparently, I need to splice a new proof in here with


      ;; the new formula on it just as I do in brown's rule.


      (let ((sub (make-sequent


		  :new-hyps (and (sign form) (list form))


		  :new-goals (and (not (sign form)) (list form))


		  :superproof birth :depth (1+ (depth birth))


		  :plan (plan birth) :wither (whither birth)


		  :subproofs (subproofs birth))))


	(setf (birth-place form) sub)


	(setf (plan birth) `((rp-comp ,(externalize-formula target))))


	(set-splits birth sub)


	(setf (whither birth) :a)


	;; No chance for a new-alpha.


	(mapc #'(lambda (pr)


		  (setf (superproof pr) sub)


		  (map-proof #'(lambda (p) (incf (depth p))) pr))


	      (subproofs sub))


	(setf (subproofs proof) (list sub))


	)


      )


    )


  )





;;; In make-new-extensionalities, be sure to put new equalities into


;;; *new-equals*.


