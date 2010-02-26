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



;;;From - Thu Jul 18 12:04:05 1996

;;;From: munyer@MCS.COM (Robert Munyer)

;;;Newsgroups: comp.lang.lisp

;;;Subject: Re: Better way to indent recursively using pretty printing

;;;Date: 18 Jul 1996 08:39:33 -0500

;;;Message-ID: <4sleql$cmp@Mercury.mcs.com>

;;;References: <31ED2221.2F8EE8F@math.utexas.edu>



;;;In article <31ED2221.2F8EE8F@math.utexas.edu>,

;;;Benjamin Shults  <bshults@math.utexas.edu> wrote:



;;;>   (apply #'format `(nil ,@(beautify-plan1 plan)))

;;;>

;;;> Apparently call-arguments-limit and lambda-parameters-limit are

;;;> too low for me at 64.



;;;First, something that has nothing to do with your question but

;;;will simplify notation.  You don't have to use cons or list* or

;;;backquote to prepend initial arguments for apply, because apply

;;;will do it for you.  I. e. instead of



;;;    (apply #'format `(nil ,@(beautify-plan1 plan)))



;;;you can use



;;;    (apply #'format nil (beautify-plan1 plan))



;;;OK, now for the more useful part.  First, an easy quick fix.



;;;    (let ((beaut (beautify-plan1 plan)))

;;;      (format nil "~?" (first beaut) (rest beaut)))



;;;If you're lucky, format will process this without ever using the

;;;call-arguments-limit mechanism, and you'll be able to use your

;;;existing code with virtually no changes.



;;;Here are some other things you might want to consider:



;;;It sounds like you are "flattening" your structure before you

;;;pass it to format.  This is probably not really necessary; you

;;;could probably get format to do what you want, without flattening

;;;the structure, by making use of the ~? and ~{~} directives.



;;;On the other hand, maybe you really want it flat.  In that case,

;;;consider this:



;;;    (apply #'format nil fmt args)



;;;is the same thing as



;;;    (with-output-to-string (s)

;;;      (apply #'format s fmt args))



;;;which is nearly the same thing as



;;;    (with-output-to-string (s)

;;;      (map nil

;;;           #'(lambda (f a) (apply #'format s f a))

;;;           fmt-list arg-list-list))



;;;where fmt-list is a list of substrings of fmt, and arg-list-list

;;;is a list of sublists of args, each having from 0 to 63 elements.



;;;Hope this helps.



;;;-- Robert



(in-package :ipr)



;;; This file implements functions to output pretty things to the

;;; screen.



;; Benji, write a function which prints out statistics such as 

;; number of branches, nodes, etc.



(defun tell-emacs (command)

  ;; this lets the user enter an arbitrary lisp command.

  ;; this is called by chk-ask-ipr in emacs

  (emacs-eval

   `(with-output-to-temp-buffer ,(quote-string "*Ipr-messages*")

      (princ (format ,(quote-string

		       (format nil "The result of the command: ~s is~%~s"

			       command (eval command))))))))



(defun output-to-s-expression (obj)

  (if (proof-p obj)

      (if (hypotheses obj)

	  (if (goals obj)

	      `(implies ,(case (length (hypotheses obj))

			   (1 (formula (car (hypotheses obj))))

			   (t `(and ,@(mapcar #'formula (hypotheses obj)))))

		 ,(case (length (goals obj))

		    (1 (formula (car (hypotheses obj))))

		    (t `(or ,@(mapcar #'formula (goals obj))))))

	    `(not ,(case (length (hypotheses obj))

		     (1 (formula (car (hypotheses obj))))

		     (t `(and ,@(mapcar #'formula (hypotheses obj)))))))

	(case (length (goals obj))

	  (0 nil)

	  (1 (formula (car (goals obj))))

	  (t `(or ,@(mapcar #'formula (goals obj))))))

    (if (kb-theorem-suppositions obj)

	(if (kb-theorem-conclusions obj)

	    `(implies ,(case (length (kb-theorem-suppositions obj))

			 (1 (formula (car (kb-theorem-suppositions obj))))

			 (t `(and ,@(mapcar

				     #'formula

				     (kb-theorem-suppositions obj)))))

	       ,(case (length (kb-theorem-conclusions obj))

		  (1 (formula (car (kb-theorem-suppositions obj))))

		  (t `(or ,@(mapcar #'formula (kb-theorem-conclusions obj))))))

	  `(not ,(case (length (kb-theorem-suppositions obj))

		   (1 (formula (car (kb-theorem-suppositions obj))))

		   (t `(and ,@(mapcar

			       #'formula

			       (kb-theorem-suppositions obj)))))))

      (case (length (kb-theorem-conclusions obj))

	(0 nil)

	(1 (formula (car (kb-theorem-conclusions obj))))

	(t `(or ,@(mapcar #'formula (kb-theorem-conclusions obj))))))))



(defun print-statistics ()

  (emacs-eval `(with-output-to-temp-buffer ,(quote-string "*Ipr-messages*")

		 (princ (format ,(quote-string

				  (format nil "The number of branches is ~s.

The number of nodes is ~s.

The time is ~s." (how-many-branches) (how-many-nodes) *time*)))))))



(defun proof-plan (proof done &aux (subst (and done (externalize-f-cache

						     (full-subst done) t))))

  ;; This returns a plan for the entire proof.  If done then

  ;; instantiate the plan using SUBST.

  (proof-plan1 proof subst))



(defun proof-plan1 (proof subst)

  (if (cdr (subproofs proof))

      (append (if subst (instantiate-plan (plan proof) subst) (plan proof))

	      (mapcar #'(lambda (p) (proof-plan1 p subst)) (subproofs proof)))

    (if (subproofs proof)

	(append (if subst (instantiate-plan (plan proof) subst) (plan proof))

		(proof-plan1 (car (subproofs proof)) subst))

      (if subst (instantiate-plan (plan proof) subst)

	(plan proof)))))



;;;(defun proof-plan1 (proof subst)

;;;  (if (cdr (subproofs proof))

;;;      (append (if subst (instantiate-plan (plan proof) subst) (plan proof))

;;;	      (mapcar #'(lambda (p) (proof-plan1 p subst)) (subproofs proof)))

;;;    (if (subproofs proof)

;;;	(append (if subst (instantiate-plan (plan proof) subst) (plan proof))

;;;		(proof-plan1 (car (subproofs proof)) subst))

;;;      (car (instantiate-plan (plan proof) subst)))))



(defun show-remaining-sequents ()

  (mapcar #'(lambda (e) (declare (type (or list symbol) e))

	      (beautify-formula e))

	  (mapcar #'output-to-s-expression (leaves *proof*))))



(defun show-plan (proof)

  (declare (ignore proof))

  (emacs-eval

   `(send-plan ,(quote-string (concatenate 'string

				(beautify-plan (proof-plan *proof* *done*))

				(beautify-entire-proof *proof*))))))



(defun beautify-subst1 (subst)

  (let (cars cdrs)

    (mapcar #'(lambda (pair &aux (term (beautify-term1 (cdr pair)))

			    (var (beautify-term1 (car pair))))

		(setq cars (nconc cars (list (format nil "~a for ~a"

						     (car term) (car var)))))

		(setq cdrs (nconc cdrs (cdr term) (cdr var)))) subst)

    `(,(format nil (formatter "~{~a~^~:@_~}") cars) ,@cdrs)))



(defun kb-beautify-kb ()

  (mapcar #'(lambda (thm)

;;;	      (declare (type proof thm))

	      (beautify-formula (output-to-s-expression thm)))

	  *kb-theorems*))



(defun beautify-sequent (proof)

  (declare (type proof proof))

  (beautify-proof proof))



(defun beautify-proof-as-theorem (proof)

  (declare (type proof proof))

  (apply #'format `(nil ,@(beautify-proof-as-theorem1 proof))))



(defun beautify-proof-as-theorem1 (proof)

  ;; I need to fix the involved slots with *done* or make a function

  ;; which accepts the proof and then collapses and fixes them.  In

  ;; the meantime, I need a function which finds all and only functs

  ;; involved with *done*.

  (declare (type proof proof))

  (let ((hyps (if (cdr (hypotheses proof))

		  (do ((f (hypotheses proof) (cdr f)) (h 1 (1+ h))

		       (hyp nil (append hyp (list (beautify-formula1

						   (formula (car f)) h)))))

		      ((null f) hyp)

		    (declare (list f hyp) (type (integer 0) h)))

		(do ((f (hypotheses proof) (cdr f))

		     (hyp nil (append hyp (list (beautify-formula2

						 (formula (car f)))))))

		    ((null f) hyp)

		  (declare (list f hyp)))))

	(goals (if (cdr (goals proof))

		   (do ((f (goals proof) (cdr f)) (g 1 (1+ g))

			(goal nil (append

				   goal (list (beautify-formula1

					       (formula (car f)) g)))))

		       ((null f) goal)

		     (declare (list f goal) (type (integer 0) g)))

		 (do ((f (goals proof) (cdr f)) ; this could be more

					; efficient

		      (goal nil (append goal (list (beautify-formula2

						    (formula (car f)))))))

		     ((null f) goal)

		   (declare (list f goal))))))

    (declare (list hyps goals))

    `(,(format nil (formatter "~:[~:[~;~

~:[:~; One of the following is true:~]~:@_~{~A~@:_~}~]~;Suppose~

~3*~:[:~; all of the following:~]~:@_~{~a~@:_~}~:[.~

~;Then~:[:~; one of the following:~]~:@_~{~a~@:_~}~]~]")

	       hyps goals (cdr goals)

	       (mapcar 'car goals) (cdr hyps) (mapcar 'car hyps)

	       goals (cdr goals) (mapcar 'car goals))

      ,@(let ((h (mapcan 'cdr hyps)) (g (mapcan 'cdr goals)))

	  (if h (append h g) g)))))



(defun beautify-entire-proof (proof)

  (declare (type proof proof))

  (do ((p (list proof) (append (subproofs (car p)) (cdr p)))

       (string "" (concatenate 'string

		    string "



"

;;;		    (apply #'format `(nil ,@(beautify-proof1 (car p)))))))

		    (beautify-proof (car p)))))

      ((null p) string)))



(defun beautify-proof (proof)

  (declare (type proof proof))

  (apply #'format `(nil ,@(beautify-proof1 proof))))



(defun beautify-proof1 (proof)

  ;; I need to fix the involved slots with *done* or make a function

  ;; which accepts the proof and then collapses and fixes them.  In

  ;; the meantime, I need a function which finds all and only functs

  ;; involved with *done*.

  (declare (type proof proof))

  (if (and *involved-only* *done*)

      (let (i-hyps i-goals (h 0) (g 0))

	(declare (list i-hyps i-goals) (type (integer 0) h g))

	(do-tails (ij (involved proof))

	  (declare (list ij))

	  (if (sign (car ij))

	      (setq i-hyps

		(nconc i-hyps (list (cons (formula (car ij)) (incf h)))))

	    (setq i-goals

	      (nconc i-goals (list (cons (formula (car ij)) (incf g)))))))

	(when i-hyps

	  (if (cdr i-hyps)

	      (setq i-hyps

		(mapcar #'(lambda (f) (declare (list f))

			    (beautify-formula1 (car f) (cdr f))) i-hyps))

	    (setq i-hyps (list (beautify-formula2 (caar i-hyps))))))

	(when i-goals

	  (if (cdr i-goals)

	      (setq i-goals

		(mapcar #'(lambda (f) (declare (list f))

			    (beautify-formula1 (car f) (cdr f))) i-goals))

	    (setq i-goals (list (beautify-formula2 (caar i-goals))))))

	`(,(format nil (formatter "[~d]~{~d~^.~}~:@_~:[~;;;;~1:*~s~:@_~]~

~:[~:[~;We have shown~:[:~; one of the following:~]~:@_~{~A~@:_~}~]~;~

If we suppose~3*~:[:~; all of the following:~]~:@_~{~a~@:_~}~

~:[then we are done.~;then we have shown~:[:~; one of the following:~]~

~:@_~{~a~@:_~}~]~]")

		   (depth proof) (path proof)

		   (when (superproof proof) (caar (plan (superproof proof))))

		   i-hyps i-goals (cdr i-goals)

		   (mapcar 'car i-goals) (cdr i-hyps) (mapcar 'car i-hyps)

		   i-goals (cdr i-goals) (mapcar 'car i-goals))

	  ,@(let ((hy (mapcan 'cdr i-hyps)) (go (mapcan 'cdr i-goals)))

	      (if hy (append hy go) go))))

    (let* ((hypotheses (unused-hypotheses-here proof proof))

	   (hyps (if (cdr hypotheses)

		     (do ((f hypotheses (cdr f)) (h 1 (1+ h))

			  (hyp nil (append hyp (list (beautify-formula1

						      (formula (car f)) h)))))

			 ((null f) hyp)

		       (declare (list f hyp) (type (integer 0) h)))

		   (do ((f hypotheses (cdr f))

			(hyp nil (append hyp (list (beautify-formula2

						    (formula (car f)))))))

		       ((null f) hyp)

		     (declare (list f hyp)))))

	   (gls (unused-goals-here proof proof))

	   (goals (if (cdr gls)

		      (do ((f gls (cdr f)) (g 1 (1+ g))

			   (goal nil (append

				      goal (list (beautify-formula1

						  (formula (car f)) g)))))

			  ((null f) goal)

			(declare (list f goal) (type (integer 0) g)))

		    (do ((f gls (cdr f)) ; this could be more

					; efficient

			 (goal nil (append goal (list (beautify-formula2

						       (formula (car f)))))))

			((null f) goal)

		      (declare (list f goal))))))

      (declare (list hyps goals))

      `(,(format nil (formatter "[~d] ~{~d~^.~}~:@_~:[~;;;;~1:*~s~:@_~]~:[~

~:[~;Show~:[:~; one of the following:~]~:@_~{~A~@:_~}~]~;Suppose~

~3*~:[:~; all of the following:~]~:@_~{~a~@:_~}~:[.~

~;and show~:[:~; one of the following:~]~:@_~{~a~@:_~}~]~]")

		 (depth proof) (path proof) (when (superproof proof)

					      (caar (plan (superproof proof))))

		 hyps goals (cdr goals)

		 (mapcar 'car goals) (cdr hyps) (mapcar 'car hyps)

		 goals (cdr goals) (mapcar 'car goals))

	,@(let ((h (mapcan 'cdr hyps)) (g (mapcan 'cdr goals)))

	    (if h (append h g) g))))))



(defun unused-hypotheses-here (proof c-p)

  ;; Positive formulas which are not used at PROOF.

  (append (remove-if #'(lambda (f) (formula-unused-here f proof))

		     (new-hyps c-p))

	  (and (superproof c-p)

	       (unused-hypotheses-here proof (superproof c-p)))))



;;; A formula is unused in a proof if one of the places in the used

;;; slot is in the tree of proof.

;;; 

;;; A formula is used in a proof if none of the places in the used

;;; slot are in the tree of proof.

(defun formula-unused-here (form proof)

  (some #'(lambda (p) (and (not (eq t p)) (in-tree proof p))) (used form)))



(defun unused-goals-here (proof c-p)

  ;; Negative formulas which are not used at PROOF.

  (append (remove-if #'(lambda (f) (formula-unused-here f proof))

		     (new-goals c-p))

	  (and (superproof c-p)

	       (unused-goals-here proof (superproof c-p)))))



(defun beautify-kb-theorem (theorem)

  (declare (type kb-theorem theorem))

  (apply #'format `(nil ,@(beautify-kb-theorem1 theorem))))



(defun beautify-kb-theorem1 (theorem)

  ;; I need to fix the involved slots with *done* or make a function

  ;; which accepts the theorem and then collapses and fixes them.  In

  ;; the meantime, I need a function which finds all and only functs

  ;; involved with *done*.

  (declare (type theorem theorem))

  (let ((hyps (if (cdr (kb-theorem-suppositions theorem))

		  (do ((f (kb-theorem-suppositions theorem) (cdr f))

		       (h 1 (1+ h))

		       (hyp nil (append hyp (list (beautify-formula1

						   (car f) h)))))

		      ((null f) hyp)

		    (declare (list f hyp) (type (integer 0) h)))

		(do ((f (kb-theorem-suppositions theorem) (cdr f))

		     (hyp nil (append hyp (list (beautify-formula2 (car f))))))

		    ((null f) hyp)

		  (declare (list f hyp)))))

	(goals (if (cdr (kb-theorem-conclusions theorem))

		   (do ((f (kb-theorem-conclusions theorem) (cdr f))

			(g 1 (1+ g))

			(goal nil (append

				   goal (list (beautify-formula1 (car f) g)))))

		       ((null f) goal)

		     (declare (list f goal) (type (integer 0) g)))

		 (do ((f (kb-theorem-conclusions theorem) (cdr f))

					; this could be more

					; efficient

		      (goal nil (append goal (list (beautify-formula2

						    (car f))))))

		     ((null f) goal)

		   (declare (list f goal))))))

    (declare (list hyps goals))

    `(,(format nil (formatter

		    "Internal name: ~s~:@_Original name: ~s~:@_~

Description: ~a~:@_~:@_~:[~:[~;~

~:[~;One of the following is true:~:@_~]~{~A~@:_~}~]~;~:[The following~

~2*~:[ is false:~; are contradictory:~]~:@_~{~a~@:_~}~;~

Suppose~2*~:[~; all of the following:~]~:@_~{~a~@:_~}~

Then~:[~; one of the following is true:~]~:@_~{~a~@:_~}~]~]")

	       (kb-theorem-name theorem) (kb-theorem-orig-name theorem)

	       (kb-theorem-string theorem)

	       hyps goals (cdr goals)

	       (mapcar 'car goals) (cdr hyps) (mapcar #'car hyps)

	       (cdr goals) (mapcar 'car goals))

      ,@(let ((h (mapcan 'cdr hyps)) (g (mapcan #'cdr goals)))

	  (if h (append h g) g)))))



;;; Plans: list all unbound variable as "constants" at the beginning.

;;; State the theorem to prove at the beginning.  When a 'consider

;;; rule is applied, then say "Let ~a be a constant" or "Let ~a be a

;;; function depending on ..." and from then on just give the skolem

;;; function's name without the parens or args.  If a formula is not

;;; involved then don't mention it.  When a beta rule is applied say

;;; "...renaming universal variables".  Go ahead and instantiate rigid

;;; variables as soon as they are mentioned.

(defun beautify-plan (plan)

  (declare (list plan))

  (let ((*print-right-margin* 70) (out (beautify-plan1 plan)))

    (declare (list out))

    ;; put intro and qed here (if done)

    (if *done*

	(concatenate 'string

	  (format nil "~@[~a~%~]Theorem~:[:~; [~a]:~]~%~a~%~%Proof:~%"

		  (when (not (string= "" (kb-theorem-string *proof*)))

		    (kb-theorem-string *proof*))

		  (theorem-name *proof*)

		  (beautify-proof *proof*)

		  )

	  (apply #'format `(nil ,@out)))

      (apply #'format `(nil ,@out)))))



(defun beautify-plan1 (plan)

  (declare (list plan))

  ;; When *done* I want either to instantiate everything possible or

  ;; mention the substitutions which close the thing at the end.  I

  ;; think that instantiating is the thing to do.

  (when plan

    (let ((exp (car plan)))

      (case (car exp)

	(done

	 (if (cadr exp)

	     (let ((subst (beautify-subst1 (cadr exp))))

	       `(,(format nil (formatter

			       "This is finished~:[.~; by the application ~

of the substitution:~1:*~:@_~a~]") (car subst)) ,@(cdr subst)))

	   '("This branch is finished!")))

	(browns-rule

	 (let ((form (beautify-formula1 (cadr exp)))

	       (rest (beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter "Apply the equality ~~2i~~:@_~a.~:@_~a")

		      (car form) (car rest))

	     ,@(cdr form) ,@(cdr rest))))

	(show-there-is

	 ;; what about when many variables were introduced at once?

	 ;; perhaps make a note of its unbound children here

	 (let ((form (beautify-formula1 (cadr exp)))

	       (rest (beautify-plan1 (cdr plan)))

	       (var (beautify-term1 (cadddr exp))))

	   ;; Allow that the variable may have been instantiated.

	   `(,(format nil (formatter "Replace the first bound variable in the ~

 formula: ~~2i~~:@_~a~~:@_with ~a.~:@_~a")

		      (car form) (car var) (car rest))

	     ,@(cdr form) ,@(cdr var) ,@(cdr rest))

;;;	   `(,(format nil (formatter "Replace the first bound variable in the ~

;;; formula: ~~2i~~:@_~a~~:@_with the variable ~a.~:@_~a")

;;;		      (car form) (cadddr exp) (car rest))

;;;	     ,@(cdr form) ,@(cdr rest))

	   )

	 )

	(consider

	 ;; what about when many skolems were introduced at once?

	 (let ((form (beautify-formula1 (cadr exp)))

	       (term (beautify-term1 (cadddr exp)))

	       (rest (beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter "Replace the first bound variable in the ~

 formula: ~~2i~~:@_~a~~:@_with the new term ~a.~:@_~A")

		      (car form) (car term) (car rest))

	     ,@(cdr form) ,@(cdr term) ,@(cdr rest))))

	(promote

	 (let ((form1 (beautify-formula1 (cadr (cadr exp))))

	       (form2 (beautify-formula1 (caddr (cadr exp))))

	       (rest (beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter "Suppose that ~~1i~~:_~a~~i~

~~:@_and show that ~~1i~~:_~a.~:@_~A")

		      (car form1) (car form2) (car rest))

	     ,@(cdr form1) ,@(cdr form2) ,@(cdr rest))))

	(and-split

	 (let ((form (beautify-formula1 (cadr exp)))

	       (rest (mapcar #'beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter

			   "Split the goal: ~~1i~~:_~a~~i~~:@_into cases.~

  ~~i~~:@_After that split we will do the following:~~1i~{~:@_~a~}")

;;; ~~1i~:@_~

;;; ~<~a~:>")

		      (car form) (mapcar #'car rest))

	     ;; put indentation stuff here

	     ,@(cdr form) ,@(mapcan #'cdr rest))))

	(or-split

	 (let ((form (beautify-formula1 (cadr exp)))

	       (rest (mapcar #'beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter "Split the hypothesis: ~~1i~~:_~a~~i~~:@_~

into cases.~~i~~:_After that split we will do the following:~~1i~:@_~

~~1i~{~:@_~a~}")

		      (car form) (mapcar #'car rest))

	     ,@(cdr form) @,(mapcan #'cdr rest))))

	(back-chain

	 (let ((form (beautify-formula1 (cadr exp)))

	       (rest (mapcar #'beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter

			   "Consider the hypothesis: ~~1i~~:_~a.~~i~~:@_~

If we can prove that the hypothesis is true,~~i~~:@_~

then we will be able to assume the conclusion to be true and use it.~:@_~

~A")

		      (car form) (mapcar #'car rest))

	     ,@(cdr form) ,@(mapcar #'cdr rest))))

	(if (if (eq (caddr exp) 'hyp)

		(let ((pred (beautify-formula1 (cadr (cadr exp))))

		      (f1 (beautify-formula1 (caddr (cadr exp))))

		      (f2 (beautify-formula1 (cadddr (cadr exp))))

		      (rest (mapcar #'beautify-plan1 (cdr plan))))

		  `(,(format nil (formatter

				  "First assume ~~:@_~~1i~~:_~a~~:@_and ~

~~1i~a~~:@_Then suppose ~~1i~a~~:@_and show ~~1i~a.~~:@_~a")

			     (car pred) (car f1) (car f2) (car pred)

			     (mapcar #'car rest))

		    ,@(cdr pred) ,@(cdr f1) ,@(cdr f2) ,@(cdr pred)

		    ,@(mapcar #'cdr rest)))

	      (let ((pred (beautify-formula1 (cadr (cadr exp))))

		    (f1 (beautify-formula1 (caddr (cadr exp))))

		    (f2 (beautify-formula1 (cadddr (cadr exp))))

		    (rest (mapcar #'beautify-plan1 (cdr plan))))

		`(,(format nil (formatter

				"First assume that ~~1i~~:_~a~~i~~:@_and show ~

~~1i~a.~~:@_Then show ~~1i~a.~:@_Then show either~~1i~~:@_~a or~

~~1i~~:@_~a.~~:@_~a")

			   (car pred) (car f1) (car pred) (car f2)

			   (mapcar #'car rest))

		  ,@(cdr pred) ,@(cdr f1) ,@(cdr pred) ,@(cdr f2)

		  ,@(mapcar #'cdr rest)))))

	(iff (if (eq (caddr exp) 'hyp)

		 (let ((form1 (beautify-formula1 (cadr (cadr exp))))

		       (form2 (beautify-formula1 (caddr (cadr exp))))

		       (rest (mapcar #'beautify-plan1 (cdr plan))))

		   `(,(format nil (formatter

				   "First show either ~~1i~~:_~a or~~i~~:@_~

~a.~~:@_Then assume both ~~1i~~:_~a and~~i~~:@_~a.~~:@_~a")

			      (car form1) (car form2) (car form1) (car form2)

			      (mapcar #'car rest))

		     ,@(cdr form1) ,@(cdr form2) ,@(cdr form1) ,@(cdr form2)

		     ,@(mapcar #'cdr rest)))

		 (let ((form1 (beautify-formula1 (cadr (cadr exp))))

		       (form2 (beautify-formula1 (caddr (cadr exp))))

		       (rest (mapcar #'beautify-plan1 (cdr plan))))

		   `(,(format nil (formatter

				   "First assume ~a~~1i~~:_and show ~a.~~:@_~

Then assume ~a~~1i~~:_and show ~a.~~:@_~a")

			      (car form2) (car form1) (car form1) (car form2)

			      (mapcar #'car rest))

		     ,@(cdr form2) ,@(cdr form1) ,@(cdr form1) ,@(cdr form2)

		     ,@(mapcar #'cdr rest)))))

	(flip (let ((form (beautify-formula1 (cadr exp)))

		    (rest (beautify-plan1 (cdr plan))))

		`(,(format nil (formatter

				"Flip the formula: ~~1i~~:_~a.~:@_~a")

			   (car form) (car rest)) ,@(cdr form) ,@(cdr rest))))

	(apply-theorem

	 (let ((form1 (beautify-formula1 (cadddr (cdr exp))))

	       (form2 (beautify-formula1 (cadddr exp)))

	       (form3 (beautify-formula1 (cadddr (cddr exp))))

	       (form4 (beautify-formula1 (cadddr (cdddr exp))))

	       (rest (beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter

			   "Since we ~:[~*~;know that ~~1i~~:_~a~]~~i~~:_~

~:[~; and we~] ~

~:[~*~;are trying to show that ~~1i~~:@_~A~]~~i~~:@_~

we can apply ~~i~~:_~a~~i~~:@_~

~:[ ~:[ which finishes that branch of the proof.~*~*~;~

Now we only need to show that~~1i~~:@_~*~a.~]~~:@_~~:@_~

~;~:[to conclude that ~~1i~~:_~a~*~;~]~]~~:@_~a")

		      (cadddr (cdr exp)) (car form1)

		      (and (cadddr exp) (cadddr (cdr exp)))

		      (cadddr exp) (car form2) (beautify-theorem (caddr exp))

		      (cadr (cddddr exp)) (caddr (cddddr exp)) (car form3)

		      (car form4) (car rest))

	     ,@(cdr form1) ,@(cdr form2) ,@(cdr form3) ,@(cdr form4)

	     ,@(cdr rest))))

	(apply-theorem-split

	 (let ((form1 (beautify-formula1 (cadddr (cdr exp))))

	       (form2 (beautify-formula1 (cadddr exp)))

	       (form3 (beautify-formula1 (cadddr (cddr exp))))

	       (form4 (beautify-formula1 (cadddr (cdddr exp))))

	       (rest (mapcar #'beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter

			   "Since we ~:[~*~;know that ~~1i~~:_~a~]~~i~~:_~

~:[~; and we~] ~

~:[~*~;are trying to show that ~~1i~~:@_~A~]~~i~~:@_~

we can apply ~~i~~:_~a~~i~~:@_~

~:[ ~:[ which finishes that branch of the proof.~*~*~;~

Now we only need to show that~~1i~~:@_~*~a.~]~~:@_~~:@_~

~;~:[to conclude that ~~1i~~:_~a~*~;~

Now we must split up the proof.  ~~i~~:_~

First we will add ~~1i~~:@_~a to the hypotheses.  ~~i~~:_~

Then we will come back and prove ~~1i~~:@_~a.  ~]~]~:@_~a")

		      (cadddr (cdr exp)) (car form1)

		      (and (cadddr exp) (cadddr (cdr exp)))

		      (cadddr exp) (car form2) (beautify-theorem (caddr exp))

		      (cadr (cddddr exp)) (caddr (cddddr exp)) (car form3)

		      (car form4) (mapcar #'car rest))

	     ,@(cdr form1) ,@(cdr form2) ,@(cdr form3) ,@(cdr form4)

	     ,@(mapcar #'cdr rest))))

	(comprehension-schema

	 (if (eq (caddr exp) 'hyp)

	     (let ((form (beautify-formula1 (cadr exp)))

		   (rest (beautify-plan1 (cdr plan))))

	       `(,(format nil (formatter

			       "Apply the comprehension schema to ~

~~1i~~:_~a.~~:@_~a")

			  (car form) (car rest))

		 ,@(cdr form) ,@(cdr rest)))

	   (let ((form (beautify-formula1 (cadr exp)))

		 (rest (mapcar #'beautify-plan1 (cdr plan))))

	     `(,(format nil (formatter

			     "Apply the comprehention schema to ~

~~1i~~:_~a.~~:@_~a")

			(car form) (mapcar #'car rest))

	       ,@(cdr form) ,@(mapcar #'cdr rest)))))

	(class-schema

	 (let ((form (beautify-formula1 (cadr exp)))

	       (rest (beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter

			   "Apply the class schema to ~~1i~~:_~a.~:@_~A")

		      (car form) (car rest))

	     ,@(cdr form) ,@(cdr rest))))

	(the-removal

	 (let ((form (beautify-formula1 (cadr exp)))

	       (rest (beautify-plan1 (cdr plan))))

	   `(,(format nil (formatter

			   "Apply the iota schema to ~~1i~~:_~a.~:@_~a")

		      (car form) (car rest)) ,@(cdr form) ,@(cdr rest))))))))



;;; I will want this to return the instantiation of everything in PLAN.

(defun instantiate-plan (plan subst)

  (declare (list plan))

  (let ((exp (car plan)))

    (case (car exp)

      (done plan)

      (browns-rule `((browns-rule ,(instantiate-formula (cadr exp) subst))))

      (show-there-is `((show-there-is ,(instantiate-formula (cadr exp) subst)

				      ,@(cddr exp))))

      (consider

       `((consider ,(instantiate-formula (cadr exp) subst) ,(caddr exp)

		   ,(instantiate-term (cadddr exp) nil))))

      (promote `((promote ,(instantiate-formula (cadr exp) subst))))

      (and-split `((and-split ,(instantiate-formula (cadr exp) subst))))

      (or-split `((or-split ,(instantiate-formula (cadr exp) subst))))

      (back-chain `((back-chain ,(instantiate-formula (cadr exp) subst))))

      (if `((if ,(instantiate-formula (cadr exp) subst))))

      (iff `((iff ,(instantiate-formula (cadr exp) subst))))

      (flip `((flip ,(instantiate-formula (cadr exp) subst))))

      ((apply-theorem apply-theorem-split)

       `((,(car exp) ,(cadr exp) ,(caddr exp)

	  ,(and (cadddr exp) (instantiate-formula (cadddr exp) subst))

	  ,(and (cadddr (cdr exp))

		(instantiate-formula (cadddr (cdr exp)) subst))

	  ,(and (cadddr (cddr exp))

		(instantiate-formula (cadddr (cddr exp)) subst))

	  ,(and (cadddr (cdddr exp))

		(instantiate-formula (cadddr (cdddr exp)) subst)))))

      (church-schema

       `((church-schema ,(instantiate-formula (cadr exp) subst))))

      (class-schema

       `((class-schema ,(instantiate-formula (cadr exp) subst))))

      (the-removal

       `((the-removal ,(instantiate-formula (cadr exp) subst)))))))



(defun beautify-dlist (list)

  (declare (list list))

  (let ((d-list (beautify-dlist1 list)))

    (declare (list d-list))

    (apply #'format `(nil ,@d-list))))



(defun beautify-dlist1 (list)

  (declare (list list))

  `(,(format nil (formatter "~~{~~a~~^, ~~} "))

    ,(mapcar #'(lambda (v) (ext-name (car v))) list))

  )



(defun beautify-theorem (exp)

  (format nil (formatter "~a")

	  (let ((theorem (car (member-if #'(lambda (e)

					     (eq (kb-theorem-name e) exp))

					 *kb-theorems*))))

	    (if theorem (kb-theorem-string theorem) exp))))



(defun beautify-term (exp)

  (apply #'format `(nil ,@(beautify-term1 exp))))



(defun beautify-term1 (exp)

  (let ((*print-right-margin* 70))

    (if (atom exp)		; (if (force exp) (beautify-term1 (force exp)))

	(if *done*

	    (let ((inst (instantiate-term exp (full-subst *done*))))

	      (if (eq inst exp)

		  (list (symbol-name (ext-name exp)))

		(beautify-term1 inst)))

	  (list (symbol-name (ext-name exp))))

      (case (car exp)

	(the (let ((form (beautify-formula1 (caddr exp))))

	       `(,(format nil (formatter

			       "~~<The unique ~a such that ~~1i~~:_~a~~:>")

			  (ext-name (caadr exp)) (car form))

		 ,(cdr form))))

	(the-class-of-all

	    (let ((form (beautify-formula1 (caddr exp))))

	      `(,(format

		  nil (formatter

		       "~~<The class of all ~a such that ~~1i~~:_~a~~:>")

		  (ext-name (caadr exp)) (car form)) ,(cdr form))))

	(t (if (member (car exp) (mapcar 'car *term-formats*))

	       ;; this could be more efficient

	       (prepare-string-for-beauty1 exp t)

	     (if (member (car exp) *skolem-functions*)

		 (if (and (cdr exp) (not *done*))

		     (let ((terms (mapcar #'(lambda (e) (beautify-term1 e))

					  (cdr exp))))

		       `(,(format nil (formatter

				       "~~<~a(~{~a~^, ~~1i~~:_~})~~:>")

				  (car exp) (mapcar 'car terms))

			 ,(mapcan 'cdr terms)))

		   (list (format nil (formatter "~a") (list (car exp)))))

	       (if (cdr exp)

		   (let ((terms (mapcar #'(lambda (e) (beautify-term1 e))

					(cdr exp))))

		     `(,(format nil (formatter "~~:<~a ~{~a~^, ~~1i~~:_~}~~:>")

				(car exp) (mapcar 'car terms))

		       ,(mapcan 'cdr terms)))

		 ;; if we are inside a plan then do it this way always

		 (list (format nil (formatter "~a") exp))))))))))



(defun prepare-string-for-beauty1 (exp &optional term)

  ;; This is called on the strings provided by the user in the

  ;; kb-construction functions.

  (let ((terms (mapcar #'(lambda (f) (beautify-term1 f)) (cdr exp)))

	(string (cadr (assoc (car exp)

			     (if term *term-formats* *pred-formats*)))))

    `(,(apply #'format `(nil ,(format nil "~~~~<~a~~~~:@>" string)

			     ,@(mapcar 'car terms)))

      ,(mapcan 'cdr terms))))



(defun beautify-formula (exp)

  (apply #'format `(nil ,@(beautify-formula2 exp))))



(defun beautify-formula1 (exp &optional num)

  (let ((*print-right-margin* 70))

    (if num (let ((form (beautify-formula2 exp)))

	      (cons (format nil (formatter "~<~S  ~a~:>")

			    (list num (car form))) (cdr form)))

      (beautify-formula2 exp))))



(defun beautify-formula2 (exp)

  (declare (list exp))

  (case (car exp)

    (and (let ((forms (mapcar #'(lambda (f) (beautify-formula2 f))

			      (cdr exp))))

	   `(,(format nil (formatter "~~<~{~a~^ ~~i~~:@_and ~~1i~~:_~}~~:>")

		      (mapcar 'car forms)) ,(mapcan 'cdr forms))))

    (or (let ((forms (mapcar #'(lambda (f) (beautify-formula2 f)) (cdr exp))))

	  `(,(format nil (formatter "~~<~{~a~^ ~~:@_or ~~1i~~:_~}~~:>")

		     (mapcar 'car forms)) ,(mapcan 'cdr forms))))

    (if (let ((form1 (beautify-formula2 (cadr exp)))

	      (form2 (beautify-formula2 (caddr exp)))

	      (form3 (beautify-formula2 (cadddr exp))))

	  `(,(format nil (formatter

			  "~~<if~~1i~~:_~a ~~i~~:_then ~~1i~~:_~a ~~i~~:_~

otherwise ~~1i~~:_~a~~:>") (car form1) (car form2) (car form3))

	    ,(append (cdr form1) (cdr form2) (cdr form3)))))

    (iff (let ((form1 (beautify-formula2 (cadr exp)))

	       (form2 (beautify-formula2 (caddr exp))))

	   `(,(format nil (formatter

			   "~~<The following are equivalent:~~1i~~:@_~a ~

~~i~~:_and ~~1i~~:_~a~~:>") (car form1) (car form2))

	     ,(append (cdr form1) (cdr form2)))))

    (not (let ((form (beautify-formula2 (cadr exp))))

	   `(,(format nil (formatter

			   "~~<it is not the case that ~~1i~~:_~a~~:>")

		      (car form)) ,(cdr form))))

    (implies (let ((form1 (beautify-formula2 (cadr exp)))

		   (form2 (beautify-formula2 (caddr exp))))

	       `(,(format nil (formatter

			       "~~<if ~~1i~~:_~a ~~i~~:_then ~~1i~~:_~a~~:>")

			  (car form1) (car form2))

		 ,(append (cdr form1) (cdr form2)))))

    (forall (let ((d-list (beautify-dlist1 (cadr exp)))

		  (form (beautify-formula2 (caddr exp))))

	      `(,(format nil (formatter

			      "~~<for every ~~2i~~:_~a ~~1i~~:_~a~~:>")

			 (car d-list) (car form))

		(,@(cdr d-list) ,@(cdr form)))))

    (for-some (let ((d-list (beautify-dlist1 (cadr exp)))

		    (form (beautify-formula2 (caddr exp))))

		`(,(format nil (formatter

				"~~<for some ~~2i~~:_~a ~

~~1i~~:_~a~~:>")

			   (car d-list) (car form))

		  (,@(cdr d-list) ,@(cdr form)))))

    (a-class (let ((term (beautify-term1 (cadr exp))))

	       `(,(format nil (formatter "~~<~a ~~1i~~:_is a class~~:>")

			  (car term)) ,(cdr term))))

    (a-member-of

     (let ((term1 (beautify-term1 (cadr exp)))

	   (term2 (beautify-term1 (caddr exp))))

       `(,(format nil (formatter "~~<~a ~~1i~~:_is a member of ~~1i~~:_~a~~:>")

		  (car term1) (car term2))

	 ,(append (cdr term1) (cdr term2)))))

    (an-element

     (let ((term (beautify-term1 (cadr exp))))

       `(,(format nil (formatter "~~<~a ~~1i~~:_is an element~~:>") (car term))

	 ,(cdr term))))

    (= (let ((term1 (beautify-term1 (cadr exp)))

	     (term2 (beautify-term1 (caddr exp))))

	 `(,(format nil (formatter "~~<~a = ~~:_~a~~:>")

		    (car term1) (car term2))

	   ,(append (cdr term1) (cdr term2)))))

    (t (if (member (car exp) (mapcar 'car *pred-formats*))

	   (prepare-string-for-beauty1 exp)

	 (if (cdr exp)

	     (let ((forms (mapcar #'(lambda (e) (beautify-term1 e))

				  (cdr exp))))

	       `(,(format nil (formatter "~~:<~a ~{~a~^ ~~1i~~:_~}~~:>")

			  (car exp) (mapcar 'car forms))

		 ,(mapcan 'cdr forms)))

	   ;; if we are inside a plan then do it this way always

	   (list (format nil (formatter "~a") exp)))))))



(defun print-theorems ()

  (mapcar #'(lambda (th) (format t "~A~%~%" (beautify-kb-theorem th)))

	  *kb-theorems*))



