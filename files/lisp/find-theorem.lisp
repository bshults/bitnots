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

;;; This file implements the search of the knowledge base for options.

(defun non-silly-option (option)
  (notevery #'t-init-silly (option-t-inits option)))

(defun find-and-apply-best-theorem (proof)
;;; This won't work with splicing because there may be new formulas.
;;; So, I need to check for new formulas also.
  (and (options proof) (not (or (new-goals-above proof)
				(new-hyps-above proof)))
       ;; Should we rerate and resort them first?  No, I don't think
       ;; so since nothing has happened.
       (return-from find-and-apply-best-theorem
	 (apply-next-theorem-step proof)))
  (find-best-options proof)
  ;; This should return non-nil iff it successfully applies a theorem.
  (apply-next-theorem-step proof))

(defvar *options-proof* nil)

(defun find-best-options (proof &aux new-hyps new-goals)
  ;; This should return non-nil iff it successfully applies a theorem.
  ;; Pass one:
  ;; Make T-INITS.
  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
			(format t "~%T") (format t "T")))
  ;; Reset *new-t-init-alist*.
  (setq *options-proof* nil)
  (setq *options* nil)
  (setq *merge-t-init-alist* nil)
  (setq *new-t-init-alist* nil)
  (setq *rel-t-init-alist* nil)
  ;; First, pick out the formulas which are new since the node above
  ;; this one having an options slot.  Set *t-init-alist*.
  ;; 
  ;; Somewhere, I need to check that theorem is not in the theorems
  ;; list of any of the good formulas.
  (setq *t-init-alist*
    ;; To solve the splicing problem, I added the new-*-above slot to
    ;; proofs so that info about the new forms above this point will
    ;; be present here.
    ;; 
    ;; I do not want to include those juncts which are new in the node
    ;; containing the options because they will have been included in
    ;; the search which created the (options proof).  This is true,
    ;; even if PROOF is a leaf.
    ;; 
    ;; I select the *options* and *t-init-alist* on the current branch
    ;; so that I will not have formulas which are not on the current
    ;; branch being considered.  Therefore, I do not want to
    ;; destructively modify (t-init-alist proof).
    (do ((pf proof (superproof pf))
	 ;; remove-if returns the list if nothing is removed.
	 ;; In that case it does not copy.
	 (n-hyps nil (remove-if #'used (copy-list (new-hyps pf))))
	 (n-goals nil (remove-if #'used (copy-list (new-goals pf)))))
	((or (null pf) (t-init-alist pf))
	 (when (setq *options-proof* pf)
	   (setq *options*
	     (copy-list (remove-if
			 ;; It is always safe to remove options
			 ;; without goodies.  Just make sure that the
			 ;; t-inits are included in t-init-alist.
			 #'(lambda (o)
			     (or (option-applied o)
				 (zerop (option-goodies o))
				 (some #'used (option-good-hyps o))
				 (some #'used (option-good-goals o))))
			 (options pf))))
	   ;; If pf=proof then I may not want to look at
	   ;; new-hyps-above because it may be circular.
	   (setq new-hyps
	     (nconc (remove-if #'used (new-hyps-above pf)) n-hyps new-hyps))
	   (setq new-goals
	     (nconc (remove-if #'used (new-goals-above pf)) n-goals new-goals))
	   ;; I may want to copy-tree this for future reference.  Not
	   ;; just for future reference.  I need it to remain the same
	   ;; so that it will never contain references to formulas
	   ;; born below it.  That way we will avoid cross branch
	   ;; options.  But, no.  If I copy tree, then the eq test
	   ;; won't work.  So I need to copy structure down to
	   ;; kb-formulas etc.
	   (copy-to-kb-forms-and-remove-used (t-init-alist pf)))
	 (when (eq proof *proof*)
	   (setq new-hyps (nconc n-hyps new-hyps))
	   (setq new-goals (nconc n-goals new-goals))
	   nil))
      (setq new-hyps (nconc n-hyps new-hyps))
      (setq new-goals (nconc n-goals new-goals))))
  ;; Use them to build up *new-t-init-alist*.  Then map across
  ;; *new-t-init-alist* in the second pass.
  ;; This sets up *new-t-init-alist*.
  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
			(format t "~%N") (format t "N")))
  (mapc #'fix-t-init-alist new-hyps)
  (mapc #'fix-t-init-alist new-goals)
  ;; Now all formulas have their t-inits slots filled.  The
  ;; substitutions in these t-inits may have theorem variables in both
  ;; the car and cdr.
;;;  (when *equality-fetcher*
;;;    ;; for each equality goal:
;;;    ;; 1. try to unify the sequent.
;;;    ;; 2. make a t-init involving the unified formulas as good
;;;    ;;    the equality goal as the kb-junct.
;;;    (mapc
;;;     #'(lambda (theorem)
;;;	 (mapc
;;;	  #'(lambda (goal)
;;;	      (when (eq '= (car goal))
;;;		;; this should only look at new-hyps and new-goals
;;;		(fix-t-init-alist=goal
;;;		 goal new-hyps new-goals
;;;		 ;; the below should be better.  what I really want is
;;;		 ;; those formulas which are not new.  This could be
;;;		 ;; done by storing the list of current hyps and goals
;;;		 ;; after an application of a theorem and have this
;;;		 ;; search up to the last place where such a thing was
;;;		 ;; sotred.
;;;		 (hypotheses proof)
;;;		 (goals proof))
;;;		))
;;;	  (kb-conclusions theorem)))
;;;     *kb-theorems*))
  ;; Do I know that all other formulas are taken care of in
  ;; *t-init-alist*?  Yes.
  (make-options proof))

(defun make-options (proof)
  ;; Pass two:
  ;; Make maximal unifications.
  ;; 
  ;; It would be faster to go through the theorem formulas.  That way,
  ;; once we have found a unification which has fewer than ~3 bad
  ;; juncts and we get past ther first ~3 theorem formulas we can
  ;; stop.  To do this, I will want a clean-up-kb function which
  ;; clears this info from theorem structures so it doesn't waste
  ;; space during the next proof.  Remember that one fromula from the
  ;; PROOF can unify with more than one formula from the kb-theorem.
  ;; 
  ;; I also think that t-inits should be associated with formulas
  ;; because if they are only associated with the inits slots in
  ;; sequents then they can be lost on cousin sequents.  Idea:
  ;; post the inits at the birth-place of the formula.
  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
			(format t "~%2") (format t "2")))
  (setq *juncts-max* 4)			; don't allow theorems to be used
					; if there are more that 4 bad-juncts.
  ;; Try to use info from the branch's options slot so that the same
  ;; options won't be found again.
  ;;
  ;; The merge will only concern itself with those theorems which are
  ;; triggers in *new-t-init-alist* because old options will be used
  ;; from *options*.  This sets up *merge-t-init-alist* and
  ;; *rel-t-init-alist*.
  ;; 
  ;; REL only contains the OLD inits associated with juncts which were
  ;; triggered by NEW formulas.  But REL should also contain the old
  ;; juncts associated with ALL juncts of any theorem which was
  ;; triggered by a new formula.
  (do* ((theorem *new-t-init-alist* (cdr theorem))
	;; Beware, old-theorem is altered in this loop.
	(old-theorem (assoc (caar theorem) *t-init-alist*)
		     (assoc (caar theorem) *t-init-alist*))
	(merge (list (list (caar theorem)))
	       (if theorem (cons (list (caar theorem)) merge) merge))
	(rel (list (list (caar theorem)))
	     (if theorem (cons (list (caar theorem)) rel) rel)))
      ((null theorem) (setq *merge-t-init-alist* (nreverse merge)
			    *rel-t-init-alist* rel))
    ;; Go through all the juncts of the *new-t-init-alist* theorem.
    ;; Update merge and t-init-alist.
    (if old-theorem
	(do* ((juncts (reverse (cdar theorem)) (cdr juncts))
	      ;; Beware, old-junct is altered in this loop.
	      (old-junct (assoc (caar juncts) (cdr old-theorem))
			 (assoc (caar juncts) (cdr old-theorem)))
	      ;; REMOVE copies so the result does not share structure.
	      (old-juncts (remove old-junct (cdr old-theorem))
			  (delete old-junct old-juncts)))
	    ((null juncts)
	     ;; The merge should contain the other juncts at the end.
	     ;; As should rel.
	     (setf (cdr (last (car merge))) old-juncts)
	     ;; *rel* does not need to contain these since they will
	     ;; never be "passed" in MAKE-OPTIONS.
;;;	     (setf (cdr (last (car rel))) old-juncts)
	     )
	  (if old-junct
	      (let ((insert (concatenate 'list (car juncts) (cdr old-junct))))
		;; If *t-init-alist* has more information on it
		(push insert (cdar merge))
		(push (cons (caar juncts) (cdr old-junct)) (cdar rel))
		(setf (cdr old-junct) (cdr insert)))
	    (progn (push (car juncts) (cdar merge))
		   (push (car juncts) (cdr old-theorem)))))
      (progn (push (car theorem) *t-init-alist*)
	     ;; The copy-list is needed because *new-t-init-alist* is
	     ;; used below.
	     (setf (cdar merge) (nconc (copy-list (cdar theorem))
				       (cdar merge))))))
  ;; This is the updated *t-init-alist* including new t-inits.
  (setf (t-init-alist proof) *t-init-alist*)
  ;; For each new init, this will create an option and extend it along
  ;; the lines of the old (and new) inits for the same theorem.
  (mapl
   #'(lambda (theorem m-theorem &aux (apps-to-branch
				      (get-applications-of-thm-to-branch-up
				       proof (caar theorem)))
		      (rel-all-js (cdr (assoc (caar theorem)
					      *rel-t-init-alist*))))
       ;; There will be at least BAD-JUNCTS bad juncts in a new
       ;; option.  (But consider =options).  Rerank *options* which
       ;; have (caar theorem) as their theorem.  I will re-sort them
       ;; later.  I remove those which have already been applied.
       ;; 
       ;; Why not do all of this later outside this loop (or earlier?)
       (mapc #'(lambda (o) (and (eq (caar theorem) (option-theorem o))
				(rate-option-temp o proof apps-to-branch)))
	     *options*)
       (do ((t-juncts (cdar theorem) (cdr t-juncts))
	    (m-juncts (cdar m-theorem) (cdr m-juncts))
	    (rel-passed-js
	     nil (cons (assoc (caar t-juncts) rel-all-js) rel-passed-js))
	    (bad-juncts 0 (1+ bad-juncts)))
	   ((or (> bad-juncts *juncts-max*) (null t-juncts)))
	 (do ((inits (cdar t-juncts) (cdr inits))) ((null inits))
	   (multiple-value-bind (option old) (make-new-option (car inits))
;;;	     (if old
		 ;; An option has already been created for this
		 ;; t-init.  But it may not be in the options of this
		 ;; proof!  How about just find all of the
		 ;; non-redundant descendants of option and make sure
		 ;; they are in *options*?
		 ;; So, options need to keep track of their descendants.

	     ;; Here the SUBST of option may have theorem vars anywhere.
	     ;; Since there are t-inits formed from =hyps, I need this:
	     (if (t-init-formulas (car inits))
		 (mapc
		  #'(lambda (f)
		      (if (sign f)
			  (progn
			    (push f (option-good-hyps option))
			    (setf (option-bad-goals option)
			      ;; Keep track of length of
			      ;; kb-conclusions and kb-hypotheses in
			      ;; kb-theorems so that I don't have to
			      ;; measure length here.
			      (kb-conclusions (caar theorem))
			      (option-bad-hyps option)
			      (remove (caar t-juncts)
				      (kb-hypotheses (caar theorem))
				      :test #'eq)
			      (option-baddies option)
			      (+ (length (option-bad-hyps option))
				 (length (option-bad-goals option)))))
			(progn
			  (push f (option-good-goals option))
			  (setf (option-bad-hyps option)
			    (kb-hypotheses (caar theorem))
			    (option-bad-goals option)
			    (remove (caar t-juncts)
				    (kb-conclusions (caar theorem))
				    :test #'eq)
			    (option-baddies option)
			    (+ (length (option-bad-hyps option))
			       (length (option-bad-goals option)))))))
		  (t-init-formulas (car inits)))
	       (setf (option-bad-goals option)
		 (kb-conclusions (caar theorem))
		 (option-bad-hyps option)
		 (remove (caar t-juncts) (kb-hypotheses (caar theorem)))
		 (option-goodies option) 0 (option-baddies option)
		 (+ (length (option-bad-hyps option))
		    (length (option-bad-goals option)))))
	     (bt-unify-kb5 (append (cdr m-juncts) rel-passed-js)
			   option proof bad-juncts nil apps-to-branch)
	     ;; Check for redundancy and push into proof's options in
	     ;; order of BADDIES.
	     (when (and (not (redundant-option option nil))
			;; should a silliness check be here?
			(formula-set-applicable option proof)
			(not (zerop (option-goodies option)))
			(non-silly-option option))
	       (setf (option-proof option)
		 (let ((list (append (option-good-goals option)
				     (option-good-hyps option))))
		   (if list
		       (do ((forms list (cdr forms))
			    (num (birth-place (car list))
				 (let ((new-num (birth-place (car forms))))
				   (if (> (depth num) (depth new-num)) num
				     new-num))))
			   ((null forms) num)) *proof*)))
	       (rate-option option proof apps-to-branch)
	       ;; Put this option in the options 
	       (do ((p proof (superproof p)))
		   ((eq p (option-proof option))
		    (when (options p)
		      (if (> (option-rank option)
			     (option-rank (car (options p))))
			  (push option (options p))
			(do ((ops (options p) (cdr ops)))
			    ((or (null (cdr ops))
				 (> (option-rank option)
				    (option-rank (cadr ops))))
			     (push option (cdr ops)))))
;;;		      (push option (options p))
		      ) t)
		 (when (options p)
		   (if (> (option-rank option)
			  (option-rank (car (options p))))
		       (push option (options p))
		     (do ((ops (options p) (cdr ops)))
			 ((or (null (cdr ops))
			      (> (option-rank option)
				 (option-rank (cadr ops))))
			  (push option (cdr ops)))))
;;;		   (push option (options p))
		   ))
	       (push option *options*))))))
   *new-t-init-alist* *merge-t-init-alist*)
  ;; Rationale: Imagine the scenario: I make options on a branch B.
  ;; Later, I add to the new-hyps-above at HB.  I later use that info
  ;; to amke more options and add them at B.  Now when I add more
  ;; new-hyps-above to B, the ones that exist there already remain and
  ;; will be used again to make duplicate ints.  How to stop it?
  ;; Well, that is the only way this can happen (i think).  Thus, if I
  ;; make more options on a leaf already having options, then clear
  ;; the new-hyps-above and new-goals-above at that time.
  ;; 
  ;; I can clear these because any option which is formed using only
  ;; the new-juncts-above is not already in the options slot (see
  ;; above).  This also prevents multiple "equal" options from being
  ;; created on different branches.
  (do ((p proof (superproof p))) ((null p))
    (and (options p) (setf (new-hyps-above p) nil (new-goals-above p) nil)))
  (setf (options proof)
    (sort *options*
	  #'(lambda (o1 o2)
	      (> (+ (option-perm-rank o1) (option-temp-rank o1))
		 (+ (option-perm-rank o2) (option-temp-rank o2)))))))

(defvar *new-options* (make-hash-table :test #'eq))

(defun make-new-option (t-init)
  ;; Until I get the code for dealing with old option written, I need
  ;; this to make a new one.
;;;  (multiple-value-bind (val win) (gethash t-init *new-options*)
;;;    (if win (values val t)
;;;      (setf (gethash t-init *new-options*)
  (make-option :theorem (t-init-theorem t-init) :t-inits (list t-init)
	       :subst (t-init-subst t-init)))

;;; What if a new init of the second junct allows an old init in the
;;; first junct to form a great option?  Well, it will not find that
;;; option because it does not go back over the old inits of the
;;; previous junct.  Solution, keep the tails of merge separate and
;;; use them with the new inits of subsequent juncts.

(defun bt-unify-kb5 (t-juncts option proof bad-juncts new-options
			      apps-to-branch)
  ;; There will be at least BAD-JUNCTS bad-juncts in option.  I want
  ;; this to go through those in *t-init-alist* also, not just those
  ;; in t-juncts which all came from *new-t-init-alist*.  I want this
  ;; so that the new ones will also find their combinations with the
  ;; old ones.
  (do ((t-junct t-juncts (cdr t-junct)))
      ((or (null t-junct) (> bad-juncts *juncts-max*)))
    (do ((inits (cdar t-junct) (cdr inits))) ((null inits) (incf bad-juncts))
      (and *error-checking*
	   (not (eq (option-theorem option) (t-init-theorem (car inits))))
	   (error "The theorem slot of ~s should be ~s." (car inits)
		  (option-theorem option)))
      ;; Here, I need t-init-subst to have theorem variables, and they
      ;; do.
      (let ((compat (compatible-substitutions (t-init-subst (car inits))
					      (option-subst option))))
	(and (listp compat)
	     (let ((new-option
		    (o-combine (car inits) (copy-option option) compat)))
	       ;; This has to be done AFTER the copy is made.
	       (setf (option-redundant option) t)
	       (bt-unify-kb5 (cdr t-junct) new-option proof bad-juncts
			     new-options apps-to-branch)
	       (when (and (not (redundant-option new-option new-options))
			  (non-silly-option new-option)
			  (formula-set-applicable new-option proof))
		 ;; This is right.
		 (push new-option new-options)
		 (and (not (zerop (option-goodies new-option)))
		      (setf (option-proof new-option)
			(let ((list (append (option-good-goals new-option)
					    (option-good-hyps new-option))))
			  (if list
			      (do ((forms list (cdr forms))
				   (num (birth-place (car list))
					(let ((new-num (birth-place
							(car forms))))
					  (if (> (depth num) (depth new-num))
					      num new-num))))
				  ((null forms) num)) *proof*)))
		      (rate-option new-option proof apps-to-branch)
		      (do ((p proof (superproof p)))
			  ((eq p (option-proof new-option))
			   (when (options p)
			     (if (> (option-rank new-option)
				    (option-rank (car (options p))))
				 (push new-option (options p))
;;; This ranking is not needed: go back to the push below ---bps
;;; The same goes for the one below and the ones in the calling function.
			       (do ((ops (options p) (cdr ops)))
				   ((or (null (cdr ops))
					(> (option-rank new-option)
					   (option-rank (cadr ops))))
				    (push new-option (cdr ops)))))
;;;			     (push new-option (options p))
				    ) t)
			(when (options p)
			  (if (> (option-rank new-option)
				 (option-rank (car (options p))))
			      (push new-option (options p))
			    (do ((ops (options p) (cdr ops)))
				((or (null (cdr ops))
				     (> (option-rank new-option)
					(option-rank (cadr ops))))
				 (push new-option (cdr ops)))))
;;;			  (push new-option (options p))
			  ))
		      (push new-option *options*)))))))))

(defun redundant-option (option new-options)
  (or (option-redundant option)
      (some #'(lambda (o) (subsetp (option-t-inits option) (option-t-inits o)))
	    new-options)))

(defun o-combine (init option subst)
  ;; if there is no formula then it is a =t-init.
  (if (t-init-formulas init)
      (mapc #'(lambda (f)
		(if (sign f)
		    ;; This is ok because a copy was made.
		    (progn (push f (option-good-hyps option))
			   (incf (option-goodies option))
			   (setf (option-subst option)
			     subst (option-bad-hyps option)
			     ;; But remove must be used below because
			     ;; list structure is still shared.
			     (remove (t-init-thm-junct init)
				     (option-bad-hyps option) :test #'eq)))
		  (progn
		    (push f (option-good-goals option))
		    (incf (option-goodies option))
		    (setf (option-subst option) subst (option-bad-goals option)
			  (remove (t-init-thm-junct init)
				  (option-bad-goals option) :test #'eq)))))
	    (t-init-formulas init))
    (setf (option-subst option) subst (option-bad-hyps option)
	  (remove (t-init-thm-junct init) (option-bad-hyps option))))
  (setf (option-redundant option) nil)
  (decf (option-baddies option))
  (push init (option-t-inits option))
  option)

(defun match-form-with-theorem (form theorem &aux success)
  ;; Redo this to use success as the = version does it.
  (push (list theorem) (t-inits form))
  (mapc
   #'(lambda (kb-junct &aux (f-c (unify-formulas
				  (formula form) kb-junct (bindings form))))
       (when (listp f-c)
	 (setq success t)
	 (push (list kb-junct
		     (make-t-init
		      ;; This subst may (and should) have some theorem
		      ;; variables
		      :subst f-c :theorem theorem :formulas (list form) :silly
		      (and (member (car kb-junct) '(= a-member-of))
			   (atom (cadr kb-junct)) (atom (caddr kb-junct)))
		      :thm-junct kb-junct))
	       (cdar (t-inits form)))))
   (if (sign form) (kb-hypotheses theorem) (kb-conclusions theorem)))
  (if success (copy-thm-to-kb-forms (car (t-inits form)))
    ;; I want to pop the (list theorem) back out and return NIL.
    (and (pop (t-inits form)) nil)))

(labels ((used-t-init (init) (some #'used (t-init-formulas init))))
  (defun copy-to-kb-forms-and-remove-used (t-init-alist)
    (mapcar
     #'(lambda (thm)
	 (cons (car thm)
	       (mapcar #'(lambda (junct)
			   (cons (car junct)
				 (copy-list
				  (remove-if #'used-t-init (cdr junct)))))
		       (cdr thm)))) t-init-alist))
  
  (defun copy-thm-to-kb-forms (thm)
    (cons (car thm)
	  (mapcar #'(lambda (tj)
		      (cons (car tj) (copy-list
				      (remove-if #'used-t-init (cdr tj)))))
		  (cdr thm)))))

;;; It is possible to call this with a formula which has already
;;; searched the KB.  The formula may have been used in a search on
;;; another branch.
(defun fix-t-init-alist (form)
  ;; *new-t-init-alist* is of the form:
  ;; ( ( thm . ( ( thm-junct . ( thm-init thm-init ... ) ) ... ) ) ...)
  ;; where a thm-init is a structure with slots : subst, formula,
  ;; thm-junct, theorem.
  (if (kb-searched form)
      ;; For each theorem matched by this formula, add the information
      ;; from this formula to *new-t-init-alist*.
      (mapc
       #'(lambda (f-t &aux (f-theorem (copy-thm-to-kb-forms f-t))
		      (n-theorem (assoc (car f-theorem) *new-t-init-alist*)))
	   (if n-theorem
	       (mapc #'(lambda (f-t-junct) (patch-in n-theorem f-t-junct))
		     (cdr f-theorem))
	     (push f-theorem *new-t-init-alist*))) (t-inits form))
    ;; Otherwise, search the KB with this formula.
    (mapc
     #'(lambda (kb-thm &aux (f-theorem
			     ;; Do not even try if this formula exists
			     ;; because of KB-THM.
			     (and (not (member kb-thm (theorems form)))
				  (match-form-with-theorem form kb-thm)))
		       (n-theorem (and f-theorem
				       (assoc kb-thm *new-t-init-alist*))))
	 (if n-theorem
	     (mapc #'(lambda (f-t-junct) (patch-in n-theorem f-t-junct))
		   ;; I'm thinking now that there can be more than one
		   ;; new t-init here.  Ok, there can be more than one
		   ;; junct, but for each junct there can be only one
		   ;; new t-init.
		   (cdr f-theorem))
	   (and f-theorem (push f-theorem *new-t-init-alist*))))
     *kb-theorems*))
  (setf (kb-searched form) t))

(defun match=with-theorem (theorem &aux (success (list theorem)))
  (mapc #'(lambda (kb-junct &aux (f-c (if (eq '= (car kb-junct))
					  (unify-terms (cadr kb-junct)
						       (caddr kb-junct) nil)
					:fail)))
	    (when (listp f-c)
	      (push (list kb-junct (make-t-init
				    ;; This subst may (and should) have
				    ;; some theorem variables
				    :subst f-c :theorem theorem
				    :thm-junct kb-junct))
		    (cdr success)))) (kb-hypotheses theorem))
  (and (cdr success) success))

(defun fix-t-init-alist=goal (goal new-hyps new-goals)
  ;; This adjusts *new-t-init-alist* adding t-inits which come from
  ;; the equality in goal helping formulas in new-hyps or new-goals
  ;; unify.
  )

(defun fix-t-init-alist= (&aux success)
  ;; *new-t-init-alist* is of the form:
  ;; ( ( thm . ( ( thm-junct . ( thm-init thm-init ... ) ) ... ) ) ...)
  ;; where a thm-init is a structure with slots : subst, formula,
  ;; thm-junct, theorem.
  (mapc #'(lambda (kb-thm &aux (=theorem (match=with-theorem kb-thm))
			  (n-theorem (and =theorem (setq success t)
					  (assoc kb-thm *new-t-init-alist*))))
	    (if n-theorem
		(mapc #'(lambda (=t-junct) (patch-in n-theorem =t-junct))
		      (cdr =theorem))
	      (and =theorem (push =theorem *new-t-init-alist*))))
	*kb-theorems*) success)

(defun patch-in (n-thm f-t-junct &aux
		       (n-t-junct (assoc (car f-t-junct) (cdr n-thm))))
  (if n-t-junct (push (cadr f-t-junct) (cdr n-t-junct))
    ;; I use cadr above because there can be only
    ;; one.
    (push f-t-junct (cdr n-thm))))

(defun list-equal (a b &key (test #'eql))
  (do ((a1 a (cdr a1)) (b1 b (cdr b1)))
      ((null a1) (null b1))
    (or (and b1 (funcall test (car a1) (car b1)))
	(return nil))))

;;; Do not use this for redundancy checking.  That should be checked
;;; by form-equal.  I am doing this to lose a bit more completeness
;;; and eliminate more theorem applications.
(defun formula-subsumes (a b f-cache)
  ;; This assumes that bound vars are distinct from free.
  ;; Returns a list if B is an instance of A, otherwise :fail.
  ;; F-CACHE applies to both A and B.
  (if (eq (car a) (car b))
      (case (car a)
	((and or if iff implies not)
	 (do ((sub nil nil)
	      (a1 (cdr a) (mapcar #'(lambda (f) (instantiate-formula f sub))
				  (cdr a1)))
	      (b1 (cdr b) (mapcar #'(lambda (f) (instantiate-formula f sub))
				  (cdr b1))))
	     ((null a1) f-cache)
	   (setq sub (formula-subsumes (car a1) (car b1) f-cache))
	   (if (listp sub) (setq f-cache sub) (return :fail))))
	((forall for-some)
	 (if (= (length (cadr a)) (length (cadr b)))
	     (let (alist1 alist2)
	       (mapc #'(lambda (var1 var2 &aux (v1 (car var1)) (v2 (car var2))
				     (gen (gensym)))
			 (push (cons v1 gen) alist1)
			 (push (cons v2 gen) alist2)) (cadr a) (cadr b))
	       (formula-subsumes
		(instantiate-formula (caddr a) alist1)
		(instantiate-formula (caddr b) alist2)
		f-cache))
	   :fail))
	(t (do ((a1 (cdr a) (cdr a1)) (b1 (cdr b) (cdr b1)))
	       ((null a1) f-cache)
	     (let ((sub (term-subsumes (car a1) (car b1) f-cache)))
	       (if (listp sub) (setq f-cache sub) (return :fail)))))) :fail))

;;; Do not use this for redundancy checking.  That should be checked
;;; by term-equal.  I am doing this to lose a bit more completeness
;;; and eliminate more theorem applications.
(defun term-subsumes (a b f-cache)
  (if (consp a)
      (if (consp b)
	  (if (eq (car a) (car b))
	      (case (car a)
		((the the-class-of-all)
		 (let* ((alist1 (list (cons (caar a) (gensym))))
			(alist2 (list (cons (caar b) (cdar alist1)))))
		   (declare (list alist1 alist2))
		   (formula-subsumes
		    (instantiate-formula (cadr a) alist1)
		    (instantiate-formula (cadr b) alist2)
		    f-cache)))
		(t (do ((a1 (cdr a) (cdr a1)) (b1 (cdr b) (cdr b1)))
		       ((null a1) f-cache)
		     (let ((sub (term-subsumes (car a1) (car b1) f-cache)))
		       (if (listp sub) (setq f-cache sub) (return :fail))))))
	    :fail)			; or check grammar
	(let ((b1 (cdr (assoc b f-cache))))
	  (if b1 (term-subsumes a b1 f-cache) :fail)))
    (let ((a1 (cdr (assoc a f-cache))))
      (if a1 (term-subsumes a1 b f-cache)
	(if (consp b)
	    (let ((sub (compatible-substitutions f-cache (list (cons a b)))))
	      (if (listp sub) sub :fail))
	  (if (eq a b) f-cache
	    (let ((sub (compatible-substitutions f-cache (list (cons a b)))))
	      (if (listp sub) sub :fail))))))))

(defun term-depth (term)
  (if (consp term)
      (1+ (reduce #'max (cons 0 (mapcar #'term-depth (cdr term))))) 0))

(defun match-p (f-cache)
  (notany #'(lambda (pair) (order (car pair))) f-cache))

(defun get-applications-of-thm-to-branch-up (proof theorem)
  (and proof (append (get-applications-of-thm-to-branch-up
		      (superproof proof) theorem)
		     (cdr (assoc theorem (applications-to-branch proof))))))

(defun formula-set-applicable (option proof)
  ;; This returns non-nil if the theorem is still applicable.  In
  ;; particular, if none of the previous applications of this theorem
  ;; used the same formulas.
  (let* ((goals (option-good-goals option)) (hyps (option-good-hyps option))
	 (val (notany
	       #'(lambda (op &aux (gs (option-good-goals op))
			     (hs (option-good-hyps op)))
		   ;; It would be nice if the formulas were
		   ;; ordered so we could test with EQUAL.
		   (and (subsetp goals gs :test #'form-equal :key #'formula)
			(subsetp gs goals :test #'form-equal :key #'formula)
			(subsetp hyps hs :test #'form-equal :key #'formula)
			(subsetp hs hyps :test #'form-equal :key #'formula)))
	       (get-applications-of-thm-to-branch-up
		proof (option-theorem option)))))
;;;    (and *error-checking* (not val)
;;;	 (warn-user "formula-set-applicable was useful"))
    val))

