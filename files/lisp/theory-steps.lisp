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

;;; This file applies the epsilon rule currently.  I.e., it picks an
;;; option and brings the unmatched formulas into the tableau.  This
;;; is like a beta-rule in a clausal-tableaux prover or an Ext rule in
;;; hyper tableaux.

(defun apply-next-theorem-step (proof)
  ;; This should return non-nil iff it successfully applies a theorem.
  ;; If there is a iota term in the bad juncts, then adjust *iotas*.
  ;; I might like this to find all leaves of the highest sequent
  ;; where the next theorem applies.
  (apply-next-theorem3 proof))

(defun apply-next-theorem3 (proof)
  ;; I might like this to find all leaves of the highest sequent where
  ;; the next theorem applies.  The disadvantage to this is that the
  ;; check for duplication (one of the restrictions) must be checked
  ;; on each leaf.
  ;; 
  ;; At this point proof must have something in the theorems-to-apply
  ;; slot in particular, a list of options.  Pop the first one and
  ;; apply it.
  ;;
  ;; This option-proof is really not needed.  PROOF must have OPTIONS
  ;; since this is called by find-and-apply-best which sets the
  ;; OPTIONS slot of PROOF.
  (let ((option-proof (do ((p proof (superproof p))) ((null p))
			(and (options p) (return p)))))
    (and (null option-proof) (return-from apply-next-theorem3 nil))
    ;; This applies all 0-options if there are any, otherwise, it
    ;; applies the first option.  It returns T since option-proof must
    ;; have options in it and so something must be applied.
    (do ((option (pop (options option-proof))
		 (and (options option-proof)
		      (zerop (option-baddies (car (options option-proof))))
		      (pop (options option-proof)))))
	((null option) t)
      ;; Taking into account the branch we are on, re-rank and sort
      ;; the options.  Do not rank options with N bad-js if there
      ;; is no way they could beat some option ranked already.
      ;; They are already in order by badies.
      (apply-option option))))

(defun apply0options (proof)
  (and (null (options proof)) (return-from apply0options nil))
  ;; This applies all 0-options if there are any, otherwise, it
  ;; applies the first option.  It returns T since proof must
  ;; have options in it and so something must be applied.
  (do ((option (and (zerop (option-baddies (car (options proof))))
		    (pop (options proof)))
	       (and (options proof)
		    (zerop (option-baddies (car (options proof))))
		    (pop (options proof)))))
      ((null option) t)
    ;; Taking into account the branch we are on, re-rank and sort
    ;; the options.  Do not rank options with N bad-js if there
    ;; is no way they could beat some option ranked already.
    ;; They are already in order by badies.
    (setf (option-subst option)
      (compatible-substitutions
       (mapcar #'(lambda (var) (cons var (make-parameter :ext-name var)))
	       (delete-duplicates
		(orderless-vars-in-cdrs (option-subst option))))
       (option-subst option)))
    (setf (option-bindings option)
      (remove-if-not #'(lambda (p) (order (car p))) (option-subst option)))
    (theorem-success
     (option-proof option) option
     `((apply-theorem
	,(externalize-f-cache (option-bindings option) t)
	,(kb-theorem-name (option-theorem option))
	,(let ((lis (mapcar #'(lambda (f) (externalize-formula (formula f)))
			    (option-good-goals option))))
	   (if (cdr lis) `(or ,@lis) (car lis)))
	,(let ((lis (mapcar #'(lambda (f) (externalize-formula (formula f)))
			    (option-good-hyps option))))
	   (if (cdr lis) `(and ,@lis) (car lis))) nil nil)))
    (record-application option (option-proof option))
    (when (and *equality* *new-equals*)
      (new-equality-formula) (setq *new-equals* nil))))

;;; The PROOF arg may be used if I decide only to apply split on a
;;; single branch.
(defun apply-option (option)
  (let* ((gls (length (option-bad-goals option)))
	 (hyps (length (option-bad-hyps option)))
	 ;; The cdr gets rid of the 'AND.  This should make theorem
	 ;; vars into *params.
	 (bad-juncts
	  ;; This method avoids making parameters for variables which
	  ;; will never occur in the sequent.
	  ;; 
	  ;; First, instantiate the bad-formulas with the old
	  ;; option-subst.
	  (let* ((bjs (internalize-theorem-formula
		       ;; The above call internalizes bound vars so
		       ;; the below must get all bound vars into the
		       ;; formulas.
		       (instantiate-formula
			(cons 'and (append (option-bad-goals option)
					   (option-bad-hyps option)))
			(option-subst option))))
		 ;; Second, make an alist of new parameters for the
		 ;; theorem variables in the BJS and in the old
		 ;; option-subst.
	    	 (new-var-alist
		  ;; alist of old-name, new-internal-name pairs for
		  ;; each theorem variable in the bad-formulas or in
		  ;; the option-subst.
		  ;; 
		  ;; First, get all vars which have no order.  Then
		  ;; make params for each of them.
		  (mapcar
		   #'(lambda (var) (cons var (make-parameter :ext-name var)))
		   (delete-duplicates
		    (nconc (orderless-vars-in-formula bjs nil)
		      (orderless-vars-in-cdrs (option-subst option)))))))
	    ;; Finally, apply that substitution to the option-subst.
	    (setf (option-subst option)
	      (compatible-substitutions new-var-alist (option-subst option)))
	    ;; and to the BJS.
	    (cdr (instantiate-formula bjs new-var-alist))))
	 (bad-hyps (nthcdr gls bad-juncts))
	 (bad-goals (nbutlast bad-juncts hyps)))
    ;; These no longer contain any theorem variables.  But
    ;; option-bindings may since it was formed before the new vars
    ;; were created.
    (setf (option-bindings option)
      (remove-if-not #'(lambda (p) (order (car p))) (option-subst option)))
    (if (or (and bad-hyps bad-goals) (cdr bad-goals) (cdr bad-hyps))
	;; I might like this to find all leaves of the highest sequent
	;; where the next theorem applies.  The disadvantage to this
	;; is that the check for duplication (one of the restrictions)
	;; must be checked on each leaf.  But here are some
	;; advantages: 1> since the application of the theorem is
	;; recorded as high as possible, later applications of the
	;; theorem on other branches where it is equally applicable
	;; will not happen since it has already been applied.  One way
	;; to fix this problem is to feed the proper proof to the
	;; record-application so that it only records the application
	;; on the branches where it has been applied.  The further
	;; trouble with this is that the option is then removed and
	;; marked applied.  So for now, let's do it to all leaves.
	(progn
	  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
				(format t "~%S") (format t "S")))
	  (if *high-splitting*
	      (aif (variables-involved-in-theorem-split
		    (append bad-goals bad-hyps))
		   (mapc #'(lambda (p) (split-proof-with-theorem9
					p option bad-goals bad-hyps aif-it))
			 (leaves (option-proof option)))
		   (split-proof-with-theorem-high option bad-goals bad-hyps))
	    (mapc #'(lambda (p) (split-proof-with-theorem9
				 p option bad-goals bad-hyps nil))
		  (leaves (option-proof option)))))
      (progn (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
				   (format t "~%P") (format t "P")))
	     (apply-option9 (option-proof option) option bad-goals bad-hyps)))
;;; How can a theorem application introduce an alpha?  It can't.
;;;	     (when *new-alpha*
;;;	       (reduce-formula (car (subproofs (option-proof proof))))
;;;	       (setq *new-alpha* nil))
    (record-application option (option-proof option))
    (when (and *equality* *new-equals*)
      (new-equality-formula) (setq *new-equals* nil))))

(defun orderless-vars-in-formula (form list)
  ;; LIST is the list of bound vars
  (case (car form)
    ((and or if iff not implies)
     (mapcan #'(lambda (f) (orderless-vars-in-formula f list)) (cdr form)))
    ((forall for-some) (orderless-vars-in-formula
			(caddr form) (append list (mapcar #'car (cadr form)))))
    (t (mapcan #'(lambda (trm) (orderless-vars-in-term trm list))
	       (cdr form)))))

(defun orderless-vars-in-term (term list)
  (if (consp term)
      (case (car term)
	((the the-class-of-all)
	 (orderless-vars-in-formula (caddr term) (cons (caadr term) list)))
	(t (mapcan #'(lambda (trm) (orderless-vars-in-term trm list))
		   (cdr term))))
    (and (not (order term))
	 (not (member term list))
	 (list term))))

(defun orderless-vars-in-cdrs (subst)
  (do ((pair subst (cdr pair))
       (return nil (append (orderless-vars-in-term (cdar pair) nil) return)))
      ((null pair) return)))

(defun variables-involved-in-theorem-split (forms)
  (do ((f forms (cdr f)) (vars nil (append vars n-vars))
       (n-vars (unbound-vars-in-formula (cadr forms))
	       (unbound-vars-in-formula (cadr f)))
       (s-vars nil s-vars)) ((null (cdr f)) s-vars)
    (let ((t-vars vars))
      (setq vars nil)
      (mapc #'(lambda (v) (if (member v n-vars :test #'eq)
			      (pushnew v s-vars :test #'eq)
			    (pushnew v vars :test #'eq))) t-vars))))

(defun apply-option9 (proof option bad-goals bad-hyps)
  ;; Is this being applied as high as possible?  Yes, PROOF should now
  ;; be the highest possible place to apply this.
  (let* ((bindings (option-bindings option))
	 (theorems (remove-duplicates
		    (append (mappend #'theorems (option-good-goals option))
			    (mappend #'theorems (option-good-hyps option)))))
	 (plan `((apply-theorem
		  ,(externalize-f-cache bindings t)
		  ,(kb-theorem-name (option-theorem option))
		  ,(let ((lis (mapcar #'(lambda (f)
					  (externalize-formula (formula f)))
				      (option-good-goals option))))
		     (if (cdr lis) `(or ,@lis) (car lis)))
		  ,(let ((lis (mapcar #'(lambda (f)
					  (externalize-formula (formula f)))
				      (option-good-hyps option))))
		     (if (cdr lis) `(and ,@lis) (car lis)))
		  ,(when bad-goals (externalize-formula (car bad-goals)))
		  ,(when bad-hyps (externalize-formula (car bad-hyps)))))))
    (cond (bad-hyps
	   (let* ((bad-hyp-obj
		   (make-formula
		    (car bad-hyps) :bindings bindings :theorems
		    ;; this theorem must be new to this list,
		    ;; otherwise, this theorem would not be being
		    ;; applied.
		    (cons (option-theorem option) theorems) :splits
		    (remove-duplicates
		     (reduce #'append
			     (mapcar #'splits
				     (append (option-good-goals option)
					     (option-good-hyps option)))))
		    :parents-of (append (option-good-goals option)
					(option-good-hyps option))))
		  (sub (make-sequent
			:new-goals (list bad-hyp-obj) :plan (plan proof)
			:whither (whither proof) :subproofs (subproofs proof)
			:superproof proof
			:depth (1+ (depth proof)))))
	     (set-splits proof sub)
	     (setf (birth-place bad-hyp-obj) sub)
	     (mapc #'(lambda (pr)
		       (setf (superproof pr) sub)
		       (map-proof
			#'(lambda (p) (incf (depth p))
			    (when (options p)
			      (push bad-hyp-obj (new-goals-above p)))) pr))
		   (subproofs sub))
	     (setf (plan proof) plan)
	     (setf (whither proof) :math)
	     (setf (subproofs proof) (list sub))))
	  (bad-goals
	   (let* ((bad-goal-obj
		   (make-formula
		    (car bad-goals) :bindings bindings :sign t :theorems
		    ;; this theorem must be new to this list,
		    ;; otherwise, this theorem would not be being
		    ;; applied.
		    (pushnew (option-theorem option) theorems) :splits
		    (remove-duplicates
		     (reduce #'append
			     (mapcar #'splits
				     (append (option-good-goals option)
					     (option-good-hyps option)))))
		    :parents-of (append (option-good-goals option)
					(option-good-hyps option))))
		  (sub (make-sequent
			:new-hyps (list bad-goal-obj) :superproof proof
			:plan (plan proof) :whither (whither proof)
			:subproofs (subproofs proof)
			:depth (1+ (depth proof)))))
	     (setf (birth-place bad-goal-obj) sub)
	     (set-splits proof sub)
	     (mapc #'(lambda (pr)
		       (setf (superproof pr) sub)
		       (map-proof
			#'(lambda (p) (incf (depth p))
			    (when (options p)
			      (push bad-goal-obj (new-hyps-above p)))) pr))
		   (subproofs sub))
	     (setf (plan proof) plan)
	     (setf (whither proof) :math)
	     (setf (subproofs proof) (list sub))))
	  (t (theorem-success proof option plan)))))

(defun split-proof-with-theorem9 (proof option bad-goals bad-hyps vars &aux
					(theorems
					 (delete-duplicates
					  (mapcar
					   #'theorems
					   (append
					    (option-good-goals option)
					    (option-good-hyps option))))))
  (setf (whither proof) :b)
  (let* ((parents (append (option-good-goals option)
			  (option-good-hyps option)))
	 (bindings (option-bindings option))
	 (inc 1)
	 (splits (cons (setf (split proof) (make-split :node proof :vars vars))
		       (remove-duplicates
			;; reduce instead?
			(reduce #'append (mapcar #'splits parents))))))
    (setf (plan proof)
      `((apply-theorem-split
	 ,(externalize-f-cache bindings t)
	 ,(kb-theorem-name (option-theorem option))
	 ,(let ((lis (mapcar #'(lambda (f) (externalize-formula (formula f)))
			     (option-good-goals option))))
	    (if (cdr lis) `(or ,@lis) (car lis)))
	 ,(let ((lis (mapcar #'(lambda (f) (externalize-formula (formula f)))
			     (option-good-hyps option))))
	    (if (cdr lis) `(and ,@lis) (car lis)))
	 ,(if (cdr bad-goals)
	      `(or ,@(mapcar #'externalize-formula bad-goals))
	    (and bad-goals (externalize-formula (car bad-goals))))
	 ,(if (cdr bad-hyps)
	      `(and ,@(mapcar #'externalize-formula bad-hyps))
	    (and bad-hyps (externalize-formula (car bad-hyps)))))))
    (setf (subproofs proof)
      (append
       (mapcar
	#'(lambda (f &aux (form (make-formula
;;; no universals since we're splitting
				 f :bindings bindings :sign t :theorems
				 (cons (option-theorem option) theorems)
				 :parents-of parents :splits splits))
		     (sub (make-sequent
			   :new-hyps (list form) :superproof proof
			   :depth (1+ (depth proof)) :index inc)))
	    (incf inc)
	    (setf (birth-place form) sub))
	bad-goals)
       (mapcar
	#'(lambda (f &aux (form (make-formula
				 f :bindings bindings :parents-of parents
				 :splits splits :theorems
				 (cons (option-theorem option) theorems)))
		     (sub (make-sequent
			   :new-goals (list form) :superproof proof
			   :depth (1+ (depth proof)) :index inc)))
	    (incf inc)
	    (setf (birth-place form) sub))
	bad-hyps)))))

(defun split-proof-with-theorem-high
  (option bad-goals bad-hyps &aux (proof (option-proof option)))
  ;; Don't forget to mark the superproof of the new subs with the fact
  ;; that variables were involved.  I need to do the same for all
  ;; splitting rules.  Here proof is the highest point where the
  ;; option can be applied.  I need to see how low I can go before
  ;; getting to a variable split.
  (mapc #'(lambda (p) (split-proof-high p option bad-goals bad-hyps))
	(leaves-to-var-split proof)))

(defun split-proof-high (proof option bad-goals bad-hyps)
  ;; Here proof is the place where I want to apply the theorem.  In
  ;; this version, I need to make copies of the subproofs of proof.
  (let* ((parents (append (option-good-goals option)
			  (option-good-hyps option)))
	 (bindings (option-bindings option))
	 (theorems (delete-duplicates
		    (cons (option-theorem option)
			  (append (mappend #'theorems
					   (option-good-goals option))
				  (mappend #'theorems
					   (option-good-hyps option))))))
	 (inc 1)
	 (splits (cons (make-split :node proof)
		       (remove-duplicates
			(reduce #'append (mapcar #'splits parents)))))
	 (copies1 (let (cops)
		    (dotimes (incer (1- (length bad-goals))
				    (cons (cons (split proof)
						(subproofs proof)) cops))
		      (push (copy-subproofs proof) cops))))
	 ;; if the above is non-empty, then the below should be all
	 ;; copies.
	 (copies2
	  (let (cops)
	    (if copies1 (dotimes (incer (length bad-hyps) cops)
			  (push (copy-subproofs proof) cops))
	      (dotimes (incer (1- (length bad-hyps))
			      (cons (cons (split proof) (subproofs proof))
				    cops))
		(push (copy-subproofs proof) cops)))))
	 (subs
	  (append
	   (mapcar
	    #'(lambda (f copy)
		(let* ((form (make-formula
			      f :bindings bindings :sign t :theorems theorems
			      :parents-of parents :splits splits))
		       (sub (make-sequent
			     :new-hyps (list form) :superproof proof
			     :subproofs (cdr copy) :depth (1+ (depth proof))
			     :whither (whither proof) :plan (plan proof)
			     :index inc)))
		  (incf inc)
		  (setf (birth-place form) sub)
		  (mapc #'(lambda (pr) (setf (superproof pr) sub)
			    (map-proof
			     #'(lambda (p)
				 (when (options p)
				   (push form (new-goals-above p)))
				 (incf (depth p))) pr))
			(subproofs sub))
		  ;; this gives each of the subs the same split...
		  ;; No, this is bad because it alters the (split proof)
		  ;; so that on the next iteration it's messed up.
		  (and (car copy) (setf (split sub) (car copy)
					(split-node (car copy)) sub))
		  (setf (split proof) (car splits))
		  sub)) bad-goals copies1)
	   (mapcar
	    #'(lambda (f copy)
		(let* ((form (make-formula
			      f :bindings bindings :parents-of parents
			      :splits splits :theorems theorems))
		       (sub (make-sequent
			     :new-goals (list form) :superproof proof
			     :subproofs (cdr copy) :depth (1+ (depth proof))
			     :whither (whither proof) :plan (plan proof)
			     :index inc)))
		  (incf inc)
		  (setf (birth-place form) sub)
		  (mapc #'(lambda (pr) (setf (superproof pr) sub)
			    (map-proof
			     #'(lambda (p)
				 (when (options p)
				   (push form (new-goals-above p)))
				 (incf (depth p))) pr))
			(subproofs sub))
		  (and (car copy) (setf (split sub) (car copy)
					(split-node (car copy)) sub))
		  (setf (split proof) (car splits))
		  sub))
	    bad-hyps copies2))))
    (setf (subproofs proof) subs)
    (setf (whither proof) :b)
    (setf (plan proof)
      `((apply-theorem-split
	 ,(externalize-f-cache bindings t)
	 ,(kb-theorem-name (option-theorem option))
	 ,(let ((lis (mapcar #'(lambda (f) (externalize-formula (formula f)))
			     (option-good-goals option))))
	    (if (cdr lis) `(or ,@lis) (car lis)))
	 ,(let ((lis (mapcar #'(lambda (f) (externalize-formula (formula f)))
			     (option-good-hyps option))))
	    (if (cdr lis) `(and ,@lis) (car lis)))
	 ,(if (cdr bad-goals)
	      `(or ,@(mapcar #'externalize-formula bad-goals))
	    (and bad-goals (externalize-formula (car bad-goals))))
	 ,(if (cdr bad-hyps)
	      `(and ,@(mapcar #'externalize-formula bad-hyps))
	    (and bad-hyps (externalize-formula (car bad-hyps)))))))))

;;; This copy-option was just added for debugging.  Really I should
;;; not copy it.  Benji !@#$%^&*
(defun record-application (option proof &aux (new (copy-option option)))
  ;; Store this as high as possible.  I.e. in the proof ABOVE the new
  ;; one.
  (setf (option-applied option) t)
  (do ((apps (setf (applications-to-branch proof)
	       (copy-tree (applications-to-branch proof))) (cdr apps)))
      ((null apps) (push (list (option-theorem option) new)
			 (applications-to-branch proof)))
    (and (eq (caar apps) (option-theorem option))
	 (return (push new (cdar apps))))))

(defun internalize-theorem-formula (formula)
  ;; initialization. First, make-formula-internal: check a little
  ;; syntax and replace bound variables and collect unbound variables
  ;; into *unbound-vars* and install terms and preds. defined in kb
  ;; for use.  Second, check names of unbound vars and replace them
  ;; with skolem constants.  Return the new formula.
  (setq *unbound-vars* nil)
  (internalize-theorem-formula1 formula nil))
;;;  (if (setq *unbound-vars* (delete-duplicates *unbound-vars*))
;;;      (instantiate-formula
;;;       formula
;;;       (mapcan #'(lambda (var &aux (string (symbol-name var)))
;;;		   (and (or (< (length string) 7)
;;;			    (not (string= "*PARAM" (subseq string 0 6))))
;;;			(let ((name (make-a-new-name
;;;				     var (mapcar #'(lambda (p) (ext-name p))
;;;						 *parameters*))))
;;;			  (list (cons var (make-parameter :ext-name name
;;;							  :var name))))))
;;;	       *unbound-vars*))

(defun internalize-theorem-formula1 (formula bound-vars
					     &aux (car (car formula)) alist)
  ;; bound-vars is the INTERNAL NAMES of variables (e.g. *var1)
  ;; in the future this may also count arguments and check to see if
  ;; everything is defined.
  (declare (list formula bound-vars alist) (symbol car))
  (case car
    ((and or not iff implies if)
     `(,car ,@(mapcar #'(lambda (form) (declare (list form))
			  (internalize-theorem-formula1 form bound-vars))
		      (cdr formula))))
    ((for-some forall)
     `(,car ,(mapcar #'(lambda (var)
			 (let ((new-var (make-bound-var (car var))))
			   (push new-var bound-vars)
			   (push (cons (car var) new-var) alist)
			   (list new-var))) (cadr formula))
	    ,(internalize-theorem-formula1
	      (instantiate-formula (caddr formula) alist) bound-vars)))
    (t `(,car ,@(mapcar #'(lambda (term)
			    (internalize-theorem-term term bound-vars))
			(cdr formula))))))

(defun internalize-theorem-term (term bound-vars)
  ;; bound-vars is the internal names
  (declare (type (or symbol list) term) (list bound-vars))
  (if (consp term)
      (case (car term)
	((the-class-of-all the)
	 (let* ((var (caadr term))
		(new-var (make-bound-var var))
		(cadr (progn (push new-var bound-vars) (list new-var)))
		(caddr (replace-unbound-occurrences-in-formula
			new-var var (caddr term))))
	   (declare (symbol var new-var) (list cadr caddr))
	   `(,(car term) ,cadr ,(internalize-theorem-formula1
				 caddr bound-vars))))
	(t `(,(car term)
	     ,@(mapcar #'(lambda (trm)
			   (internalize-theorem-term trm bound-vars))
		       (cdr term)))))
    (progn (when (not (member term bound-vars)) (push term *unbound-vars*))
	   term)))

(defun theorem-success (proof option plan)
  ;; I need the PLAN argument because the plan of PROOF may not have
  ;; been set properly yet.
  (declare (type proof proof) (type option option))
  (let ((init (make-init :f-cache (option-bindings option) :used-goals
			 (option-good-goals option) :init-plan plan :used-hyps
			 (option-good-hyps option) :sequent proof)))
    (declare (type init-sub init))
    ;; This init needs to know about the option that created it so
    ;; that the plan can tell how it was closed.
    (push init (successes proof))
    (push init (theorem-successes proof))
    (when (null (f-cache init)) (push init (trivial-closures proof)))
    init))

