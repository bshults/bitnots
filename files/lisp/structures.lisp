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

;;; This file contains a lot of structure definitions and functions
;;; for dealing with them.

(defvar *location-here* '(1))	; set by emacs.
				; in the future, more than one proof
				; at a time may be supported.
				; I hope to get rid of this.
(proclaim '(list *location-here*))

(defvar *proof* (make-proof))	; this because check-formula may adjust
				; the consider slot in the rigid
				; version.  The main proof.
(proclaim '(type proof *proof*))

;;; this might do a little more error checking, or not.
;;;(defun get-proof (&optional (path *location-here*) (proof *proof*))
;;;  (declare (list path) (type proof proof))
;;;  (if proof (if (cdr path)
;;;		(get-proof (cdr path) (nth (1- (cadr path)) (subproofs proof)))
;;;	      proof) *proof*))

(defvar *done* nil)

(defun first-leaf (proof)
  (declare (type proof proof))
  (let ((next (car (subproofs proof))))
    (or (and next (first-leaf next))
	proof)))

(defun first-open-leaf (proof)
  (declare (type proof proof))
  (when (not *done*)
    (do ((next (subproofs proof) (cdr next))) ((null next) proof)
      (declare (list next))
      (when (not *done*) (return (car next))))))

(defun leaves (proof)
  ;; i may want skip to be a list of locations instead of a location.
  "returns the list of undone leaves."
  (let ((subs (subproofs proof)))
    (if subs
        (do ((proofs subs (append (subproofs (car proofs)) (cdr proofs)))
             (return nil (or (and (not (subproofs (car proofs)))
                                  (nconc return (list (car proofs))))
                             return)))
            ((null proofs) return))
      (list proof))))

(defun leaves-to-var-split (proof)
  ;; This returns a list containing proof or a list of nodes below
  ;; proof if there are nodes under proofs whose creation was not
  ;; because of a var-split.  The list returned is of the lowest such
  ;; nodes.
  (if (and (split proof) (split-vars (split proof))) (list proof)
    (let ((subs (subproofs proof)))
      (if subs (do ((proofs subs (append
				  (and (not (and (split (car proofs))
						 (split-vars
						  (split (car proofs)))))
				       (subproofs (car proofs)))
				  (cdr proofs)))
		    (return nil (if (and (split (car proofs))
					 (split-vars (split (car proofs))))
				    (nconc return (list (car proofs)))
				  (or (and (not (subproofs (car proofs)))
					   (nconc return (list (car proofs))))
				      return))))
		   ((null proofs) return))
	(list proof)))))

(defun next-sibling (proof)
  (declare (type proof proof))
  (when (superproof proof)
    (cadr (member proof (subproofs (superproof proof))))))

(defun previous-sibling (proof)
  (declare (type proof proof))
  (when (superproof proof)
    (do ((pr (subproofs (superproof proof)) (cdr pr))) ((null pr))
      (declare (list pr))
      (when (eq proof (cadr pr)) (return (car pr))))))

;;; this in-tree works up from proof
(defun in-tree-up (proof root)
  ;; If I had a depth measure, this could be ultra fast.
  (do ((p proof (superproof p))) ((null p))
    (when (eq p root) (return t))))

;;; this in-tree works up from proof up
(defun in-tree (proof root)
  (or (eq proof root)
      (properly-in-tree proof root)))

(defun properly-in-tree (proof root)
  (let ((depth (depth root)) (dep (depth proof)))
    (and (< depth dep)
	 (do ((p (superproof proof) (superproof p))
	      (d (decf dep) (decf d)))
	     ((= d depth) (eq p root))))))

(defun get-branch1 (proof)
  ;; if i decide to do away with superproof, then this could easily
  ;; be replaced by calls to get-proof.
  (when (proof-p proof)
    (cons proof (get-branch1 (superproof proof)))))

(defun as-far-down-as-possible (proof)
  ;; the highest proof under proof which has more than one open subproof.
  (declare (type proof proof))
  (if (subproofs proof)
      (if (cdr (subproofs proof)) proof
	(as-far-down-as-possible (car (subproofs proof))))))

(defun path (proof)
  (labels ((path1 (p acc &aux (sup (superproof p)))
	     (if sup
                 (aif (index p)
                      (path1 sup (cons aif-it acc))
                      (path1 sup acc))
	       acc)))
    (path1 proof nil)))

(defun to-the-left-of (s1 s2)
  ;; S1 is STRICTLY to the left of S2.
  (do ((loc1 (path s1) (cdr loc1)) (loc2 (path s2) (cdr loc2)))
      ((or (null loc1) (null loc2)) nil)
    (if (> (car loc1) (car loc2)) (return nil)
      (if (< (car loc1) (car loc2)) (return t)))))

(defun before (s1 s2)
  ;; S1 is equal to, to the left of or above S2.
  (or (eq s1 s2)
      (do ((loc1 (path s1) (cdr loc1)) (loc2 (path s2) (cdr loc2)))
	  ((null loc1) t)
	(declare (list loc1 loc2))
	(if (or (null loc2) (> (car loc1) (car loc2))) (return nil)
	  (if (< (car loc1) (car loc2)) (return t))))))

(defun all-leaves-to-the-left-of (proof)
  (leaves-of-left-of *proof* proof))

(defun leaves-of-left-of (proof right &aux return)
  (let ((subs (remove-if #'(lambda (p) (to-the-left-of right p))
			 (subproofs proof))))
    (do ((proofs subs (append (remove-if
			       #'(lambda (p) (to-the-left-of right p))
			       (subproofs (car proofs))) (cdr proofs))))
	((null proofs) return)
      (and (to-the-left-of (car proofs) right)
	   (setq return (nconc return (leaves (car proofs))))))))

(defun no-leaves-of-left-of (proof right)
  ;; This returns non-nil iff there are no leaves to the left of RIGHT
  ;; under PROOF.
  (let ((subs (remove-if #'(lambda (p) (to-the-left-of right p))
			 (subproofs proof))))
    (do ((proofs subs (append (remove-if
			       #'(lambda (p) (to-the-left-of right p))
			       (subproofs (car proofs))) (cdr proofs))))
	((null proofs) t)
      (and (to-the-left-of (car proofs) right)
	   (return nil)))))

(defun map-proof (function proof)
  (declare (type proof proof) (type function function))
  (funcall function proof)
  (let ((subs (subproofs proof)))
    (when subs
      (do ((proofs subs (append (subproofs (car proofs)) (cdr proofs))))
	  ((null proofs))
	(funcall function (car proofs))))))

(defun how-many-branches (&optional (proof *proof*))
  (declare (type proof proof))
  (length (leaves proof)))

(defun how-many-nodes (&optional (proof *proof*) &aux (num 0))
  (declare (type proof proof) (type (integer 0 *) num))
  (map-proof #'(lambda (s) (declare (ignore s)) (incf num)) proof)
  num)

(defun output-comment (proof)
  (declare (type proof proof))
  (when (superproof proof) (plan (superproof proof))))

(defun make-sequent (&key (theorem-string "") theorem-name index (depth 1)
			  plan whither involved split new-goals new-hyps
			  subproofs superproof applications-to-branch)
  (make-proof
   :depth depth :index index :split split :plan plan :whither whither
   :new-goals new-goals :new-hyps new-hyps :theorem-string theorem-string
   :involved involved :subproofs subproofs :applications-to-branch
   applications-to-branch :theorem-name theorem-name :superproof superproof))

;;; terms

;;; variables and parameters and universals

(proclaim '(type boolean *making-theorems*))

(defvar *making-theorems* nil)		; for unifier (and forcer)

(proclaim '(type boolean *unify-p*))

(defvar *unify-p* t)		; use unification?  this is turned off during
				; some dealings with kb.

(defvar *ext-names* (make-hash-table :test 'eq))
(defvar *order* (make-hash-table :test 'eq))

(defun ext-name (x)
  (gethash x *ext-names* x))

(defsetf ext-name (symbol) (new-symbol)
  `(setf (gethash ,symbol *ext-names*) ,new-symbol))

(defmacro order (x)
  `(gethash ,x *order*))

(property-macro bound)		; applied to a gentemp used temporarily in the
				; unifier to represent a bound variable.

(property-macro param)		; used by a var to know what
				; param it was given

(defvar *bound-variable-number* 0)	; length of *bound-variables*
(proclaim '(type (integer 0 *) *bound-variable-number*))
(defvar *bound-variables* nil)		; all bound-variables
(proclaim '(list *bound-variables*))

(defun make-bound-var (name &key copy)
  ;; returns the internal name of bound variables where NAME is an
  ;; external name.  This is also called from inside show-there-is
  ;; withing the copy-formula-inv within the
  ;; rename-internal-bound-vars-in-formula
  (declare (symbol name) (type boolean copy))
;;;  (when *making-theorems* (error "make-var in theorem-making process"))
  (let ((string (symbol-name name)))
    (if (and (not copy)
	     (or (char= (char string 0) #\_)
		 (and (>= (length string) 4)
		      (string= (subseq string 0 4) "*VAR"))
		 (and (>= (length string) 6)
		      (string= (subseq string 0 6) "*PARAM"))))
	(ipr-error "Illegal name of bound var: ~a" name)
      (let ((int-name (create-name
		       '*VAR (make-symbol
			      (format nil "~D"
				      (incf *bound-variable-number*))))))
	(declare (symbol int-name))
	(setf (ext-name int-name)
	  (if copy name (intern (format nil "_~a" name))))
;;;	(push int-name *bound-variables*)
;;;	(when (reject int-name) (setf (reject int-name) nil))
	int-name))))

(defun make-bound-var-with-new-name (name used-names)
;;; REMOVE-THE3 
;;; SAFELY-INTERNALIZE-TERM 
;;; SAFELY-INTERNALIZE-FORMULA 
  ;; internal name of bound variables where NAME is an external name
  ;; USED-NAMES is a list of external names to avoid
  (declare (symbol name) (list used-names))
  (let ((int-name (create-name
		   '*VAR (make-symbol (format nil "~D"
					      *bound-variable-number*)))))
    (declare (symbol int-name))
    (setf (ext-name int-name) (make-a-new-name name used-names))
    (push int-name *bound-variables*)
    (incf *bound-variable-number*)
;;;    (when (reject int-name) (setf (reject int-name) nil))
    int-name))

;;; A faster way would be to make a hash table keyed on symbols with
;;; trailing numbers cut off.  The data would be a list of numbers
;;; which have been appended to the end of that string.  I just pick
;;; the lowest number not in the list.  Maybe have the data in the
;;; table be the next number to be tried.
(defun make-a-new-name (name used-names) ; called by make-var-with-new-name
;;; MAKE-SKOLEM-FUNCTION-SYMBOL 
;;; MAKE-BOUND-VAR-WITH-NEW-NAME 
  (declare (symbol name) (list used-names))
  (if (member name used-names)
      (do* ((inc 0 (1+ inc))
	    (new-name (intern (format nil "~s~d" name inc))
		      (intern (format nil "~s~d" name inc))))
	  ((not (member new-name used-names)) new-name)
	(declare (symbol new-name) (type (integer 0 *) inc)))
    name))

;;; Write a version of the above which uses the second value of intern.

(defvar *parameters* nil)	; all parameters anywhere, this is used
				; to keep ext-names from being repeated
(defvar *parameter-number* 0)	; keeps track of order in which
				; variables were introduced
				; for the unifier.  Also
				; indicates the length of the
				; list *parameters*.
(proclaim '(list *parameters*))
(proclaim '(type (integer 0 *) *parameter-number*))

(defun make-parameter (&key ext-name var) ; universal
  ;; I think ext-name here must be a symbol.   Yes, this is only called with
  ;; a non-nil ext-name by show-there-is3 and there it must be a symbol
  ;; because that is called by unquantify which requires it.
  ;; BE sure that this resets every entry because the same symbols are
  ;; reused.
  (declare (symbol ext-name var))
  (let ((int-name
	 (create-name
	  '*PARAM (make-symbol
		   (format nil "~D" (incf *parameter-number*))))))
    (declare (symbol int-name))
    (setf (order int-name) *parameter-number*)
;;;    (when (depends int-name) (setf (depends int-name) nil))
    (setf (ext-name int-name) (get-ext-name ext-name))
;;;    (when (children int-name) (setf (children int-name) nil))
;;;    (when (parent int-name) (setf (parent int-name) nil))
;;;    (setf (s-t-i int-name) s-t-i)
    (push int-name *parameters*)
;;;    (when (reject int-name) (setf (reject int-name) nil))
;;;    (setf (force int-name) nil)
    (push int-name (param var))
    int-name))

(defvar *skolem-functions* nil)
(proclaim '(list *skolem-functions*))
(defvar *skolem-symbols-table* (make-hash-table :test #'equal))

(defun get-skolem-function-symbol (form)
  ;; takes a delta formula and figures out what skolem function symbol
  ;; it should introduce.
  (let ((hashed (hashify-form form)))
    (or (gethash hashed *skolem-symbols-table*)
	(setf (gethash hashed *skolem-symbols-table*)
	  (make-skolem-function-symbol
	   :ext-name (ext-name (caar (cadr form))))))))

(defun hashify-form (form)
  form)
;;;  (car (hashify-form1 form nil 0)))

(defun hashify-form1 (form alist num)
  (declare (integer num))
  (case (car form)
    ((and if iff or not implies)
     (do* ((f (cdr form) (cdr f))
	   (new (hashify-form1 (car f) alist num)
		(hashify-form1 (car f) assoc inc))
	   (cdr (list (car new)) (cons (car new) cdr))
	   (inc (caddr new) (caddr new))
	   (assoc (cadr new) (cadr new)))
	 ((null f) (list (cons (car form) (nreverse cdr)) assoc inc))))
    ((forall for-some)
     (let* ((cadr
	     (mapcar
	      #'(lambda (v &aux (var (intern (format nil "__v~d" (incf num)))))
		  (setq alist (acons (car v) var alist))
		  (list var))
	      (cadr form)))
	    (new (hashify-form1 (caddr form) alist num)))
       (list (cons (car form) (cons cadr (car new))) (cadr new) (caddr new))))
    (t
     (do* ((f (cdr form) (cdr f))
	   (new (hashify-term1 (car f) alist num)
		(hashify-term1 (car f) assoc inc))
	   (cdr (list (car new)) (cons (car new) cdr))
	   (inc (caddr new) (caddr new))
	   (assoc (cadr new) (cadr new)))
	 ((null f) (list (cons (car form) (nreverse cdr)) assoc inc))))
    ))

(defun hashify-term1 (term alist num)
  (declare (integer num))
  (if (consp term)
      (case (car term)
	((the the-class-of-all)
	 (let* ((var (intern (format nil "__v~d" (incf num))))
		(cadr (list var))
		(new (hashify-form1
		      (caddr term)
		      (setq alist (acons (caadr term) var alist)) num)))
	   (list (cons (car term) (cons cadr (car new)))
		 (cadr new) (caddr new))))
	(t
	 (do* ((f (cdr term) (cdr f))
	       (new (hashify-term1 (car f) alist num)
		    (hashify-term1 (car f) assoc inc))
	       (cdr (list (car new)) (cons (car new) cdr))
	       (inc (caddr new) (caddr new))
	       (assoc (cadr new) (cadr new)))
	     ((null f) (list (cons (car term) (nreverse cdr)) assoc inc)))))
    (let ((new (assoc term alist)))
      (if new (list (cdr new) alist num)
	(let ((v (intern (format nil "__v~d" (incf num)))))
	  (list v (acons term v alist) num))))))

(defun make-skolem-function-symbol (&key ext-name)
  ;; ext-name should begin with a _ .
  (declare (symbol ext-name))
  (push (setq ext-name (make-a-new-name ext-name *skolem-functions*))
	*skolem-functions*)
  ext-name)

(defun new-param-p (param)		; proof
  ;; this expects an EXTERNAL variable name
  (declare (symbol param))
  (dolist (v *parameters* t) (and (eq (ext-name v) param) (return nil))))

(defun get-ext-name (name)
;;; MAKE-PARAMETER 
  (declare (symbol name))
  (if (new-param-p name) name
    ;; wouldn't make-a-new-name with map #'ext-name *parameters* be
    ;; faster?  Maybe not.
    (do* ((num 0 (1+ num))
	  (new-var (intern (format nil (concatenate 'string
					 (string name) "~s") num))
		   (intern (format nil (concatenate 'string
					 (string name) "~s") num))))
	((new-param-p new-var) new-var)
      (declare (type (integer 0 *) num)))))

;;; Formulas

(defvar *new-alpha* nil)
(defvar *view-details-of-formulas* t)

(defstruct (formula
	    (:print-function 
	     (lambda (obj str pl)
	       (declare (ignore pl))
	       (format str "~%[ ~S~:[~;~%  :birth-place ~S~%  :sign ~S
  ~@[copies: ~d~]~] ]"
		       (formula obj) *view-details-of-formulas*
		       (let ((bp (birth-place obj)))
			 (and bp (list (depth bp) (path bp))))
		       (sign obj) (copies obj))))
	    (:constructor make-formula1) (:conc-name nil))
  (formula nil :type list)
  (replacements nil :type list) ; list of formulas which reducer wants to
				; replace this formula with.
  (parents-of nil :type list)	; list of ancestors
  (sign nil :type boolean)	; T if in hypotheses
  (birth-place nil :type (or null proof)) ; sequent where this first appeared
  ;; list of sequents which originated a split on which the formula depends.
  (splits nil :type list)	; This contains a split structure
				; whose node slot points to the node
				; above the splitting which created
				; this formula.
  (kb-searched nil :type boolean) ; has this theorem been through the kb?
  (t-inits nil :type list)	; alist of <theorem . t-junct-alist>
				; where t-junct-alist is of the form
				; <t-junct . t-init-list>.  This keeps
				; track of the theorems in the KB with
				; which this formula can be used.
  (bindings nil :type list)	; bindings on which this formula's
                                ; existence depends
;;;  (inits nil :tyep list)	; used only in kb-formulas.
  ;; The above stores an alist of <thm . alist> pairs where alist is
  ;; an alist of <proof . inits> pairs where inits are t-inits formed
  ;; for this theorem and the PROOF.
  (theorems nil :type list)	; list of theorems which were applied
				; to create this formula.
  (copies 0 :type integer)	; number of copies made
  ;; Instead of the ELIGIBLE stuff, how about setting the copies
  ;; of the new formulas introduced to a number near *copy-max*.
  (rule nil :type symbol)	; rule that will apply to this formula
  (step-args nil :type list)	; arguments to give the step function
  ;; A formula is USED iff it does not occur on any leaf.  If a
  ;; formula is used, then this slot will contain a list of nodes at
  ;; and below which this formula is to be considered used.
  (used nil :type list)
;;; (repeat nil :type symbol) ; to be removed (see subsumtion)
  (free-vars nil :type list)	; list of free variables in this formula
  (universals nil :type list)   ; list of variables universal in this
                                ; formula (used?)
;;;  (applied nil :type list)	; list of things that have been done to this
  )

(defun new-juncts (proof)
  (declare (type proof proof))
  (append (new-hyps proof) (new-goals proof)))

(defun ancestors (juncts proof) ; newbies whence sup
  ;; this assumes that if a junct is not new to a sequent then the exact
  ;; same formula occurred in the superproof.
  ;; Now a junct may have many ancestors if it is new.
  (declare (type proof proof) (list juncts))
  (mapcan #'(lambda (j) (declare (type formula j))
	      (if (if (sign j)
                      (member j (new-hyps proof))
		    (member j (new-goals proof)))
		  ;; You can't use (eq proof (birth-place j)) above because
		  ;; the birth-place of J may have changed.
		  (copy-list (parents-of j))
                (list j)))
          juncts))

;;;(defvar *maths* nil) ; list of math formulas

(defun formulas (list)
  (declare (list list))
  (mapcar #'formula list))

(defun make-formula (formula &key (copies 0) splits bindings sign universals
			     birth-place parents-of theorems)
  (declare (list formula) (ignore universals))
  ;; benji @%#$ find out who calls this and be sure that splits is
  ;; being handled properly.
  (let ((obj (make-formula1
	      :theorems theorems :parents-of parents-of :sign sign
	      :birth-place birth-place :bindings bindings :formula formula
	      :splits splits :copies copies)))
    (other-formula-stuff obj)
;;;    (when (member (car formula) '(forall for-some)))
;;;      (setf (reject-length (caaadr formula)) (or copies 0))
    obj))
  
(defun make-formula-from-old
  (formula obj &key (sign (sign obj)) step-args (copies 0) universals
	   (bindings (bindings obj)) (parents-of (list obj))
	   (theorems (theorems obj)) copy (splits (splits obj)))
  (declare (list formula) (type formula obj) (ignore universals))
  (let ((new-obj (make-formula1
		  :theorems theorems :parents-of parents-of :formula formula
		  :sign sign :bindings bindings :splits splits :copies
		  copies)))
    (declare (type formula new-obj))
    (if copy (setf (rule new-obj) (rule obj)
		   (step-args new-obj) step-args)
      (other-formula-stuff new-obj))
;;;    (when (member (car formula) '(forall for-some))
;;;      (setf (reject-length (caaadr formula)) copies)
;;;      (setf (copies new-obj) copies))
    new-obj))

(defun other-formula-stuff (formula)
  (if (sign formula)
      (case (car (formula formula))
	(iff (setf (rule formula) 'iff)
	     (setf (step-args formula) (list formula))
	     (push formula *betas*))
	(implies (setf (rule formula) 'back-chain)
	  (setf (step-args formula) (list formula))
	  (push formula *betas*))
	(if (setf (rule formula) 'if-split)
	    (setf (step-args formula) (list formula))
	  (push formula *betas*))
	(or (setf (rule formula) 'or-split)
	    (setf (step-args formula) (list formula))
	    (push formula *betas*))
	(not (setf (rule formula) 'flip)
	     (setf (step-args formula) (list formula))
	     (push formula *alphas*))
	(and (setf (rule formula) 'alpha)
	     (setq *new-alpha* t))	; (push formula *alphas*)
	(forall (setf (rule formula) 'show-there-is)
	  (setf (step-args formula) (list formula))
	  (push formula *gammas*))
	(for-some (setf (rule formula) 'consider)
	  (setf (step-args formula) (list formula))
	  (push formula *deltas*))
	(= (push formula *new-equals*)
;;;	 (push formula *=s*)
	   (and *hol* (push formula *pos-extensionality-targets*)))
	(a-member-of
	 (if (eq 'the-class-of-all (caaddr (formula formula)))
	     (progn (setf (step-args formula) (list formula))
		    (setf (rule formula) 'comprehension-schema)
		    (push formula *comprehensions*))
	   (push formula *member-targets*))
	 (push formula *sets*)))
    (case (car (formula formula))
      (implies (setf (rule formula) 'promote) (push formula *alphas*)
	       (setf (step-args formula) (list formula)))
      (not (setf (rule formula) 'flip)
	   (setf (step-args formula) (list formula))
	   (push formula *alphas*))
      (or (setf (rule formula) 'alpha)
	  (setq *new-alpha* t))		; (push formula *alphas*)
      (iff (setf (rule formula) 'iff)
	   (setf (step-args formula) (list formula))
	   (push formula *betas*))
      (if (setf (rule formula) 'if-split)
	  (setf (step-args formula) (list formula))
	(push formula *betas*))
      (and (setf (rule formula) 'and-split)
	   (setf (step-args formula) (list formula))
	   (push formula *betas*))
      (forall (setf (rule formula) 'consider)
	(setf (step-args formula) (list formula))
	(push formula *deltas*))
      (for-some (setf (rule formula) 'show-there-is)
	(setf (step-args formula) (list formula))
	(push formula *gammas*))
      (= (and *hol* (push formula *neg-extensionality-targets*))
      ;; (push formula *=s*)
	 )
      (a-class (setf (step-args formula) (list formula))
	       (setf (rule formula) 'class-schema)
	       (push formula *sets*))
      (a-member-of
       (when (eq 'the-class-of-all (caaddr (formula formula)))
	 (setf (step-args formula) (list formula))
	 (setf (rule formula) 'comprehension-schema)
	 (push formula *comprehensions*))
       (push formula *sets*)))))

(defun hypotheses (proof)
  (do ((pf proof (superproof pf))
       (new-hyps nil (append (remove-if #'used (new-hyps pf)) new-hyps)))
      ((null pf) new-hyps)))

(defun goals (proof)
  (do ((pf proof (superproof pf))
       (new-goals nil (append (remove-if #'used (new-goals pf)) new-goals)))
      ((null pf) new-goals)))

(defun spec-hypotheses (proof)
  (labels ((select
	    (f)	(or (eq (rule f) 'show-there-is)
		    (and (used f) (not (eq (rule f) 'consider))))))
    (do ((pf proof (superproof pf))
	 (new-hyps nil (append (remove-if #'select (new-hyps pf)) new-hyps)))
	((null pf) new-hyps))))

(defun set-hyps (proof elt)
  ;; really set-hyps should return a list of inits because the elt may
  ;; be unifiable with some vars.  Also, this is not complete because
  ;; more formulas may be spliced up above and they will need to be
  ;; checked.  See how Brown's rule solves this similar problem.
  ;; Maybe this is something the unifier should do.  It may be easier
  ;; for it to handle this kind of thing.
  (do ((pf proof (superproof pf))) ((null pf))
    (let ((some (some #'(lambda (h) (and (eq (car (formula h)) 'a-member-of)
					 (equal (cadr (formula h)) elt)))
		      (new-hyps proof))))
      (and some (return some)))))

(defun spec-goals (proof)
  (labels ((select
	    (f)	(or (eq (rule f) 'show-there-is)
		    (and (used f) (not (eq (rule f) 'consider))))))
    (do ((pf proof (superproof pf))
	 (new-goals nil (append (remove-if #'select (new-goals pf))
				new-goals)))
	((null pf) new-goals))))

(defstruct rp-prod
  trigger				; a term
  class-term
  formula				; the formula this came from
  )

;;; This keeps track of splits in the tree.  There is probably a
;;; better way.
(defstruct split
  (vars nil :type list)		; A list containing each variable which occurs
				; in an involved formula on more than
				; one branch.
  node)

(defun set-splits (proof sub &optional split)
  (and (split proof)
       (setf (split sub)                (split proof)
	     (split-node (split proof)) sub
	     (split proof)              split)))

;;; Unification objects:

(defstruct (unification
	    (:print-function
	     (lambda (obj str pl)
	       (declare (ignore pl))
	       (format str "[Unif: ~S]" (full-subst obj))))
	    (:conc-name nil)
	    )
  (init-subs nil :type list)		; list of init-subs which merged here
  (full-subst nil :type list)		; the combination of all of above
  (need-to-close-up nil :type list)	; splits, all of whose subproofs must
					; be closed (not killed).
  (need-to-close-down nil :type list)
					; this is an ordered list of subproofs
					; of elements of need-to-close-up
					; which are not yet closed by
					; unification
  (closed nil :type list))		; where the init-subs are.

(defvar *unification* (make-unification))

(defstruct (init-sub (:conc-name nil)
		     (:print-function
		      (lambda (obj str pl)
			(declare (ignore pl))
			(format str "[Init: ~s]" (f-cache obj)))))
;;;  (option nil)			; the theorem information (if any)
				; associated with this init-sub
  (init-plan nil)		; only if this is a theorem application.
  (f-cache nil :type list)	; mgu for goal and hyps (unless = used)
;;;  (unifications nil :type list)
  ;; list of unifications of which this is known to be a part
  sequent			; sequent where this was formed
  (init-splits nil :type list)	; list of splits on which this depends
  (used-goals nil :type list)	; a list of negative formulas
  (used-hyps nil :type list))	; nil,`(,hyp) or `(,hyp ,@=hyps)

(defun init-splits-fun (init)
  (declare (type init-sub init))
  (remove-duplicates (append (mappend #'splits (used-goals init))
			     (mappend #'splits (used-hyps init)))))

(defun make-init (&key f-cache used-goals used-hyps sequent
		       init-plan &aux (init (make-init-sub
					     :init-plan init-plan :sequent
					     sequent :f-cache f-cache
					     :used-goals used-goals
					     :used-hyps used-hyps)))
  ;; This automatically constructs the splits slot
  (setf (init-splits init) (init-splits-fun init))
;;;  (mapc #'(lambda (p) (incf (ups (split-node p)))) (init-splits init))
  init)

;;; KB

(defstruct kb-term
  (name nil :type symbol)
  (theorems nil :type list)
  (pattern nil :type list)
  (definition nil :type list))

(defstruct kb-predicate
  (name nil :type symbol)
  (theorems nil :type list)
  (pattern nil :type list)
  (definition nil :type list)
  )

(defstruct kb-theorem
  (name nil :type symbol)
  (orig-name nil :type symbol)
  (string "" :type string)
  (format "" :type string)
  (suppositions nil :type list)
  (conclusions nil :type list))

;;;(defstruct kb-formula
;;;  (formula nil :type list)
;;;  (inits nil :type list)
;;;  )

(defstruct (t-init
	    (:print-function
	     (lambda (obj str pl)
	       (declare (ignore pl))
	       (format str "~%[ :theorem ~S~%  :subst ~s
 :formulas ~s
 :thm-junct ~s ]"
		       (kb-theorem-name (t-init-theorem obj))
		       (t-init-subst obj) (t-init-formulas obj)
		       (t-init-thm-junct obj)))))
  ;; The subst of a T-INIT contains both theorem and sequent variables.
  (subst nil :type list)
  (silly nil :type boolean)
  (formulas nil :type list)
  (thm-junct nil :type list)
  (theorem nil :type (or null kb-theorem)))

(defstruct (option			; theorem unification
	    (:print-function
	     (lambda (obj str pl)
	       (declare (ignore pl))
	       (format str
		       "~%[ :theorem ~S~%  :subst ~s :rank ~s
  :bad-hyps ~d  :bad-goals ~d  ]"
		       (kb-theorem-name (option-theorem obj))
		       (option-subst obj) (+ (option-perm-rank obj)
					     (option-temp-rank obj))
		       (length (option-bad-hyps obj))
		       (length (option-bad-goals obj)))))
	    (:copier real-copy-option))
  (applied nil :type boolean)
  (bad-hyps nil :type list)
  (bad-goals nil :type list)
  (good-hyps nil :type list)
  (good-goals nil :type list)
  (proof nil)			; The highest node at which this
				; could be applied.
  (baddies 0 :type (integer 0 *)) ; number of bad juncts
  ;; this can be zero if it is based on an =t-init.
  (goodies 1 :type (integer 0 *)) ; number of good juncts
  (theorem nil :type (or null kb-theorem))
  (bindings nil :type list) ; that part of the substitution which
                                ; binds sequent variables
  (subst nil :type list)	; the entire substitution
;;;  (t-bindings nil :type list)	; that part of the substitution which
				; binds theorem variables.
;;;  (match-p nil :type boolean)
  (redundant nil :type boolean)
  (parent nil)			; the option of which this is a copy
  (temp-rank 0 :type number)
  (perm-rank 0 :type number)
  (t-inits nil :type list))

(defun copy-option (option)
  (let ((obj (real-copy-option option)))
    (setf (option-parent obj) option)
    obj))

;;; Making copies of subtrees:

(defvar *replacements* (make-hash-table))

(defmacro top-of-replacements ()
  '(gethash :top *replacements*))

(defmacro replacement (old)
  `(gethash ,old *replacements*))

(defmacro set-replacement (old new)
  `(setf (replacement ,old) ,new))

;;;(defmacro copy-thing-if-local (thing proof copy-function)
;;;  `(or (gethash ,thing *replacements*)
;;;       (if (properly-in-tree ,proof (top-of-replacements))
;;;	   (setf ,thing (funcall #',copy-function ,thing)))))

(defun copy-subproofs (proof)
  ;; This returns a list containing one copy of each of the subproofs
  ;; of proof.
  (clrhash *replacements*)
  (set-replacement :top proof)
  (cons (and (split proof)
	     (set-replacement (split proof) (copy-split (split proof))))
	(mapcar #'copy-proof-local (subproofs proof))))

(defun copy-proof-local (prf &aux (done (gethash prf *replacements*))
			     (cop (or done (set-replacement
					    prf (copy-proof prf)))))
  ;; This recursively copies PRF and its subproofs.  This must make
  ;; copies of the new formulas and successes as well.  Maybe I should
  ;; have a hash table which associates originals with new copies so I
  ;; can make that replacement quickly.  The following slots may need
  ;; changing.
  (or done
      (prog1 cop
	(and (split cop)
	     (setf (split cop) (copy-split-local (split cop))))
	(and (superproof cop)
	     (setf (superproof cop)
	       (if (properly-in-tree (superproof cop) (top-of-replacements))
		   (copy-proof-local (superproof cop)) (superproof cop))))
	(setf (new-hyps cop)
	  (mapcar #'(lambda (f) (copy-formula-local f)) (new-hyps cop))
	  (new-goals cop)
	  (mapcar #'(lambda (f) (copy-formula-local f)) (new-goals cop))
	  (successes cop) (mapcar #'copy-init-local (successes cop))
	  (theorem-successes cop)
	  (mapcar #'copy-init-local (theorem-successes cop))
	  (trivial-closures cop) 
	  (mapcar #'copy-init-local (trivial-closures cop))
	  (involved cop) (mapcar #'copy-formula-local (involved cop))
	  (new-hyps-above cop)
	  (mapcar
	   #'(lambda (f)
	       (or (gethash f *replacements*)
		   (if (properly-in-tree (birth-place f) (top-of-replacements))
		       (copy-formula-local f)
		     (set-replacement f f))))
	   (new-hyps-above cop))
	  (new-goals-above cop)
	  (mapcar
	   #'(lambda (f)
	       (or (gethash f *replacements*)
		   (if (properly-in-tree (birth-place f) (top-of-replacements))
		       (copy-formula-local f)
		     (set-replacement f f))))
	   (new-goals-above cop))
	  (options cop)
	  (mapcar
	   #'(lambda (opt)
	       (or (gethash opt *replacements*)
		   (if (properly-in-tree (option-proof opt)
					 (top-of-replacements))
		       (copy-option-local opt)
		     (set-replacement opt opt)))) (options cop))
	  (t-init-alist cop)
	  (mapcar #'copy-theorem-local (t-init-alist cop))
	  (applications-to-branch cop)
	  (mapcar
	   #'(lambda (app)
	       (cons (car app)
		     (mapcar
		      #'(lambda (opt)
			  (or (gethash opt *replacements*)
			      (if (properly-in-tree (option-proof opt)
						    (top-of-replacements))
				  (copy-option-local opt)
				(set-replacement opt opt))))
		      (cdr app))))
	   (applications-to-branch cop))
	  (subproofs cop) (mapcar #'copy-proof-local (subproofs cop))))))


(defun copy-formula-local (formula)
  ;; This copies the formula and may reset the following slots:
  ;; birth-place, splits.
  (or (gethash formula *replacements*)
      (let ((copy (set-replacement formula (copy-formula formula))))
	(setf (birth-place copy)
	  (copy-proof-local (birth-place copy)) (splits copy)
	  (mapcar #'copy-split-local (splits copy))
	  (t-inits copy) (mapcar #'copy-theorem-local (t-inits copy))
	  (step-args copy) (list copy))
	copy)))

(defun copy-split-local (split)
  (or (replacement split)
      (let ((cs (set-replacement split (copy-split split))))
	(setf (split-node cs) (copy-proof-local (split-node cs)))
	cs)))

(defun copy-theorem-local (theorem)
  (cons (car theorem)
	(mapcar
	 #'(lambda (t-junct)
	     (cons (car t-junct)
		   (mapcar
		    #'(lambda (t-init)
			(or (gethash t-init *replacements*)
			    (if (t-init-formulas t-init)
				(if (some #'(lambda (f)
					      (properly-in-tree
					       (birth-place f)
					       (top-of-replacements)))
					  (t-init-formulas t-init))
				    (let ((ct (set-replacement
					       t-init (copy-t-init t-init))))
				      (setf (t-init-formulas ct)
					(mapcar
					 #'(lambda (f)
					     (or (replacement f)
						 (if (properly-in-tree
						      (birth-place f)
						      (top-of-replacements))
						     (copy-formula-local f)
						   (set-replacement f f))))
					 (t-init-formulas ct)))
				      ct)
				  (set-replacement t-init t-init))
			      (set-replacement t-init t-init))))
		    (cdr t-junct)))) (cdr theorem))))

(defun copy-t-init-local (t-init)
  (let ((ct (set-replacement t-init (copy-t-init t-init))))
    (setf (t-init-formulas ct)
      (mapcar #'(lambda (f)
		  (or (replacement f)
		      (if (properly-in-tree (birth-place f)
					    (top-of-replacements))
			  (copy-formula-local f)
			(set-replacement f f))))
	      (t-init-formulas ct)))
    ct))

(defconstant *error-checking* T)

(defun copy-option-local
  (option &aux (done (gethash option *replacements*))
	  (copt (or done (set-replacement option (real-copy-option option)))))
  (or done
      (prog1 copt
	(and (option-parent copt)
	     (setf (option-parent copt)
	       (or (gethash (option-parent copt) *replacements*)
		   (if (option-proof (option-parent copt))
		       (if (properly-in-tree
			    (option-proof (option-parent copt))
			    (top-of-replacements))
			   (copy-option-local (option-parent copt))
			 (set-replacement (option-parent copt)
					  (option-parent copt)))
		     (set-replacement (option-parent copt)
				      (option-parent copt))))))
	(setf (option-good-hyps copt)
	  (mapcar #'(lambda (f)
		      (or (gethash f *replacements*)
			  (prog1 nil
			    (and *error-checking*
				 (not (birth-place f))
				 (error "~s has no birth-place" f)))
			  (if (properly-in-tree (birth-place f)
						(top-of-replacements))
			      (copy-formula-local f)
			    (set-replacement f f))))
		  (option-good-hyps copt))
	  (option-good-goals copt)
	  (mapcar #'(lambda (f)
		      (or (gethash f *replacements*)
			  (prog1 nil
			    (and *error-checking*
				 (not (birth-place f))
				 (error "~s has no birth-place" f)))
			  (if (properly-in-tree (birth-place f)
						(top-of-replacements))
			      (copy-formula-local f)
			    (set-replacement f f))))
		  (option-good-goals copt))
	  (option-proof copt)
	  (or (gethash (option-proof copt) *replacements*)
	      (prog1 nil
		(and *error-checking*
		     (not (option-proof copt))
		     (error "~s has no proof" copt)))
	      (if (properly-in-tree (option-proof copt) (top-of-replacements))
		  (copy-proof-local (option-proof copt))
		(set-replacement (option-proof copt) (option-proof copt))))
	  (option-t-inits copt)
	  (mapcar
	   #'(lambda (t-init)
	       (or (gethash t-init *replacements*)
		   (if (t-init-formulas t-init)
		       (if (some #'(lambda (f) (properly-in-tree
						(birth-place f)
						(top-of-replacements)))
				 (t-init-formulas t-init))
			   (copy-t-init-local t-init)
			 (set-replacement t-init t-init))
		     (set-replacement t-init t-init))))
	   (option-t-inits copt))))))

(defun copy-init-local (init &aux (done (gethash init *replacements*))
			     (cin (or done (set-replacement
					    init (copy-init-sub init)))))
  (or done
      (prog1 cin
	(setf (sequent cin)
	  (copy-proof-local (sequent cin)) (init-splits cin)
	  (mapcar
	   #'(lambda (split)
	       (or (gethash split *replacements*)
		   (if (properly-in-tree (split-node split)
					 (top-of-replacements))
		       (copy-split-local split)
		     (set-replacement split split))))
	   (init-splits cin))
	  (used-goals cin)
	  (mapcar #'(lambda (f)
		      (or (gethash f *replacements*)
			  (if (properly-in-tree (birth-place f)
						(top-of-replacements))
			      (copy-formula-local f)
			    (set-replacement f f))))
		  (used-goals cin))
	  (used-hyps cin)
	  (mapcar #'(lambda (f)
		      (or (gethash f *replacements*)
			  (if (properly-in-tree (birth-place f)
						(top-of-replacements))
			      (copy-formula-local f)
			    (set-replacement f f))))
		  (used-hyps cin))))))

