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

;; make some user level commands for noticing repeated proofs
;; and needed simplifications

(in-package :ipr)

;;; This file implements the lisp side of most of the user interface.
;;; Also, the next level down after that.  I.e. the high-level
;;; flow-control of the proof procedure.

(defmacro prove (formula &optional name (string ""))
  "This is called from ipr-mode in emacs.  It initializes everything.
Do not call this unless you are starting all over with a new theorem.
Then it enters the interaction loop.  The interaction loop continues
until the proof is done or the user quits."
  `(prove-fun ',formula ',name ,string))

(defun prove-fun (formula &optional name (string ""))
  (declare (list formula))
  (initialize-data name string)
  (catch 'error
    (setf (new-goals *proof*)
      (mapcar #'(lambda (f &aux (form (make-formula f :birth-place *proof*)))
		  (declare (type formula form) (list f))
                  ;;		  (push form (new-goals *proof*))
                  form)
	      (reduce-d (check-formula formula))))
    ;; This just tests to see if the tree is immediately closed.
    (bt-unify-tree)
    ;; This puts the theorems with equalities in the hypotheses into
    ;; place.
    (when (and *kb-theorems* *theory* (fix-t-init-alist=))
      ;; The above puts =options into *new-t-init-alist*.
      (make-options *proof*)
      ;; this is an unusual situation.  Normally, the current
      ;; juncts will not be included here.
      (and (options *proof*)
	   (setf (new-goals-above *proof*) (new-goals *proof*))))
    (interaction-loop *proof*)))

(defun detect-interruption ()
  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
			(format t "~%?")
		      (format t "?")))
  (equal (read-char-no-hang) #\0))

(defun initialize-data (name string)
  ;; clear information attatched to variables
  (clrhash *order*)
  (clrhash *ext-names*)
  (setq *unbound-vars* nil)	; unbound variables in the formula to be proved
  (setq *bound-variable-number* 0)
  (setq *bound-variables* nil)
  (setq *parameters* nil)	; unification variables
;;;  (setq *n-order* 0)		; ?
  (setq *parameter-number* 0)	; ?

  ;; clear information involving formulas
  (setq *thes* nil)		; T if there is a iota-term in the formula
  (setq *alphas* nil)
  (setq *betas* nil)
;;;  (setq *=s* nil)
  (setq *deltas* nil)
  (setq *gammas* nil)
;;;  (setq *sets* nil)
  (setq *comprehensions* nil)
  (setq *member-targets* nil)
  (setq *neg-extensionality-targets* nil)
  (setq *pos-extensionality-targets* nil)

  ;; clear information involving term and predicate symbols
  (setq *involved-terms* nil)
  (setq *involved-predicates* nil)
  (setq *unknown-terms* nil)	; terms in the formula not defined in the kb
  (setq *unknown-predicates* nil) ; predicates in formula not defined in the kb
  (setq *skolem-functions* nil)

  ;; clear information involving the tableau
  (setq *dead-proofs* nil)	; sequents that are killed by condenser
  (setq *proof* (make-sequent :theorem-name name :theorem-string string))
  (setq *high-proof* *proof*)
  (setq *unifying-proof* (make-sequent))

  ;; clear information involving the unifications
  ;; I could make these local to the functions which use them but then
  ;; I could not ask about them during debugging as easily.
  (setq *t-init-alist* nil)
  (setq *new-t-init-alist* nil)
  (setq *merge-t-init-alist* nil)
  (setq *rel-t-init-alist* nil)
  (setq *unification* (make-unification))

  ;; clear information involving the knowledge base.
  (setq *options* nil)

  ;; clear information involving flow of the program
  (and *show-calls* (setq *show-calls* 10))
  (setq *nothing-to-do* nil)
  (setq *time* 0)
  (setq *interactions* 0)
  (setq *done* nil)
  (setq *open* nil)		; view open proofs only?
  (setq *copy-max* 1)		; q-limit (start at 0?)
  (setq *view* 7)		; output
  (when *use-math*
    (setq *theorem-max* 2) ; ?
    (setq *undefined-terms* nil) ; ?
    (setq *undefined-predicates* nil)) ; ?
  )

(defun goer (&optional (proof *proof*)) ; SPACE
  ;; Default: should include the widest range of options possible.
  (declare (type proof proof))
  (let ((*cat-list* *default-cat-list*))
    (setq *nothing-to-do* nil)
    (prove-apropos proof)))

(defun prove-apropos (proof &aux once)
  (declare (type proof proof))
  (catch 'error
    ;; This could be more efficient if I had a different function
    ;; written for each of the toggle variables.
    (while (nor (and *interact*
		     (or once (and (setq once t) nil)))
		(and *watch*
		     (or once (and (setq once t) nil))
		     (or (send-proof) t))
		*nothing-to-do* *done*
		(and *allow-interruptions*
		     (detect-interruption)))
      ;; Take care of all alphas and then continue.
      ;; alpha rules are like nothing.
      (and *alphas* (pop-all-alphas))
      (and (or *fol* *hol*) *deltas* (pop-all-deltas))
      (when (nor
	     (cond
	      ((and *hol* (or *comprehensions*
			      (make-new-comprehensions)
			      )
		    (prove-set-theory)))
	      (*betas* (prove-beta))
	      ((and (or *fol* *hol*)
                    (or (and *browns-rule*
                             (not *keep-origs-after-brown*)
;;;		     (setq *now-gammas*
;;;		       (mapcan #'(lambda (g) (and (eligible? g) (list g)))
                             (setf *gammas* (delete-if #'used *gammas*)))
;;;		    (setq *now-gammas*
;;;		      (mapcan #'(lambda (g) (and (eligible? g) (list g)))
;;;			      *gammas*)))
                        )
                    (prove-gamma))))
	     (and *theory* *kb-theorems* (prove-math)))
;;; when < prove-math *rate-min*
;;;  try-equality-and-hol on *leaf-queue*.
	(bt-unify-tree)
	(setq *nothing-to-do* t))))
  (interaction-loop proof))

(defun prove-set-theory ()
  ;; Returns non-nil on success.  This is only called when
  ;; *comprehensions* is non-nil.  When comprehensions is nil,
  ;; make-new-comprehensions should be called.
  (dolist (form *comprehensions*)
    (funcall (rule form) form)
    (when *new-alpha*
      (reducer (birth-place form))
      (setq *new-alpha* nil))
    (when (and *equality* *new-equals*)
      (new-equality-formula)
      (setq *new-equals* nil)))
  (setq *comprehensions* nil))

(defun prove-beta (&aux (form (pop *betas*)))
  ;; The callees of this must set the USED slot of FORM.
  ;; This assumes that beta rules are applied at leaves only.
  (declare (type formula form))
  (and *show-calls*
       (if (zerop (mod (incf *show-calls*) 60))
           (format t "~%B")
         (format t "B")))
  (funcall (rule form) form)
;;;  (push t (used form))
  (when *new-alpha*
    (reducer (birth-place form))
    (setq *new-alpha* nil))
  (when (and *equality* *new-equals*)
    (new-equality-formula)
    (setq *new-equals* nil))
  t)

;;; Prove-gamma:

;;; There are two choices to make.
;;;   A branch.
;;;   A gamma-formula.
;;; Idea:  Let's say: choose the worst branch until some gamma-formula
;;; has n copies made where n> the square of the 'copies of the formula
;;; which has the least number of copies made.
;;; 
;;; Now, I may still want to find all init-subs for whatever reason?
;;; 
;;; For now let's simplify.  If I want to see ideas about how to work
;;; only on the branch that needs it most, then see the second half of
;;; this function as it is stored in "ideas-about-bt-unification".
(defun prove-gamma ()
  ;; I only want to call bt-unify when something other than a gamma
  ;; rule has been called since the last time I called bt-unify.
  (unify-and-sort-branches)
  (and *done* (return-from prove-gamma *done*))
  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
			(format t "~%G") (format t "G")))
  ;; The following search is taking too long.
  ;; If (and *theory* *kb-theorems*) then only try the first leaf in
  ;; the queue or maybe only the leaves with the lowest rank.  Then
  ;; let math try.  Benji !@#$%^&*
  (some #'(lambda (prf)	; set this to be *viewing-proof*?
	    ;; This ensures that only the worst are tried.  But
	    ;; there is still a problem: What if the worst
	    ;; branches are not where the work needs to be done?
	    ;; I think that after some time, this requirement
	    ;; will have to be laxed.  It would be nice to have
	    ;; a supervisor which watched the plans for patterns
	    ;; and then required something else to be tried.
	    (setq *low-proof* prf) ; needed?
	    ;; At some point make the tableaux gamma rule make
	    ;; lots of variables.
	    (tableaux-gamma-rule prf)) *leaf-queue*))

(defun prove-math ()
  ;; This should return non-nil if something is done.
;;;  (rate-leaves)
  (unify-and-sort-branches)
  (and *done* (return-from prove-math *done*))
  ;; Go through the formulas in (car *leaf-queue*) and find all
  ;; options for theorem application.  In the process, theorems will
  ;; be put into the theorems slot of formulas and options will be
  ;; put into the theorems-to-apply slot.
  (some #'(lambda (p) (find-and-apply-best-theorem p)) *leaf-queue*))

(defun pop-all-alphas ()
  ;; The callees of this must set the USED slot of FORM.
  ;; The callees of this must call reducer.
  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
			(format t "~%A") (format t "A")))
  (while *alphas*
    (dolist (form (prog1 (copy-list *alphas*)
		    (setq *alphas* nil)))
      (declare (type formula form))
      (funcall (rule form) form)
      (when (and *equality* *new-equals*)
	(new-equality-formula)
	(setq *new-equals* nil))
      ))
  t)

(defun pop-all-deltas ()
  ;; The callees must set the USED slot of FORM.
  ;; The callees must call reducer.
  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
			(format t "~%D") (format t "D")))
  (while *deltas*
    (dolist (form (prog1 (copy-list *deltas*)
		    (setq *deltas* nil)))
      (declare (type formula form))
      (funcall (rule form) form)
      ;; Why not make the rule include "side-step" or call the
      ;; function by the rule?
      (when (and *equality* *new-equals*)
	(new-equality-formula)
	(setq *new-equals* nil))))
  t)

(defun interaction-loop (&optional (proof *proof*))
  (declare (ignore proof))
  ;; should be called by anybody who wants to redisplay the proof when done.
  (send-proof)
  ;; the below is fine because if there is a proof of *proof* then take it.
  (interact))

(defun interact ()
  "This checks to see what the user wants to do."
  (incf *interactions*)
;;;  (if (and *unification-takes-too-long*
;;;	   (ask-user-about-choosing-substs))
;;;      (walk-through-subs))
  (emacs-eval
   `(message
     ,(quote-string (format nil "Enter a command (#~s)." *interactions*)))))

;;; (defvar *choose-subst-mode* nil)
;;; 
;;; Change send-proof to report on branch closures.  Make a command
;;; where the user says "choose one of the successes here" then enters
;;; a number for "which one".  Apply that substitution across the
;;; entire tree and kill skipped branches.  No, that's not allowed.
;;; But you do have to undo all completely-unified slots because old
;;; inits will become invalid.

(defun send-proof (&key (command *view*))
  (declare (type (integer 0 9) command))
  ;; This might have an option like "threshold" so it shows the first
  ;; proof whose rank is below a certain threshold, etc.
  "Send current emacs-proof to the check-on buffer."
  (setq *default-view* *view*) (setq *view* command)
  (case command
    ;; entire proof
    (0 (setq *viewing-proof* *proof*)
       (emacs-eval
	`(ipr-put-string-into-proof-buffer ; (get-proof)
	  ,(quote-string (format nil "~a" (beautify-entire-proof *proof*))))))
    ;; entire proof under cursor
    (1 (emacs-eval
	`(ipr-put-string-into-proof-buffer ; (get-proof)
	  ,(let ((proof (car (leaves *high-proof*))))
	     (setq *viewing-proof* proof)
	     (quote-string
	      (format nil "~a" (beautify-proof *viewing-proof*)))))))
    ;; all leaves
    (2 (emacs-eval
	`(ipr-put-string-into-proof-buffer ; (get-proof)
	  ,(let ((proof (car (leaves *high-proof*))))
	     (when proof (setq *viewing-proof* proof))
	     (quote-string
	      (format nil "~a" (beautify-proof *viewing-proof*)))))))
    ;; all leaves under cursor
    (3 (emacs-eval
	`(ipr-put-string-into-proof-buffer ; (get-proof)
	  ,(let ((proof (car (leaves *high-proof*))))
	     (when proof (setq *viewing-proof* proof))
	     (quote-string
	      (format nil "~a" (beautify-proof *viewing-proof*)))))))
    ;; first entire branch going through cursor
    (4 (emacs-eval
	`(ipr-put-string-into-proof-buffer ; (get-proof)
	  ,(let ((proof (car (leaves *high-proof*))))
	     (when proof (setq *viewing-proof* proof))
	     (quote-string
	      (format nil "~a" (beautify-proof *viewing-proof*)))))))
    ;; first entire branch going through cursor
    (5 (emacs-eval
	`(ipr-put-string-into-proof-buffer ; (get-proof)
	  ,(let ((proof (car (leaves *high-proof*))))
	     (when proof (setq *viewing-proof* proof))
	     (quote-string
	      (format nil "~a" (beautify-proof *viewing-proof*)))))))
    ;; first leaf
    (6 (emacs-eval
	`(ipr-put-string-into-proof-buffer ; (get-proof)
	  ,(let ((proof (car (leaves *high-proof*))))
	     (when proof (setq *viewing-proof* proof))
	     (quote-string
	      (format nil "~a" (beautify-proof *viewing-proof*)))))))
    ;; first leaf under cursor
    (7 (emacs-eval
	;; make sure this does not check lisp ready.
	`(ipr-put-string-into-proof-buffer ; (get-proof)
	  ,(let ((proof (car (leaves *high-proof*))))
	     (when proof (setq *viewing-proof* proof))
	     (quote-string
	      (format nil "~a" (beautify-proof *viewing-proof*)))))))
    ;; entire first branch
    (8 (emacs-eval
	`(ipr-put-string-into-proof-buffer ; (get-proof)
	  ,(let ((proof (car (leaves *high-proof*))))
	     (when proof (setq *viewing-proof* proof))
	     (quote-string
	      (format nil "~a" (beautify-proof *viewing-proof*)))))))))
;; we have single sequent-mode.  add single branch mode.
;; so the user can look at branches in sequence.

(defun send-proof-single-sequent (proof) ;location
  (when proof
    (setq *viewing-proof* proof)
    (emacs-eval `(ipr-put-string-into-proof-buffer
		  ,(quote-string (beautify-proof *viewing-proof*))))))

(defun send-proof-leaf (num)	; called from emacs
  (declare (integer num))
  (let ((leaves (if (< num 0) (reverse (leaves *proof*)) (leaves *proof*)))
	past
	(n-leaf (if *open* (first-open-leaf *viewing-proof*)
		  (first-leaf *viewing-proof*)))
	(inc 0))
    (declare (list leaves) (symbol past) (type proof n-leaf)
	     (type (integer 0) inc))
    (setq num (abs num))
    (when (not (eq *viewing-proof* n-leaf))
      (if (< num 0) (incf num) (decf num)))
    (do ((leaf leaves (cdr leaf)))
	((null leaf)
	 (if (< num 0) 
	     (progn
	       (emacs-eval
		`(progn (beep)
			(message
			 ,(quote-string "No such leaf.  Sending last."))
			(sleep-for 3)))
	       (send-proof-single-sequent (car leaves)))
	   (progn (emacs-eval
		   `(progn (beep)
			   (message
			    ,(quote-string "No such leaf. Sending first."))
			   (sleep-for 3)))
		  (send-proof-single-sequent (car leaves)))))
      (declare (list leaf))
      (cond (past (if (< num 0) (decf inc) (incf inc)))
	    ((eq n-leaf (car leaf))
	     (if (zerop num) (return (send-proof-single-sequent (car leaf)))
	       (setq past t))))
      (when (= inc num)
	(return (send-proof-single-sequent (car leaf)))))))

(defun user-promote-step (pos)
  ;; So the implies has been found therefore we don't need to check it.
  (setq pos (1- pos))
  (let ((formula (nth pos (unused-goals-here *viewing-proof* *viewing-proof*))))
    (declare (type formula formula))
    (setq *alphas* (delete formula *alphas*))
    (if *tableaux* (promote5 formula) (promote2 *viewing-proof* formula pos))
;;;    (setf (used formula) t)
    (push t (used formula))
    (when *new-alpha* (reducer (birth-place formula)) (setq *new-alpha* nil))
    (when (and *equality* *new-equals*)
      (new-equality-formula) (setq *new-equals* nil)))
  (subproofs *viewing-proof*))

(defun user-flip-step (pos sign)
  (declare (type (integer 0 *) pos))
  (setq pos (1- pos))
  (if sign
      (let ((formula (nth pos (unused-hypotheses-here *viewing-proof* *viewing-proof*))))
	(declare (type formula formula))
	(setq *alphas* (delete formula *alphas*))
	(if *tableaux* (flip5 formula) (flip2 *viewing-proof* formula pos))
;;;	(setf (used formula) t)
	(push t (used formula))
	(when *new-alpha*
	  (reducer (birth-place formula)) (setq *new-alpha* nil))
	(when (and *equality* *new-equals*)
	  (new-equality-formula) (setq *new-equals* nil)))
    (let ((formula (nth pos (unused-goals-here *viewing-proof* *viewing-proof*))))
      (declare (type formula formula))
      (setq *alphas* (delete formula *alphas*))
      (if *tableaux* (flip5 formula) (flip2 *viewing-proof* formula pos))
;;;      (setf (used formula) t)
      (push t (used formula))
      (when *new-alpha*
	(reducer (birth-place formula)) (setq *new-alpha* nil))
      (when (and *equality* *new-equals*)
	(new-equality-formula) (setq *new-equals* nil))))
  (subproofs *viewing-proof*))

(defun user-and-split-step (pos)
  ;; The callees of this must set the USED slot of FORMULA.
  (declare (type (integer 0 *) pos))
  (setq pos (1- pos))
  (let ((formula (nth pos (unused-goals-here *viewing-proof* *viewing-proof*))))
    (declare (type formula formula))
    (setq *betas* (delete formula *betas*))
    (if *tableaux* (and-split5 formula)
      (and-split2 *viewing-proof* formula pos))
;;;    (setf (used formula) t)
    (push t (used formula))
    (when *new-alpha* (reducer (birth-place formula)) (setq *new-alpha* nil))
    (when (and *equality* *new-equals*)
      (new-equality-formula) (setq *new-equals* nil)))
  (subproofs *viewing-proof*))

(defun user-or-split-step (pos)
  ;; The callees of this must set the USED slot of FORMULA.
  (declare (type (integer 0 *) pos))
  (setq pos (1- pos))
  (let ((formula (nth pos (unused-hypotheses-here *viewing-proof* *viewing-proof*))))
    (declare (type formula formula))
    (setq *betas* (delete formula *betas*))
    (if *tableaux* (or-split5 formula) (or-split2 *viewing-proof* formula pos))
;;;    (setf (used formula) t)
    (push t (used formula))
    (when *new-alpha* (reducer (birth-place formula)) (setq *new-alpha* nil))
    (when (and *equality* *new-equals*)
      (new-equality-formula) (setq *new-equals* nil)))
  (subproofs *viewing-proof*))

(defun user-if-split-step (pos sign)
  ;; The callees of this must set the USED slot of FORMULA.
  ;; so the implies has been found therefore we don't need to check it
  (declare (type (integer 0 *) pos))
  (setq pos (1- pos))
  (if sign
      (let ((formula (nth pos (unused-hypotheses-here *viewing-proof* *viewing-proof*))))
	(declare (type formula formula))
	(setq *betas* (delete formula *betas*))
	(if *tableaux* (if-split5 formula)
	  (if-split2 *viewing-proof* formula pos))
;;;	(setf (used formula) t)
	(push t (used formula))
	(when *new-alpha*
	  (reducer (birth-place formula)) (setq *new-alpha* nil))
	(when (and *equality* *new-equals*)
	  (new-equality-formula) (setq *new-equals* nil)))
    (let ((formula (nth pos (unused-goals-here *viewing-proof* *viewing-proof*))))
      (declare (type formula formula))
      (setq *betas* (delete formula *betas*))
      (if *tableaux* (if-split5 formula)
	(if-split2 *viewing-proof* formula pos))
;;;      (setf (used formula) t)
      (push t (used formula))
      (when *new-alpha*
	(reducer (birth-place formula)) (setq *new-alpha* nil))
      (when (and *equality* *new-equals*)
	(new-equality-formula) (setq *new-equals* nil))))
  (subproofs *viewing-proof*))

(defun user-iff-step (pos sign)
  ;; The callees of this must set the USED slot of FORMULA.
  ;; so the implies has been found therefore we don't need to check it
  (declare (type (integer 0 *) pos))
  (setq pos (1- pos))
  (if sign
      (let ((formula (nth pos (unused-hypotheses-here *viewing-proof* *viewing-proof*))))
	(declare (type formula formula))
	(setq *betas* (delete formula *betas*))
	(if *tableaux* (iff5 formula)
	  (iff2 *viewing-proof* formula pos))
;;;	(setf (used formula) t)
	(push t (used formula))
	(when *new-alpha*
	  (reducer (birth-place formula)) (setq *new-alpha* nil))
	(when (and *equality* *new-equals*)
	  (new-equality-formula) (setq *new-equals* nil)))
    (let ((formula (nth pos (unused-goals-here *viewing-proof* *viewing-proof*))))
      (declare (type formula formula))
      (setq *betas* (delete formula *betas*))
      (if *tableaux* (iff5 formula)
	(iff2 *viewing-proof* formula pos))
;;;      (setf (used formula) t)
      (push t (used formula))
      (when *new-alpha*
	(reducer (birth-place formula)) (setq *new-alpha* nil))
      (when (and *equality* *new-equals*)
	(new-equality-formula) (setq *new-equals* nil))))
  (subproofs *viewing-proof*))

(defun user-back-chain-step (pos)
  ;; The callees of this must set the USED slot of FORMULA.
  ;; so the implies has been found therefore we don't need to check it
  (declare (type (integer 0 *) pos))
  (setq pos (1- pos))
  (let ((formula (nth pos (unused-hypotheses-here *viewing-proof* *viewing-proof*))))
    (declare (type formula formula))
    (setq *betas* (delete formula *betas*))
    (if *tableaux* (back-chain5 formula)
      (back-chain2 *viewing-proof* formula pos))
;;;    (setf (used formula) t)
    (push t (used formula))
    (when *new-alpha* (reducer (birth-place formula)) (setq *new-alpha* nil))
    (when (and *equality* *new-equals*)
      (new-equality-formula) (setq *new-equals* nil)))
  (subproofs *viewing-proof*))

(defun user-unquantify (pos sign)
  ;; The callees of this must set the USED slot of FORMULA.
  ;; The callees of this must call reducer.
  ;; so the implies has been found therefore we don't need to check it
  (declare (type (integer 0 *) pos))
  (setq pos (1- pos))
  (if sign (let ((formula (nth pos (unused-hypotheses-here *viewing-proof* *viewing-proof*))))
	     (declare (type formula formula))
	     (if (eq 'forall (car (formula formula)))
		 (if *tableaux* (show-there-is5 formula)
		   (show-there-is2 *viewing-proof* formula pos))
	       (progn (setq *deltas* (delete formula *deltas*))
		      (if *tableaux* (consider5 formula)
			(consider2 *viewing-proof* formula pos))
;;;		      (setf (used formula) t)
		      (push t (used formula))
		      )
	       )
	     (when *new-alpha*
	       (reducer (birth-place formula)) (setq *new-alpha* nil))
	     (when (and *equality* *new-equals*)
	       (new-equality-formula)
	       (setq *new-equals* nil)))
    (let ((formula (nth pos (unused-goals-here *viewing-proof* *viewing-proof*))))
      (declare (type formula formula))
      (if (eq 'forall (car (formula formula)))
	  (progn (setq *deltas* (delete formula *deltas*))
		 (if *tableaux* (consider5 formula)
		   (consider2 *viewing-proof* formula pos))
;;;		 (setf (used formula) t)
		 (push t (used formula))
		 )
	(if *tableaux* (show-there-is5 formula)
	  (show-there-is2 *viewing-proof* formula pos)))
      (when *new-alpha*
	(reducer (birth-place formula)) (setq *new-alpha* nil))
      (when (and *equality* *new-equals*)
	(new-equality-formula) (setq *new-equals* nil))))
  (subproofs *viewing-proof*))

(defun user-consider-step (pos sign)
  ;; The callees of this must set the USED slot of FORMULA.
  ;; The callees must call reducer.
  ;; so the implies has been found therefore we don't need to check it
  (declare (type (integer 0 *) pos))
  (setq pos (1- pos))
  (if sign (let ((formula (nth pos (unused-hypotheses-here *viewing-proof* *viewing-proof*))))
	     (declare (type formula formula))
	     (setq *deltas* (delete formula *deltas*))
	     (if *tableaux* (consider5 formula)
	       (consider2 *viewing-proof* formula pos))
;;;	     (setf (used formula) t)
	     (push t (used formula))
	     (when *new-alpha*
	       (reducer (birth-place formula)) (setq *new-alpha* nil))
	     (when (and *equality* *new-equals*)
	       (new-equality-formula)
	       (setq *new-equals* nil)))
    (let ((formula (nth pos (unused-goals-here *viewing-proof* *viewing-proof*))))
      (declare (type formula formula))
      (setq *deltas* (delete formula *deltas*))
      (if *tableaux* (consider5 formula)
	(consider2 *viewing-proof* formula pos))
;;;      (setf (used formula) t)
      (push t (used formula))
      (when *new-alpha*
	(reducer (birth-place formula)) (setq *new-alpha* nil))
      (when (and *equality* *new-equals*)
	(new-equality-formula) (setq *new-equals* nil))))
  (subproofs *viewing-proof*))

;;;(defun user-show-there-is-step (pos sign)
;;;  ;; The callees must call reducer.
;;;  ;; so the implies has been found therefore we don't need to check it
;;;  (declare (type (integer 0 *) pos))
;;;  (setq pos (1- pos))
;;;  (if sign
;;;      (let ((formula (nth pos (unused-goals-here *viewing-proof* *viewing-proof*))))
;;;	(declare (type formula formula))
;;;	(if *tableaux* (show-there-is5 formula)
;;;	  (show-there-is2 *viewing-proof* formula pos))
;;;	(when *new-alpha*
;;;	  (reducer (birth-place formula)) (setq *new-alpha* nil))
;;;	(when (and *equality* *new-equals*)
;;;	  (new-equality-formula)
;;;	  (setq *new-equals* nil)))
;;;    (let ((formula (nth pos (unused-goals-here *viewing-proof* *viewing-proof*))))
;;;      (declare (type formula formula))
;;;      (if *tableaux* (show-there-is5 formula)
;;;	(show-there-is2 *viewing-proof* formula pos))
;;;      (when *new-alpha*
;;;	(reducer (birth-place formula)) (setq *new-alpha* nil))
;;;      (when (and *equality* *new-equals*)
;;;	(new-equality-formula) (setq *new-equals* nil))))
;;;  (subproofs *viewing-proof*))

(defun user-apply-theorem (theorem-name &optional searched
					&aux (proof *viewing-proof*))
  (if (options proof)		; Benji !@#$%^&*
      (let ((option (get-matching-option theorem-name proof searched)))
	(and option (setf (subproofs proof) (apply-option option))))
    (if searched (warn-user "That theorem does not apply here.")
      (progn (warn-user "The knowledge base is about to be searched.
This could take a long time.")
	     (find-all-options-for-theorem theorem-name proof)
	     (user-apply-theorem theorem-name t)))))

;;; Make this allow orig-name or maybe even string.

(defun get-matching-option (theorem-name proof searched)
  (if (eq theorem-name
	  (kb-theorem-name (option-theorem (car (options proof)))))
      ;; Benji !@#$%^&*
      (pop (options proof))
    (do ((options (options proof) (cdr options)))
	((null (cdr options))
	 (if searched (return (warn-user "That theorem does not apply here."))
	   (progn (warn-user "The knowledge base is about to be searched.
This could take a long time.")
		  (find-all-options-for-theorem theorem-name proof)
		  (get-matching-option theorem-name proof t))))
      (and (eq theorem-name (kb-theorem-name (option-theorem (cadr options))))
	   (return (pop (cdr options)))))))

(defun toggle-watch ()
  (emacs-eval
   `(message ,(quote-string
	       (format nil "Watching is now ~:[off~;on~]."
		       (setq *watch* (not *watch*)))))))

(defun toggle-interact ()
  (emacs-eval
   `(message ,(quote-string
	       (format nil "Interacting is now ~:[off~;on~]."
		       (setq *interact* (not *interact*)))))))

(defun toggle-theory ()
  (emacs-eval
   `(message ,(quote-string
	       (format nil "Interacting is now ~:[off~;on~]."
		       (setq *theory* (not *theory*)))))))

(defun toggle-apply-first ()
;;;  (emacs-eval
;;;   `(message ,(quote-string
;;;	       (format nil "Unification is now ~
;;;~[breadth-first~;apply the first found immediately~
;;;~;find only one but don't apply it~]."
;;;		       (case (setq *how-to-unify*
;;;			       (case *how-to-unify*
;;;				 (:bt :apply)
;;;				 (:apply :one)
;;;				 (:one :bt)))
;;;			 (:bt 0)
;;;			 (:apply 1)
;;;			 (:one 2))))))
  )

