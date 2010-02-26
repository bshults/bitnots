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



;;; This file implements the higher-order logic rules such as

;;; comprehension, iota, extensionality, etc.



;;; Set Theory:



(defun comprehension-schema-step (&key goal hyp)

  (let ((formula (if hyp (nth hyp (hypotheses *low-proof*))

		   (nth goal (goals *low-proof*)))))

    (declare (type formula formula))

    (if *tableaux* (comprehension-schema5 formula)

      (comprehension-schema2 *low-proof* formula (or goal hyp))))

  (subproofs *low-proof*))



(defun comprehension-schema (formula)	; called by prove-set-theory

  ;; Returns non-nil on success.

  (declare (type formula formula))

  (when (comprehension-schema-applicable formula)

    (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))

			  (format t "~%{") (format t "{")))

    (comprehension-schema5 formula)))



(defun comprehension-schema5 (formula &aux (proof (birth-place formula)))

  ;; Should return non-nil.

  (declare (type formula formula) (type proof proof))

  (mapc #'(lambda (pf)

	    (let ((pos (if (sign formula) (position formula (hypotheses pf))

			 (position formula (goals pf)))))

	      (declare (type (integer 0 *) pos))

	      (comprehension-schema2 pf formula pos)))

	(leaves proof)))



(defun comprehension-schema2 (proof formula pos)

  "This takes a proof and applies tbe Comprehension Schema

to the indicated formula."

  (declare (type formula formula) (type proof proof) (type (integer 0 *) pos))

  (setf (subproofs proof)

    (if *fol-lemmas* (comprehension-schema4 proof formula pos)

      (comprehension-schema3 proof formula))))



(defun comprehension-schema3 (proof formula)

  "This takes a proof and applies Comprehension Schema to the indicated formula."

  (declare (type formula formula) (type proof proof))

  (let* ((junct 

	  (if (sign formula) (formula formula)

	    (if *renaming-universals* (rename-universal-variables-in-formula 

				       (formula formula)) (formula formula))))

	 (set-elt (cadr junct)) (predicate (third (third junct)))

	 (set-var (caadr (caddr junct)))

	 (inst-predicate (replace-unbound-occurrences-in-formula 

			  set-elt set-var predicate))

	 (new-pred (make-formula-from-old inst-predicate formula))

	 (new-goals1 (copy-list (goals proof))))

    (declare (list junct inst-predicate new-hyps1 new-goals1)

	     (type formula new-pred) (symbol set-var))

    (setf (whither proof) (if (sign formula) :a :b))

    (setf (plan proof)

      `((comprehension-schema ,(externalize-formula (formula formula)) goal)))

    (if (sign formula)

	(let ((subs (list

		     (make-sequent

		      :new-hyps

		      (list (make-formula-from-old

			     `(an-element ,set-elt) formula) new-pred)

		      :superproof proof :depth (1+ (depth proof))))))

	  (setf (birth-place new-pred) (car subs)

		(birth-place (car (new-hyps (car subs)))) (car subs))

	  (set-splits proof (car subs))

	  (when *new-alpha*

	    (reduce-formula (car subs))

	    (setq *new-alpha* nil))

	  subs)

      (let ((subs (list (make-sequent

			 :new-goals (list new-pred) :superproof proof

			 :depth (1+ (depth proof)) :index 1)

			(make-sequent

			 :superproof proof

			 :new-goals (list (make-formula-from-old

					   `(an-element ,set-elt) formula))

			 :depth (1+ (depth proof)) :index 2))))

	(setf (split proof) (make-split :node proof :vars nil))

	(setf (birth-place new-pred) (car subs)

	      (birth-place (car (new-goals (cadr subs)))) (cadr subs))

	(push (split proof) (splits new-pred))

	(push (split proof) (splits (car (new-goals (cadr subs)))))

	subs))))



(defun comprehension-schema-applicable (formula)

  ;; Assumes that the car of formula is a-member-of.

  (and (consp (caddr (formula formula)))

       (eq (car (caddr (formula formula))) 'the-class-of-all)))



(defun class-schema-step (goal proof)

;;;    (setf (changed-since-last-unification proof) t)

  (when (class-schema-applicable goal)

    (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))

			  (format t "~%{") (format t "{")))

    (setf (plan proof)

      `((class-schema ,(externalize-formula (formula goal)))))

    (class-success goal proof)))



(defun class-schema (goal &aux (proof (birth-place goal)))

					; called by prove-set-theory

  (mapc #'(lambda (pf) (class-schema-step goal proof))

	(leaves proof)))



(defun class-success (goal proof)

  (declare (type proof proof) (type formula goal))

  (let ((init (make-init :used-goals (list goal) :init-plan (plan proof)

			 :sequent proof)))

    (declare (type init-sub init))

    ;; This init needs to know about the option that created it so

    ;; that the plan can tell how it was closed.

    (push init (successes proof))

    (push init (theorem-successes proof))

    (push init (trivial-closures proof))))



(defun class-schema-applicable (formula)

  ;; Assumes that the car of formula is 'a-class.

  (and (consp (cadr (formula formula)))

       (eq (car (cadr (formula formula))) 'the-class-of-all)))



;;;;;; an-element in the goals is now handled by the unifier.



;;;(defun element-schema-step (goal proof)

;;;  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))

;;;			(format t "~%{") (format t "{")))

;;;  (setf (plan proof)

;;;    `((element-schema ,(externalize-formula (formula goal)))))

;;;  (element-success goal proof))



;;;(defun element-schema (goal &aux (proof (birth-place goal)))

;;;					; called by prove-set-theory

;;;  (mapc #'(lambda (pf) (element-schema-step goal proof))

;;;	(leaves proof)))



;;;(defun element-success (goal proof)

;;;  (declare (type proof proof) (type formula goal))

;;;  ;; really set-hyps should return a list of inits because the elt may

;;;  ;; be unifiable with some vars.  Also, this is not complete because

;;;  ;; more formulas may be spliced up above and they will need to be

;;;  ;; checked.  See how Brown's rule solves this similar problem.

;;;  ;; Maybe this is something the unifier should do.  It may be easier

;;;  ;; for it to handle this kind of thing.

;;;  (let ((succ (set-hyps proof (cadr (formula goal)))))

;;;    (when succ

;;;      (let ((init (make-init :used-goals (list goal) :init-plan (plan proof)

;;;			     :used-hyps (list succ) :sequent proof)))

;;;	(declare (type init-sub init))

;;;	;; This init needs to know about the option that created it so

;;;	;; that the plan can tell how it was closed.

;;;	(push init (successes proof))

;;;	(push init (theorem-successes proof))

;;;	(push init (trivial-closures proof))))))



;;; iota removal



(defun remove-the-step (path &key goal hyp)

  (let ((formula (if hyp (nth hyp (hypotheses *low-proof*))

		   (nth goal (goals *low-proof*)))))

    (if *tableaux* (remove-the5 path formula)

      (remove-the2 *low-proof* path formula (or goal hyp))))

  (subproofs *low-proof*))



(defun remove-the (path formula)

  (remove-the5 path formula))



(defun remove-the5 (path formula &aux (proof (birth-place formula)))

  (mapc #'(lambda (pf)

	    (let ((pos (if (sign formula)

			   (position formula (hypotheses proof))

			 (position formula (goals proof)))))

	      (remove-the2 pf path formula pos)))

	(leaves proof)))



(defun remove-the2 (proof path formula pos)

  (declare (type proof proof) (type formula formula) (type (integer 0 *) pos))

  (setf (subproofs proof) (remove-the3 proof path formula pos)))



(defun remove-the3 (proof path formula pos)

  ;; Only one of hyp or goal should be supplied.  It is the index if the

  ;; formula to be changed at path.

  (declare (type proof proof) (type formula formula) (type (integer 0 *) pos))

  (let* ((chyps (copy-list (hypotheses proof)))

	 (cgoals (copy-list (goals proof))) new-junct

	 (the (ipr-follow-path (formula formula) path)) (the-pred (caddr the))

	 (internal-the-var (caadr the)) (the-var (ext-name internal-the-var))

	 (free-in-the (unbound-vars-in-term (constantize-term the) nil))

	 ;; I may want externalize to go from free-params to constants

	 ;; when possible.  No, this would mess up reject term.

	 ;; so I need something special here.

	 (free-in-the-pred (unbound-vars-in-formula

			    (constantize-formula the-pred) nil))

	 ;; what about dependencies?

	 (c-var (make-var-with-new-name

		 the-var (append free-in-the free-in-the-pred)))

	 (r-var (make-var-with-new-name

		 the-var (cons (ext-name c-var) free-in-the-pred)))

	 (n-var (make-var-with-new-name

		 the-var (cons (ext-name r-var) free-in-the-pred))))

    (setf (plan proof)

      `((the-removal ,(externalize-formula (formula formula)))))

    (setq new-junct

      (make-formula-from-old 

       `(if (for-some ((,r-var))

	      (and ,(replace-unbound-occurrences-in-formula

		     r-var internal-the-var the-pred)

		   (an-element ,r-var)

		   (forall ((,n-var))

		     (implies (and (an-element ,n-var)

				   ,(replace-unbound-occurrences-in-formula

				     n-var internal-the-var the-pred))

		       (= ,r-var ,n-var)))))

	    (forall ((,c-var))

	      (implies (and ,(replace-unbound-occurrences-in-formula

			      c-var internal-the-var the-pred)

			    (an-element ,c-var))

		,(substitute-at-path (formula formula) path c-var)))

	  ,(substitute-at-path (formula formula) path *the-undefined-thing*))

       formula))

    (list (make-sequent 

	   :hypotheses (if (sign formula)

			   (if (zerop pos)

			       (cons new-junct (cdr chyps))

			     (let ((temp (remove-nth pos chyps)))

			       (push-nthcdr new-junct pos temp)))

			 chyps)

	   :goals (if (sign formula) cgoals

		    (if (zerop pos)

			(push new-junct (cdr cgoals))

		      (let ((temp (remove-nth pos cgoals)))

			(push-nthcdr new-junct pos temp))))

;;;	   :gammas

;;;	   (let ((list (remove-if-not

;;;			#'(lambda (f) (eq 'show-there-is (rule f)))

;;;			(list new-junct))))

;;;	     (if list (append (gammas proof) list)

;;;	       (gammas proof)))

	   :superproof proof

;;;	   :applications-to-branch (applications-to-branch proof)

;;;	   :location (concatenate 'list (location proof) '(1))

	   :depth (1+ (depth proof))))))



