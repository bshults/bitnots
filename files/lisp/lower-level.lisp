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



;;; This file contains miscellaneous code that probably belongs

;;; elsewhere.



;;;(defun one-step (&optional (proof *proof*) (cat-list '(:a :d :set-theory :b

;;;						       :unify :c :math)) skip)

;;;;;; Compiling ONE-STEP

;;;;;;Warning: variable *UNIFY-AFTER* is never used

;;;;;;Warning: variable SKIP is never used

;;;  ;; Called by carriage-return: go-under-here and break-down1.

;;;  ;; This should be altered to take the new ways into account.

;;;  (declare (type proof proof) (list cat-list))

;;;  (time-it

;;;   (dolist (leaf (leaves proof :open-only t))

;;;     (declare (type proof leaf))

;;;     (let ((*low-proof* leaf))

;;;       (dolist (cat cat-list)

;;;	 (when (let ((*unify-after* t))

;;;		 (case cat

;;;		   (:a (pop-alpha-here))

;;;		   (:d (pop-delta-here))

;;;		   (:set-theory (prove-set-theory-here))

;;;		   (:b (prove-beta-here))

;;;		   (:c (prove-gamma-here))

;;;		   (:math (prove-math-here))))

;;;	   (return (return leaf))))))))



(defun tableaux-gamma-rule (proof)

  ;; Called by PROVE-GAMMA.

  ;; Why not intersect *gammas* with hyps and goals.

  (declare (type proof proof))

  (let* ((least (least-finder (remove-if-not

			       #'(lambda (f) (and (eq 'show-there-is (rule f))

						  (eligible? f)))

			       (append (hypotheses proof) (goals proof))))))

    (declare (type (or null formula) least))

    ;; If none of these are gammas, then 1> if (and *theory* *kb-theorems*)

    ;; go on to math [I like this.] or 2> go to some sequent which has a

    ;; gamma-formula.

    (when least

      (incf (copies least))

      (show-there-is9 least)

;;;     (when *new-alpha* (reducer (birth-place least)) (setq *new-alpha* nil))

      (when (and *equality* *new-equals*)

	(new-equality-formula) (setq *new-equals* nil))

      t)))



(defun sequent-gamma-rule-here (proof)

  ;; Called by PROVE-GAMMA.

  ;; Why not intersect *gammas* with hyps and goals.

  (declare (type proof proof))

  (let* ((least (least-finder (remove-if-not

			       #'(lambda (f) (and (eq 'show-there-is (rule f))

						  (eligible? f)))

			       (append (hypotheses proof) (goals proof))))))

    (declare (type (or null formula) least))

    ;; If none of these are gammas, then 1> go on to math [I like

    ;; this.] or 2> go to some sequent which has a gamma-formula.

    (when least

      (setq *high-proof* proof)

      ;; Maybe pop it out of *now-gammas* if this makes it too high.

      ;; I.e. keep track of eligible gammas separately from gammas.

      (incf (copies least))

      (show-there-is9 least))))



(defun eligible? (obj)

  ;; Is it permissible to remove the quantifier?  Might check reject

  ;; stuff in the future.

  (declare (type formula obj))

;;;  (< (copies obj) (* *copy-max* *copy-max*))

  (< (copies obj) *copy-max*))



(defun least-finder (list)

  (declare (list list))

  (when list

    (dotimes (num (* *copy-max* *copy-max*))

      (let ((return (some #'(lambda (elt) (declare (type formula elt))

				    (and (= (copies elt) num) elt)) list)))

	(declare (type (or null formula) return))

	(when return (return return))))))



;;; I thought about having *copy-max* provides both minumum and

;;; maximum of the possible values of copies.  But I want the max to

;;; be the square of *copy-max* so that some formulas can lag behind

;;; while others are used more frequently.



;;; So if the user thinks that a particular formula should be expanded

;;; more times than other formulas then he can just decrease the value

;;; of (copies obj-formula).  I would also like to be able to pick

;;; only certain variables in a formula for extra expansion.  Make a

;;; command for this.



