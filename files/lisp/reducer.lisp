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



;;; This file implements functions to rewrite formulas.  I think this

;;; just splits alpha formulas so that alpha-rules are applied without

;;; more nodes being added to the tree.  E.g., if a beta-rule

;;; introduces an alpha-formula, then that alpha-formula is split into

;;; its subformulas and those are put into the node created by the

;;; beta-rule.



(defun reducer (&optional (proof *proof*))

  (declare (type proof proof))

  (mapc #'(lambda (p) (reduce-formula p)) (leaves proof)))



;;;(defun reduce1 (proof)

;;;  ;; this now only reduces leafs

;;;  (declare (type proof proof))

;;;  (if (subproofs proof)

;;;      (mapcar #'(lambda (sub)

;;;		  (declare (type proof sub))

;;;		  (reduce1 sub)) (subproofs proof))

;;;    (reduce-formula proof)))



(defun reduce-formula (proof)

  ;; one possible improvement that could be made here is only to

  ;; reduce the new juncts.

  (declare (type proof proof))

  (setf (new-goals proof)

    (mapcan #'(lambda (gl)

		(if (not (eq (rule gl) 'alpha))

                    (list gl)

		  ;; fix the birth-place of new formulas.

		  (progn

		    (setf (new-goals proof) (delete gl (new-goals proof)))

		    (mapcar #'(lambda (f)

                                (setf (birth-place f) proof)

				(push f (new-goals proof))

				(push f (replacements gl)) f)

			    (reduce-djunct gl)))))

            (new-goals proof)))

  (setf (new-hyps proof)

    (mapcan #'(lambda (hyp)

		(if (not (eq 'alpha (rule hyp))) (list hyp)

		  (progn

		    (setf (new-hyps proof) (delete hyp (new-hyps proof)))

		    (mapcar #'(lambda (f) (setf (birth-place f) proof)

				(push f (new-hyps proof))

				(push f (replacements hyp)) f)

			    (reduce-cjunct hyp)))))

            (new-hyps proof))))



(defun reduce-djunct (djt)

  "Takes a disjunct of a goal and returns a list of disjuncts

with which the original should be replaced."

  (declare (type formula djt))

  (mapcar #'(lambda (f) (declare (list f))

		    (make-formula-from-old f djt :parents-of (parents-of djt)))

	  (reduce-d (formula djt))))



(defun reduce-d (djunct)

  (declare (list djunct))

  (case (car djunct)

    (falsity nil)

    (or (mapcan #'(lambda (dj) (declare (list dj)) (reduce-d dj))

		(cdr djunct)))

    (if (let ((new-if (reduce-if

		       (cadr djunct) (caddr djunct) (cadddr djunct))))

	  (if (eq (car new-if) 'or) (reduce-d new-if) (list new-if))))

    (implies (let ((new-imp (reduce-implies (cadr djunct) (caddr djunct))))

	       (declare (list new-imp))

	       (if (eq (car new-imp) 'or) (reduce-d new-imp) (list new-imp))))

    (iff			; this may take more than two args.

     (let ((new-iff (reduce-iff djunct nil)))

       (declare (list new-iff)) new-iff))

    (not (case (caadr djunct)

	   (and (mapcan #'(lambda (cjunct)

			    (declare (list cjunct))

			    (reduce-d (list 'not cjunct))) (cdadr djunct)))

	   (not (reduce-d (cadadr djunct)))

	   (t (list djunct))))

    (t (list djunct))))



(defun reduce-cjunct (cjt)

  "Takes a conjunct of a proof and returns a list of conjuncts

with which the original should be replaced."

  (declare (type formula cjt))

  (mapcar #'(lambda (f) (declare (list f))

		    (make-formula-from-old f cjt :parents-of (parents-of cjt)))

	  (reduce-c (formula cjt))))



(defun reduce-c (cjunct)

  (declare (list cjunct))

  (case (car cjunct)

    (truth nil)

    (and (mapcan #'(lambda (cj)

		     (declare (list cj))

		     (reduce-c cj)) (cdr cjunct)))

    (iff (reduce-iff cjunct t))

    (if (let ((new-if (reduce-if

		       (cadr cjunct) (caddr cjunct) (cadddr cjunct))))

	  (declare (list new-if))

	  (if (eq (car new-if) 'if) (list new-if) (reduce-c new-if))))

    (implies (let ((new-imp (reduce-implies (cadr cjunct) (caddr cjunct))))

	       (declare (list new-imp))

	       (if (eq (car new-imp) 'implies)

		   (list new-imp) (reduce-c new-imp)))) ; #

    (not (case (caadr cjunct)

	   (or (mapcan #'(lambda (djunct)

			   (declare (list djunct))

			   (reduce-c (list 'not djunct)))

		       (cdadr cjunct)))

	   (not (reduce-c (cadadr cjunct)))

	   (implies (append (reduce-c (cadadr cjunct))

			    (reduce-c (list 'not (caddr (cadr cjunct))))))

	   (t (list cjunct))))

    (t (list cjunct))))



(defun reduce-iff (formula cjunct?)

  ;; # this needs to be studied

  (declare (list formula) (symbol cjunct?))

  (if cjunct?

      (let ((first (reduce-c (cadr formula)))

	    (second (reduce-c (caddr formula))))

	(cond ((null first) second)

	      ((null second) first)

	      ((member '(falsity) first :test 'equal)

	       (when (not (member '(falsity) second :test 'equal))

		 (case (length second)

		   (1 `((not ,(car second))))

		   (t `((or ,@(mapcar #'(lambda (junct)

					  (list 'not junct)) second)))))))

	      ((member '(falsity) second :test 'equal)

	       (case (length first)

		 (1 `((not ,(car first))))

		 (t `((or ,@(mapcar #'(lambda (junct)

					(list 'not junct)) first))))))

	      ((form-equal first second) nil)

	      (t `((iff ,(case (length first) (1 (car first))

			       (t `(and ,@first)))

			,(case (length second)

			   (1 (car second)) (t `(and ,@second))))))))

    (let ((first (reduce-d (cadr formula)))

	  (second (reduce-d (caddr formula))))

      (cond ((null first)

	     (if (null second) '((truth))

	       (case (length second)

		 (1 `((not ,(car second))))

		 (t `((and ,@(mapcar #'(lambda (junct)

					 `(not ,junct)) second)))))))

	    ((null second)

	     (case (length first)

	       (1 `((not ,(car first))))

	       (t `((and ,@(mapcar #'(lambda (junct)

				       `(not ,junct)) first))))))

	    ((member '(truth) first :test 'equal)

	     (if (member '(truth) second :test 'equal)

		 '((truth))

	       (case (length second)

		 (1 `(,(car second)))

		 (t `((or ,@second))))))

	    ((member '(truth) second :test 'equal)

	     (case (length first)

	       (1 `(,(car first)))

	       (t `((or ,@first)))))

	    ((form-equal first second) '((truth)))

	    (t `((iff ,(case (length first) (1 (car first))

			     (t `(or ,@first)))

		      ,(case (length second)

			 (1 (car second)) (t `(or ,@second))))))))))



(defun reduce-implies (hyp conc)

  (declare (list hyp conc))

  (cond ((equal '(truth) hyp) conc)

	((equal '(falsity) hyp) '(truth))

	((equal '(truth) conc) '(truth))

	((equal '(falsity) conc) `(not ,hyp))

	((eq (car hyp) 'not) `(or ,(cadr hyp) ,conc))

	((eq (car conc) 'implies)

	 (let ((new-hyp (remove-duplicates

			 (mapcan 'reduce-c (list hyp (cadr conc)))

			 :from-end t :test 'form-equal)))

	   (declare (list new-hyp))

	   (setq new-hyp (case (length new-hyp)

			   (0 '(truth))

			   (1 (car new-hyp))

			   (t `(and ,@new-hyp))))

	   `(implies ,new-hyp ,(caddr conc))))

	(t `(implies ,hyp ,conc))))



(defun reduce-if (pred f1 f2)	; could be better

  (declare (list pred f1 f2))

  (cond ((equal '(truth) pred) f1)

	((equal '(falsity) pred) f2)

	((equal '(falsity) f2) `(and ,pred ,f1))

	((equal '(truth) f2) `(implies ,pred ,f1))

	((equal '(truth) f1) `(or ,pred ,f2))

	((equal '(falsity) f1) `(not (implies ,f2 ,pred)))

	(t `(if ,pred ,f1 ,f2))))



