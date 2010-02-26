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

;;; This file contains the basic unification algorithm.  It ensures
;;; that substitutions remain idempotent.

(defun unify-with-goal (goal proof =hyps)
  (declare (type proof proof) (type formula goal) (list =hyps))
  ;; returns the cons of goal and the unifying hypothesis if it exists
  ;; otherwise nil
  (dolist (hyp (hypotheses proof))
    (declare (type formula hyp))
    (setq *equality-used* nil)
    (let ((f-cache (unify-obj-formulas goal hyp)))
      (and (listp f-cache)
	   (return (success proof (list goal)
			    (cons hyp (and *equality-used* =hyps))
			    f-cache))))))

(defun success (proof goals hyps f-cache)
  ;; Depending on who calls this, it may not do any good to push the
  ;; init into successes (because they are removed by bt-unify), and
  ;; thus, this doesn't really do any good.
  (let ((init (make-init :f-cache f-cache :used-goals goals
			 :sequent proof :used-hyps hyps)))
    (push init (successes proof))
    (and (null f-cache) (push init (trivial-closures proof)))
    init))

(defun add-to-f-cache (pair f-cache)
  ;; preserves idempotence
  (cons pair (mapcar #'(lambda (p)
                         (cons (car p)
                               (replace-unbound-occurrences-in-term
                                (cdr pair) (car pair) (cdr p))))
                     f-cache)))

(defun unify-obj-formulas (form1 form2)
  (declare (type formula form1 form2))
  (unify-formulas (formula form1) (formula form2) nil))

(defun unify-formulas (form1 form2 f-cache)
  (send-formulas-to-unifier
   (instantiate-formula form1 f-cache)
   (instantiate-formula form2 f-cache) f-cache))
;;;  (let* ((f1 (instantiate-formula form1 f-cache))
;;;	 (f2 (instantiate-formula form2 f-cache)))
;;;    (let ((u (send-formulas-to-unifier f1 f2 f-cache)))
;;;      (when (and *error-checking* (listp u) (not (idempotent u)))
;;;	(warn-user "~%~S~%is not idempotent" u)
;;;	(format t "~%~S~%is not idempotent" u))
;;;      u)))

;;;(defun idempotent (u)
;;;  (equal (mapcar #'(lambda (p) (cons (car p) (sublis u (cdr p)))) u) u))

(defun send-formulas-to-unifier (f1 f2 f-cache)
  ;; Wait a minute, benji $#%^.  If *ground* then why not just check
  ;; alpha-equivalence?
  (case (form-equal-spec f1 f2)
    (:clash :fail)
    (:succeed f-cache)
    (:fail (unify-formulas1 f1 f2 f-cache))))

(defun same-length (a b)
  (do ((c a (cdr c)) (d b (cdr d))) ((null c) (null d))
    (and (null d) (return nil))))

(defun unify-formulas1 (form1 form2 f-cache)
  ;; the caller guarantees no clash
  (case (car form1)
    ((forall for-some)
     (if (same-length (cadr form1) (cadr form2))
	 (let* ((alist1 (mapcar #'(lambda (var1)
				    (cons var1 (let ((var (gentemp)))
						 (setf (bound var) t)
						 (list (gentemp)))))
				(mapcar 'car (cadr form1))))
		(incnum 0)
		(alist2 (mapcar #'(lambda (var2)
				    (prog1 (cons var2 (cdr (nth incnum
								alist1)))
				      (incf incnum)))
				(mapcar 'car (cadr form2)))))
	   (declare (list alist1 alist2) (type (integer 0 *) incnum))
	   ;; here f-cache has not changed
	   (unify-formulas1 (instantiate-formula (caddr form1) alist1)
			    (instantiate-formula (caddr form2) alist2)
			    f-cache))
       :fail))
    ((and or not iff implies if)
     (if (and (same-length form1 form2)
	      (every #'(lambda (f1 f2)
			 (declare (list f1 f2))
			 ;; here f-cache may have changed
			 (let ((fc (unify-formulas f1 f2 f-cache)))
			   (when (listp fc) (setq f-cache fc) t)))
		     (cdr form1) (cdr form2)))
	 f-cache :fail))
    (t (if (every
	    #'(lambda
		  ;; here f-cache may have changed
		  (term1 term2 &aux (f-c (unify-terms term1 term2 f-cache)))
		(declare (type term term1 term2))
		(and (listp f-c) (or (setq f-cache f-c) t)))
	    (cdr form1) (cdr form2)) f-cache :fail))))

(defun unify-terms (term1 term2 f-cache)
  ;; be sure that force slots are expanded in term1 and term2 before they
  ;; are passed here.
  (declare (type term term1 term2) (type (or symbol list) f-cache))
  (let* ((t1 (instantiate-term term1 f-cache))
	 ;; for internal terms, this should not be needed
	 (t2 (instantiate-term term2 f-cache)))
    ;; it can happen that no substitution has been made and yet
    ;; t1 and t2 are term-equal (eq even) even though form-equal
    ;; failed on the formulas e.g. (R t1 t3) (R t1 t2) so I need
    ;; to take care of this case.
    (case (term-equal-spec t1 t2)
      (:clash :fail)
      (:succeed f-cache)
      (:fail (unify-terms1 t1 t2 f-cache)))))

(defun unify-complex-terms (t1 t2 f-cache)
  ;; t2 and t1 are both terms with the same car
  ;; clashing cannot occur here so the cars must be eq.
  (case (car t1)
    ((the-class-of-all the)
     ;; If both of the caddrs are in the hyps or they are both in the
     ;; goals then the two are equal.  generally, if i can show 
     ;; (iff (caddr t1) (caddr t2)) then the terms are term-equal.%@#$
     ;; form-equal would have taken care of this if it were very
     ;; simple.  I may fix this some time.$#benji
     (let* ((alist1 (list (cons (caadr t1)
				(let ((var (gentemp)))
				  (setf (bound var) t)
				  (list (gentemp))))))
	    (alist2 (list (cons (caadr t2) (cdar alist1)))))
       (unify-formulas1
	;; the below does not apply f-cache because f-cache has not changed
	(instantiate-formula (caddr t1) alist1)
	(instantiate-formula (caddr t2) alist2)
	f-cache)))
    (t (if (every #'(lambda (f1 f2)
;;; unify-terms here because f-cache may have changed
		      (let ((f-c (unify-terms f1 f2 f-cache)))
			(when (listp f-c)
                          (setq f-cache f-c)
                          t)))
		  (cdr t1) (cdr t2))
           f-cache
         :fail))))

(defun unify-terms1 (term1 term2 f-cache)
  ;; F-CACHE has already been applied to the terms.
  ;; the caller guarantees no clash at top level (and no identity)
  (declare (type term term1 term2))
  (if (symbolp term1)
      (if (symbolp term2)
	  ;; TERM1 and TERM2 are not the same variable they need to be
	  ;; ordered.  Prefer to bind universals and younger rigids.
	  ;; Prefer to bind vars with no order because they come from
	  ;; theorems and are really universal.  If there is a var for
	  ;; a var substitution, I want the sequent variable to
	  ;; remain.  I.e. I want the theorem variable to be replaced
	  ;; by the sequent variable.
	  (if (order term1)
	      (if (order term2)
		  (if (< (order term1) (order term2))
		      ;; term1 is older
		      (add-to-f-cache (cons term2 term1) f-cache)
		    ;; term2 is older
		    (add-to-f-cache (cons term1 term2) f-cache))
		;; term2 has no order so it is from the KB
		(add-to-f-cache (cons term2 term1) f-cache))
	    ;; term1 has no order so it is from the KB
	    (add-to-f-cache (cons term1 term2) f-cache))
	;; term2 is not a symbol
	(if (or (occurs-in-term term1 term2)
		(temp-bound-var-in-term term2))
	    ;; occurs-in-term, I also need to check if any bound
	    ;; variables (i.e. (list (gentemp))s) occur free in
	    ;; term2.  benji @%#$
	    :fail
	  (add-to-f-cache (cons term1 term2) f-cache)))
    ;; term1 is not a symbol
    (if (symbolp term2)
	(if (or (occurs-in-term term2 term1)
		(temp-bound-var-in-term term1))
	    :fail
	  (add-to-f-cache (cons term2 term1) f-cache))
      ;; term2 and term1 are both terms
      ;; clashing cannot occur here so the cars must be eq.
      (unify-complex-terms term1 term2 f-cache))))

(defun temp-bound-var-in-term (term)
  (declare (type term term))
  (when (consp term)
    (case (car term)
      ((the the-class-of-all) (temp-bound-var-in-formula (caddr term)))
      (t (if (not (cdr term)) (bound (car term))
	   (some #'temp-bound-var-in-term (cdr term)))))))

(defun temp-bound-var-in-formula (form)
  (declare (list form))
  (case (car form)
    ((forall for-some) (temp-bound-var-in-formula (caddr form)))
    ((and or if iff implies not) (some #'temp-bound-var-in-term (cdr form)))
    (t (some #'temp-bound-var-in-term (cdr form)))))

