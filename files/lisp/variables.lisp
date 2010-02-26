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





;;; This file deals with variables.  This stuff might belong in the


;;; formulas.lisp file.





;;; This (safe-replace) could be made more conservative by associating


;;; with each b-v list of vars it must not conflict with.


;;; This would be hard.





(defun occurs-in-term (var term)


  ;; everything is internal


  (declare (symbol var) (type term term))


  (if (consp term)


      (case (car term)


	((the the-class-of-all) (occurs-in-formula var (caddr term)))


	(t (some #'(lambda (tr) (occurs-in-term var tr)) (cdr term))))


    (eq var term)))





(defun occurs-in-formula (var form)


  ;; everything is internal


  (case (car form)


    ((forall for-some) (occurs-in-formula var (caddr form)))


    ((and or not implies iff if)


     (some #'(lambda (f) (occurs-in-formula var f)) (cdr form)))


    (t (some #'(lambda (tr) (occurs-in-term var tr)) (cdr form)))))





(defun unbound-vars-in-formula (formula &optional bound-vars)


  ;; Use occurs-unbound-in if you have one particular


  ;; var you want to check.


  (declare (type formula formula) (list bound-vars))


  (remove-duplicates (unbound-vars-in-formula1 formula bound-vars)
                     :test #'eq))



(defun unbound-vars-in-formula1 (formula bound-vars)


  (declare (type formula formula) (list bound-vars))


  (case (car formula)


    ((forall for-some)


     (unbound-vars-in-formula1


      (caddr formula) (nconc (mapcar 'car (cadr formula)) bound-vars)))


    ((and or not implies iff if)


     (mapcan #'(lambda (form) (declare (list form))


		 (unbound-vars-in-formula1 form bound-vars)) (cdr formula)))


    (t (mapcan #'(lambda (term) (declare (type term term))


		   (unbound-vars-in-term1 term bound-vars)) (cdr formula)))))





(defun unbound-vars-in-term (term &optional bound-vars)


  (declare (type term term) (list bound-vars))


  (remove-duplicates (unbound-vars-in-term1 term bound-vars) :test #'eq))





(defun unbound-vars-in-term1 (term bound-vars)


  (declare (type term term) (list bound-vars))


  (if (consp term)


      (case (car term)


	((the the-class-of-all) (unbound-vars-in-formula1


				 (caddr term) (cons (caadr term) bound-vars)))


	(t (mapcan #'(lambda (trm) (declare (type term trm))


		       (unbound-vars-in-term1 trm bound-vars)) (cdr term))))


    (and (not (member term bound-vars))


	 (list term))))





(defun replace-unbound-occurrences-in-formula (new old formula)


  ;; called by show-there-is3 rename-internal-bound-vars-in-term


  ;; make-term-internal remove-the3 etc.


  ;; make sure the caller of this don't need safety.


  ;; new and old are symbols


  ;; this is used by make-internal-formula to replace external variables


  ;; with internal variables


  (declare (symbol old) (list formula) (type term new))


  (case (car formula)


    ((forall for-some)


     (list (car formula) (cadr formula)


	   (replace-unbound-occurrences-in-formula new old (caddr formula))))


    ((and or implies if not iff)


     (cons (car formula)


	   (mapcar #'(lambda (form) (declare (list form))


		       (replace-unbound-occurrences-in-formula new old form))


		   (cdr formula))))


    (t (cons (car formula)


	     (mapcar #'(lambda (term) (declare (type term term))


			 (replace-unbound-occurrences-in-term new old term))


		     (cdr formula))))))





(defun replace-unbound-occurrences-in-term (new old term)


  ;; only called by FORMULA version and SELF


  ;; New and old are symbols.


  ;; This is used by make-internal-formula to replace external variables


  ;; with internal variables.


  (declare (symbol old) (type term new term))


  (if (consp term)


      (case (car term)


	((the the-class-of-all)


	 `(,(car term) ,(cadr term) ,(replace-unbound-occurrences-in-formula


				      new old (caddr term))))


	(t (cons (car term)


		 (mapcar #'(lambda (trm) (declare (type term trm))


			     (replace-unbound-occurrences-in-term new old trm))
			 (cdr term)))))


    (if (eq old term) new term)))





;;;(defun sublis-unbound-occurrences-in-formula (alist formula)


;;;  (declare (list alist formula))


;;;  (if alist (sublis-unbound-occurrences-in-formula1 alist formula)


;;;    formula))





;;;(defun sublis-unbound-occurrences-in-formula1 (alist formula &aux


;;;						     (car (car formula)))


;;;  (declare (list alist formula) (symbol car))


;;;  (case car


;;;    ((forall for-some)


;;;     (list car (cadr formula)


;;;	   (sublis-unbound-occurrences-in-formula1 alist (caddr formula))))


;;;    ((and or implies if not iff)


;;;     (cons car (mapcar #'(lambda (form)


;;;			   (sublis-unbound-occurrences-in-formula1 alist form))
;;;		       (cdr formula))))


;;;    (t (cons car (mapcar #'(lambda (term)


;;;			     (sublis-unbound-occurrences-in-term1 alist term))


;;;			 (cdr formula))))))





;;;(defun sublis-unbound-occurrences-in-term (alist term)


;;;  ;; SET is T if we want the forces to be set to the new value.


;;;  ;; If SET then we are not replacing the var with its force,


;;;  ;; we are only resetting the force of the var so that it will


;;;  ;; reflect the change.


;;;  ;; It seems that this should always set, otherwise it's not really


;;;  ;; doing anything in the case where everything is under a force.


;;;  ;; Unless this sometimes just needs to return the expanded term


;;;  ;; with forces expanded.


;;;  (declare (list alist) (type term term))


;;;  (if alist (sublis-unbound-occurrences-in-term1 alist term)


;;;    term))





;;;(defun sublis-unbound-occurrences-in-term1 (alist term)


;;;  (if (consp term)


;;;      (case (car term)


;;;	((the the-class-of-all)


;;;	 `(,(car term) ,(cadr term)


;;;	   ,(sublis-unbound-occurrences-in-formula1 alist (caddr term))))


;;;	(t (cons (car term)


;;;		 (mapcar #'(lambda (trm)


;;;			     (sublis-unbound-occurrences-in-term1 alist trm))


;;;			 (cdr term)))))


;;;    (or (cdr (assoc term alist)) term)))





