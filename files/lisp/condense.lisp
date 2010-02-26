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



;;; This file implements the HARP condense algorithm and more.  When a

;;; proof is found, this takes out all formulas that were not used.

;;; It also handles the fact that I am using breadth-first unification.



(defun bf-condense ()

  ;; This assumes that *done* finishes the proof of *proof*

  (and *condense*

       (mapc #'(lambda (init &aux (sequent (sequent init)))

		 (declare (type init-sub init) (type proof sequent))

		 (setf (subproofs sequent) nil)

		 (setf (whither sequent) nil)

		 (setf (involved sequent)

		   (append (used-goals init) (used-hyps init)))

		 ;; check for option.  If there's an option

		 ;; associated with the init then put its info into

		 ;; the plan.

		 (setf (plan sequent)

		   (or (init-plan init) 

		       `((done ,(externalize-f-cache (f-cache init)))))))

	     (init-subs *done*))

       (bf-condense1 *proof*)))



;;; This doesn't handle the case when *proof* is done by itself.



(defun bf-condense1 (proof)

  ;; This fills in all of the INVOLVED slots in the tree.

  (declare (type proof proof))

    (and (setf (involved proof)

	   (delete-duplicates

	    (mapcan #'(lambda (pf) (declare (type proof pf))

			;; On a branch, I could just check those

			;; subproofs in need-to-close-down.  Benji, I

			;; could speed this up by checking if there is

			;; a (sequent (init-sub *done*)) below the

			;; current position.  If not, then there is no

			;; need to look deeper.

			(if (involved pf) (ancestors (involved pf) pf)

			  (ancestors (bf-condense1 pf) pf)))

		    (subproofs proof))))

	 (if (cdr (subproofs proof))

	     (and (not (and (involved (car (subproofs proof)))

			    (involved (cadr (subproofs proof)))))

		  ;; (not (member proof (need-to-close-up *done*))) might

		  ;; work too.  *done* finishes more than one iff it finishes

		  ;; all.  Only one subproof of PROOF needs to remain.  PROOF

		  ;; can be extracted because if only one subproof remains

		  ;; then the step taken at PROOF was unneeded.  But does

		  ;; this mess up new-hyps, new-goals, birth-place, location,

		  ;; etc.?  As long as birth-place is not a copy- of the

		  ;; location then it should be ok.

		  (let ((sup (superproof proof)))

		    (and sup

			 (collapse

			  proof

			  (car (member-if #'(lambda (sub) (involved sub))

					  (subproofs proof))) sup))))

	   (when (subsetp (involved (car (subproofs proof))) (involved proof))

	     (let ((sup (superproof proof)))

	       (and sup (collapse proof (car (subproofs proof)) sup))))))

    (involved proof))



(defun collapse (proof new sup)

  ;; ONE-OF is T if NEW had siblings.

  ;; Cut proof out and put new in its place.

  (decf (depth new))

  (setf (subproofs sup) (nsubstitute new proof (subproofs sup))

	(index new) (index proof)

	(new-hyps new) (new-hyps proof)

	(new-goals new) (new-goals proof)

	(superproof new) sup)

  (mapc #'(lambda (f) (setf (birth-place f) new)) (new-hyps new))

  (mapc #'(lambda (f) (setf (birth-place f) new)) (new-goals new))

  (mapc #'(lambda (p) (map-proof #'(lambda (p) (decf (depth p))) p))

	(subproofs new)))



