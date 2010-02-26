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

;;; This file implements Brown's rule for equality.

(defun new-equality-formula ()
  ;; Go through *new-equals* and see if any of them are of the right
  ;; form.  If so then make the replacement in each formula.
  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))
			(format t "~%=") (format t "=")))
  (mapc
   #'(lambda (=h &aux (= (formula =h)))
       ;; Make sure they're not equal.
       (and (not (and (term-equal (cadr =) (caddr =))
		      ;; Remove the equality--really, we should
		      ;; probably undo the rule which created it.
		      (return-from new-equality-formula
			(undo-rule =h (birth-place =h)))))
	    (or (and (consp (cadr =))
		     (member (caadr =) *skolem-functions*)
		     (not (term-occurs-in-term (cadr =) (caddr =)))
		     (splicing-browns-rule =h t))
		(and (consp (caddr =))
		     (member (caaddr =) *skolem-functions*)
		     (not (term-occurs-in-term (caddr =) (cadr =)))
		     (splicing-browns-rule =h nil)))))
   *new-equals*))

(defun undo-rule (formula branch)
  ;; I want this to undo the application of the rule which created
  ;; this formula, at least on BRANCH.
  )

;;; Problem, applying theorems can give bindings to args of skolem
;;; functions that only want to apply to formulas involved there.  If
;;; I apply Brown's rule then the effect of applying that theorem
;;; widens to all formulas containing the skolem term.  This makes me
;;; want to consider substitutions which close other branches.

;;; Make sure bindings involve only sequent variables.

(defun splicing-browns-rule (=h order &aux (proof (birth-place =h))
				(skolem-term (if order (cadr (formula =h))
					       (caddr (formula =h))))
				(new-term (if order (caddr (formula =h))
					    (cadr (formula =h))))
				o-hyps o-goals)
  ;; This should make one copy of each formula, change the formula
  ;; slot of the copy appropriately and add the new formula.
  ;; 
  ;; First find all formulas (1) whose birth-places are above this node
  ;; and (2) contain the skolem term, make the replaced copies of them
  ;; and splice them under PROOF.  Then map-proof making splices as
  ;; you go to all formulas under here.
  (do ((p (superproof proof) (superproof p))
       (c-hs (mapcan
	      #'(lambda (h &aux (form (formula h))
			   (bindings
			    ;; instead of just checking whether it has
			    ;; been used anywhere, I need to see if it
			    ;; has been used in a place non-orthogonal
			    ;; to the birth place of =h.  So see if
			    ;; proof is in-tree of any of the places
			    ;; in the used slot.
			    (if (or (some #'(lambda (p)
					      (or (in-tree proof p)
						  (in-tree p proof))) (used h))
				    (eq h =h))
				:fail (compatible-substitutions
				       (bindings =h) (bindings h))))
			   ;; Make sure that the bindings of =h are
			   ;; compatible with those of H.
			   (paths (and (listp bindings)
				       (paths-to-occurrences-of-car
					(car skolem-term) form))))
		  ;; If so, then make a new formula.
		  (and paths (push h o-hyps)
		       (list (make-formula-from-old
			      (dolist (path paths form)
				(setq form
				  (substitute-at-path form path new-term)))
			      h :bindings bindings :parents-of (list h =h)
			      :copies (copies h) :splits
			      (delete-duplicates
			       (append (splits h) (copy-list (splits =h))))))))
	      (new-hyps proof))
	     (append
	      c-hs
	      (mapcan
	       #'(lambda (h &aux (form (formula h))
			    (bindings
			     (if (or (some #'(lambda (p)
					       (or (in-tree proof p)
						   (in-tree p proof)))
					   (used h))
				     (eq h =h))
				 :fail (compatible-substitutions
					(bindings =h) (bindings h))))
			    ;; Make sure that the bindings of =h are
			    ;; compatible with those of H.
			    (paths (and (listp bindings)
					(paths-to-occurrences-of-car
					 (car skolem-term) form))))
		   ;; If so, then make a new formula.
		   (and paths (push h o-goals)
			(list (make-formula-from-old
			       (dolist (path paths form)
				 (setq form
				   (substitute-at-path form path new-term)))
			       h :bindings bindings :parents-of (list h =h)
			       :copies (copies h) :splits
			       (delete-duplicates
				(append (splits h)
					(copy-list (splits =h))))))))
	       (new-hyps p))))
       (c-gs (mapcan
	      #'(lambda (g &aux (form (formula g))
			   (bindings
			    (if (or (some #'(lambda (p)
					      (or (in-tree proof p)
						  (in-tree p proof))) (used g))
				    (eq g =h))
				:fail (compatible-substitutions
				       (bindings =h) (bindings g))))
			   ;; Make sure that the bindings of =h are
			   ;; compatible with those of G.
			   (paths (and (listp bindings)
				       (paths-to-occurrences-of-car
					(car skolem-term) form))))
		  ;; If so, then make a new formula.
		  (and paths (push g o-hyps)
		       (list (make-formula-from-old
			      (dolist (path paths form)
				(setq form
				  (substitute-at-path form path new-term)))
			      g :bindings bindings :parents-of (list g =h)
			      :copies (copies g) :splits
			      (delete-duplicates
			       (append (splits g) (copy-list (splits =h))))))))
	      (new-goals proof))
	     (append
	      c-gs
	      (mapcan
	       #'(lambda (g &aux (form (formula g))
			    (bindings
			     (if (or (some #'(lambda (p)
					       (or (in-tree proof p)
						   (in-tree p proof)))
					   (used g))
				     (eq g =h))
				 :fail (compatible-substitutions
					(bindings =h) (bindings g))))
			    ;; Make sure that the bindings of =h are
			    ;; compatible with those of G.
			    (paths (and (listp bindings)
					(paths-to-occurrences-of-car
					 (car skolem-term) form))))
		   ;; If so, then make a new formula.
		   (and paths (push g o-goals)
			(list (make-formula-from-old
			       (dolist (path paths form)
				 (setq form
				   (substitute-at-path form path new-term)))
			       g :bindings bindings :parents-of (list g =h)
			       :copies (copies g) :splits
			       (delete-duplicates
				(append (splits g)
					(copy-list (splits =h))))))))
       (new-goals p)))))
      ((null p)
       ;; At this point, all unused formulas born above PROOF which
       ;; contain the skolem term have been copied with the
       ;; replacement made.  Now we want to splice a new proof here
       ;; with those copies in it.  After that we will go down and
       ;; splice many a new proof in for each formula born under here
       ;; containing the skolem term.
       (if (or c-gs c-hs)
	   (let ((sub (make-sequent
		       :new-hyps c-hs :new-goals c-gs :superproof proof
		       :depth (1+ (depth proof)) :plan
		       (plan proof) :whither (whither proof) :subproofs
		       (subproofs proof))))
	     (labels ((bp-fn (f) (setf (birth-place f) sub)))
	       (mapc #'bp-fn c-hs)
	       (mapc #'bp-fn c-gs))
	     (and (not *keep-origs-after-brown*)
		  (labels ((op-fn (f) (push sub (used f))))
		    (op-fn =h)
		    (mapc #'op-fn o-hyps)
		    (mapc #'op-fn o-goals)))
	     (setf (plan proof)
	       `((browns-rule ,(externalize-formula (formula =h)))))
	     (set-splits proof sub)
	     (setf (whither proof) :a)
	     (setf (subproofs proof) (list sub))
	     (when *new-alpha* (reduce-formula sub) (setq *new-alpha* nil))
	     (let ((g-reps (mapcan #'(lambda (f)
				       (or (copy-list (replacements f))
					   (list f))) c-gs))
		   (h-reps (mapcan #'(lambda (f)
				       (or (copy-list (replacements f))
					   (list f))) c-hs)))
	       (mapc #'(lambda (pr)
			 (setf (superproof pr) sub)
			 (map-proof
			  ;; If reducer has made replacements, then put
			  ;; the replacements into juncts-above instead
			  ;; of new-formulas.  Don't forget to (INCF
			  ;; (DEPTH P)).
			  #'(lambda (p)
			      (when (options p)
				(setf (new-goals-above p)
				  (append (new-goals-above p) g-reps))
				(setf (new-hyps-above p)
				  (append (new-hyps-above p) h-reps)))
			      (incf (depth p))) pr)
			 (simple-brown pr =h skolem-term new-term))
		     (subproofs sub))))
	 ;; Brown's rule has not been applied yet below here.  See if
	 ;; it applies lower down.
	 (mapc #'(lambda (pr) (simple-brown pr =h skolem-term new-term))
	       (subproofs proof)))))
  t)

;;; This is applied too often as it is.  This needs to control where it
;;; is called next.
(defun simple-brown (proof =h skolem-term new-term)
  (let* ((o-hyps nil)
	 (o-goals nil)
	 ;; C-HS: unused hyps with compatible bindings and containing
	 ;; skolem-term.
	 (c-hs (mapcan #'(lambda (h &aux (form (formula h))
				    (bindings
				     (if (used h) :fail
				       (compatible-substitutions
					(bindings =h) (bindings h))))
				    ;; Make sure that the bindings of =h
				    ;; are compatible with those of H.
				    (paths (and (listp bindings)
						(paths-to-occurrences-of-car
						 (car skolem-term) form))))
			   ;; If so, then make a new formula.
			   (and paths
				(push h o-hyps)
				(list (make-formula-from-old
				       (dolist (path paths form)
					 (setq form (substitute-at-path
						     form path new-term)))
				       h :bindings bindings :parents-of
				       (list h =h) :copies (copies h) :splits
				       (delete-duplicates
					(append (splits h)
						(copy-list (splits =h))))))))
		       (new-hyps proof)))
	 (c-gs (mapcan #'(lambda (g &aux (form (formula g))
				    (bindings
				     (if (used g) :fail
				       (compatible-substitutions
					(bindings =h) (bindings g))))
				    ;; Make sure that the bindings of =h
				    ;; are compatible with those of G.
				    (paths (and (listp bindings)
						(paths-to-occurrences-of-car
						 (car skolem-term) form))))
			   ;; If so, then make a new formula.
			   (and paths
				(push g o-goals)
				(list (make-formula-from-old
				       (dolist (path paths form)
					 (setq form (substitute-at-path
						     form path new-term)))
				       g :bindings bindings :parents-of
				       (list g =h) :copies (copies g) :splits
				       (delete-duplicates
					(append (splits g)
						(copy-list (splits =h))))))))
		       (new-goals proof))))
    ;; At this point, all unused formulas born at PROOF which contain
    ;; the skolem term have been copied with the replacement made.
    ;; Now we want to splice a new proof here with those copies in it.
    (if (or c-gs c-hs)
	(let ((sub (make-sequent
		    :new-hyps c-hs :new-goals c-gs :superproof proof
		    :depth (1+ (depth proof)) :plan
		    (plan proof) :whither (whither proof) :subproofs
		    (subproofs proof))))
	  (labels ((bp-fn (f) (setf (birth-place f) sub)))
	    (mapc #'bp-fn c-hs)
	    (mapc #'bp-fn c-gs))
	  (and (not *keep-origs-after-brown*)
	       (labels ((op-fn (f) (push sub (used f))))
		 (op-fn =h)
		 (mapc #'op-fn o-hyps)
		 (mapc #'op-fn o-goals)))
	  (setf (plan proof)
	    `((browns-rule ,(externalize-formula (formula =h)))))
	  (set-splits proof sub)
	  (setf (whither proof) :a)
	  (when *new-alpha* (reduce-formula sub) (setq *new-alpha* nil))
	  (let ((g-reps (mapcan #'(lambda (f)
				    (or (copy-list (replacements f)) (list f)))
				c-gs))
		(h-reps (mapcan #'(lambda (f)
				    (or (copy-list (replacements f)) (list f)))
				c-hs)))
	    (mapc #'(lambda (pr)
		      (setf (superproof pr) sub)
		      (map-proof
		       ;; If reducer has made replacements, then put
		       ;; the replacements into juncts-above instead
		       ;; of new-formulas.  Don't forget to (INCF
		       ;; (DEPTH P)).
		       #'(lambda (p) (when (options p)
				       (setf (new-goals-above p)
					 (append (new-goals-above p) g-reps))
				       (setf (new-hyps-above p)
					 (append (new-hyps-above p) h-reps)))
			   (incf (depth p))) pr)
		      (simple-brown pr =h skolem-term new-term))
		  (subproofs sub)))
	  (setf (subproofs proof) (list sub)))
      (mapc #'(lambda (pr) (simple-brown pr =h skolem-term new-term))
	    (subproofs proof)))))

