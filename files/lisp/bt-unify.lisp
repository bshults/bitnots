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



(proclaim '(inline combine check-dependencies compatible

                   close-down-if-kill-all-to-left compatible-substitutions))



(defun unify-and-sort-branches ()

  (bt-unify-tree)

  (rate-leaves)

  (setq *rank-min* (rank (car *leaf-queue*))))



(defun find-successes-and-sort-branches ()

  (pass-one *proof* nil nil)

  (rate-leaves)

  (setq *rank-min* (rank (car *leaf-queue*))))



(defun bt-unify-tree ()

  ;; Think about having this go down tree instead of looking only at

  ;; leaves.  Pass one finds all branch closures. (inits)

  ;; 

  ;; Go through the leaves of *proof*.  How about if the nearest

  ;; options slot is above a branch then search for 0-options here

  ;; (and 0-options only.)  Will I need to store them in a separate

  ;; slot?  No, finding only 0-options is too messy.  Find ALL options.

  ;; 

  ;; Be sure to make sure that SOME non=options have been found above

  ;; proof before applying this.

  ;; 

  ;; What about the new-juncts-above being used to find new theorems

  ;; multiple times?  It can happen!

  (and *kb-setup* ; *theory*

       (prog1 t (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))

                                      (format t "~%0")

                                    (format t "0"))))

       (let ((*show-calls* nil))

         (mapc #'(lambda (pf)

                   ;; If there are non=options found above PF

                   ;; somewhere and no options have been found between

                   ;; PF and the nearest branch, then search for

                   ;; options and apply 0-options.  Also, if there are

                   ;; options before you get too high and there are

                   ;; new-juncts-above there, then find the new

                   ;; options here as well.

                   (do ((p pf (superproof p))

                        (branch-passed

                         ;; instead of passing a branch, let's say if

                         ;; anything other than a theorem application

                         ;; has happenned then apply theorems again.

                         ;; Yikes!  That will take a long time!  For

                         ;; now let's just do this when there is a

                         ;; branch that was not caused by a theorem

                         ;; split.

                         nil (or branch-passed

                                 (and (index p)

                                      (not (eq (caar (plan (superproof p)))

                                               'apply-theorem-split))))))

                       ((null p))

                     (if branch-passed

                         (and (options p)

                              (if (eq *proof* p)

                                  (return nil)

                                (return (progn (find-best-options pf)

                                               (apply0options pf)))))

                       (if (options p)

                           (if (or (new-hyps-above p)

                                   (new-goals-above p))

                               (return (progn (find-best-options pf)

                                              (apply0options pf)))

                             (return nil))))))

               (leaves *proof*))))

  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))

                        (format t "~%U")

                      (format t "U")))

  (pass-one *proof* nil nil nil nil)

  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))

                        (format t "~%2")

                      (format t "2")))

  ;; Pass two finds all maximal unifications. (unifications)

  (when (pass-two *proof*)

    (bt-condense)

    *done*))



(defun pass-one (proof c-hyps c-goals n-hyps n-goals

                       &aux (new-hyps (remove-if #'used (new-hyps proof)))

                       (new-goals (remove-if #'used (new-goals proof))))

  ;; N-GOALS and N-HYPS contain the new formulas since the last

  ;; PASS-ONE top-level call.  INIT-SUBS are saved at the highest

  ;; sequent containing all of the juncts.  C-HYPS and C-GOALS are the

  ;; unused goals and hyps on the current branch from nodes which were

  ;; present the last time PASS-ONE was called on *proof*.

  (if (completely-unified proof)

      (pass-one-c-node proof c-hyps c-goals n-hyps n-goals new-hyps new-goals)

    (progn

      (setf (successes proof) (theorem-successes proof))

      ;; I wonder if theorem-successes is messed up by this.  I.e. if

      ;; it is not examined at the right time or something.

      (setf (completely-unified proof) t)

      (pass-one-u-node proof c-hyps c-goals n-hyps n-goals

                       new-hyps new-goals))))



(defun pass-one-c-node (proof c-hyps c-goals n-hyps n-goals new-hyps new-goals)

  ;; Unify new-goals with n-hyps

  (mapc

   #'(lambda (goal) (declare (type formula goal))

       (mapc

        #'(lambda (hyp)

            ;; (bindings formula) contains a substitution on which the

            ;; existence of FORMULA depends.  This guarantees that any

            ;; UNIFICATION which usees FORMULA must be compatible with

            ;; the BINDINGS of FORMULA.

            (let* ((bindings (compatible-substitutions

                              (bindings goal) (bindings hyp)))

                   (f-c (if (listp bindings)

                            (if (and *hol*

                                     (eq 'an-element (car (formula goal)))

                                     (eq 'a-member-of (car (formula hyp))))

                                (unify-terms (cadr (formula goal))

                                             (cadr (formula hyp))

                                             bindings)

                              (unify-formulas (formula goal)

                                              (formula hyp) bindings))

                          :fail)))

              (declare (type (or list symbol) f-c bindings))

              (when (listp f-c)

                (let ((init (make-init

                             :f-cache f-c :used-hyps (list hyp)

                             :sequent proof :used-goals (list goal))))

                  (declare (type init-sub init))

                  ;; Push INIT into the successes of the highest proof

                  ;; where it makes sense.

                  (push init (successes proof))

                  (when (null f-c)

                    (push init (trivial-closures proof)))))))

        n-hyps))

   new-goals)

  ;; Unify new-hyps with n-goals

  (mapc

   #'(lambda (hyp) (declare (type formula hyp))

       (mapc

        #'(lambda (goal)

            (let* ((bindings (compatible-substitutions

                              (bindings goal) (bindings hyp)))

                   (f-c (if (listp bindings)

                            (if (and *hol*

                                     (eq 'an-element (car (formula goal)))

                                     (eq 'a-member-of (car (formula hyp))))

                                (unify-terms (cadr (formula goal))

                                             (cadr (formula hyp))

                                             bindings)

                              (unify-formulas (formula goal) (formula hyp)

                                              bindings)) :fail)))

              (declare (type (or list symbol) f-c bindings))

              (when (listp f-c)

                (let ((init (make-init

                             :f-cache f-c :used-hyps (list hyp)

                             :sequent proof :used-goals (list goal))))

                  (declare (type init-sub init))

                  (push init (successes proof))

                  (when (null f-c) (push init (trivial-closures proof)))))))

        ;; I don't want the new hyps above because the new hyps will

        ;; have their chance to unify with all current goals here.

        n-goals))

   new-hyps)

  ;; Add new-goals to c-goals and new-hyps to c-hyps.

  (mapc #'(lambda (pr)

            (pass-one pr (append new-hyps c-hyps) (append new-goals c-goals)

                      n-hyps n-goals))

        (subproofs proof)))



(defun pass-one-u-node (proof c-hyps c-goals n-hyps n-goals new-hyps new-goals)

  ;; Unify new-goals with c-hyps and n-hyps

  (mapc

   #'(lambda (goal) (declare (type formula goal))

       ;; First try to unify the terms in an equality.

       (and (eq '= (car (formula goal)))

            (let ((f-c (unify-terms (cadr (formula goal))

                                    (caddr (formula goal)) nil)))

              (when (listp f-c)

                (let ((init (make-init :f-cache f-c :sequent proof :used-goals

                                       (list goal))))

                  (declare (type init-sub init))

                  ;; Push INIT into the successes of the highest proof

                  ;; where it makes sense.

                  (push init (successes proof))

                  (when (null f-c)

                    (push init (trivial-closures proof)))))))

       (mapc

        #'(lambda (hyp)

            ;; (bindings formula) contains a substitution on which the

            ;; existence of FORMULA depends.  This guarantees that any

            ;; UNIFICATION which usees FORMULA must be compatible with

            ;; the BINDINGS of FORMULA.

            (let* ((bindings (compatible-substitutions

                              (bindings goal) (bindings hyp)))

                   (f-c (if (listp bindings)

                            (if (and *hol*

                                     (eq 'an-element (car (formula goal)))

                                     (eq 'a-member-of (car (formula hyp))))

                                (unify-terms (cadr (formula goal))

                                             (cadr (formula hyp))

                                             bindings)

                              (unify-formulas (formula goal) (formula hyp)

                                              bindings))

                          :fail)))

              (declare (type (or list symbol) f-c bindings))

              (when (listp f-c)

                (let ((init (make-init

                             :f-cache f-c :used-hyps (list hyp)

                             :sequent proof :used-goals (list goal))))

                  (declare (type init-sub init))

                  ;; Push INIT into the successes of the highest

                  ;; proof where it makes sense.

                  (push init (successes proof))

                  (when (null f-c)

                  (push init (trivial-closures proof)))))))

        (append n-hyps c-hyps)))

   new-goals)

  ;; Unify new-hyps with c-goals and new-goals and n-goals

  (mapc

   #'(lambda (hyp) (declare (type formula hyp))

       (mapc

        #'(lambda (goal)

            (let* ((bindings (compatible-substitutions

                              (bindings goal) (bindings hyp)))

                   (f-c (if (listp bindings)

                            (if (and *hol*

                                     (eq 'an-element (car (formula goal)))

                                     (eq 'a-member-of (car (formula hyp))))

                                (unify-terms (cadr (formula goal))

                                             (cadr (formula hyp))

                                             bindings)

                              (unify-formulas (formula goal) (formula hyp)

                                              bindings)) :fail)))

              (declare (type (or list symbol) f-c bindings))

              (when (listp f-c)

                (let ((init (make-init

                             :f-cache f-c :used-hyps (list hyp)

                             :sequent proof :used-goals (list goal))))

                  (declare (type init-sub init))

                  (push init (successes proof))

                  (when (null f-c)

                    (push init (trivial-closures proof)))))))

        ;; I don't want the new hyps above because the new hyps will

        ;; have their chance to unify with all current goals here.

        (append new-goals c-goals n-goals)))

   new-hyps)

  (mapc #'(lambda (pr)

            (pass-one pr c-hyps c-goals (append new-hyps n-hyps)

                      (append new-goals n-goals)))

        (subproofs proof)))



;;; If I had a whole lot more RAM, I would save failed unifications so

;;; that they wouldn't all have to be reconstructed.  I tried this

;;; before and it ate all my RAM (and more).



;;; This can assume that all inits are in place.

(defun pass-two (proof)

  ;; This goes through the tree from top to bottom, left to right.

  ;; For each init-sub, if the init-sub kills all branches to the left

  ;; of the current proof, then this makes a unification and tests to

  ;; see if it finishes the entire tree.  If not then this calls

  ;; unification-complete which will try to find an extension of

  ;; unification which finishes the entire tree.

  (declare (type proof proof))

  (do ((proofs (list proof)

               (append (subproofs (car proofs)) (cdr proofs))))

      ((null proofs) *done*)

    (declare (list proofs))

    (dolist (init (successes (car proofs)))

      (declare (type init-sub init))

      ;; For each init which kills all branches to its left, we make a

      ;; new unification.  This unification will not be changed much.

      ;; But it will be copied and each copy will include one more

      ;; init-sub.

      (let ((down (close-down-if-kill-all-to-left init (car proofs))))

        ;; What I need to know is whether any of the subproofs of the

        ;; init-splits are to the left of (car proofs).  If so then

        ;; the above returns :fail.  Otherwise, the above constructs

        ;; NEED-TO-CLOSE-DOWN.  NEED-TO-CLOSE-DOWN is sorted

        ;; top-to-bottom, left-to-right.

        (and (listp down)

             ;; If it does kill all branches to the left, then make a

             ;; unification.

             (let* ((u (make-unification

                        :need-to-close-up

                        ;; in-tree returns true if 1st is strictly

                        ;; less than 2nd.

                        (sort (copy-list (init-splits init))

                              #'in-tree :key #'split-node)

                        :full-subst (f-cache init) :init-subs (list init)

                        :closed (list (car proofs)) :need-to-close-down down))

                    ;; need-to-close-down will contain only those

                    ;; subproofs of elements of need-to-close-up which

                    ;; do not have some closed branch running through

                    ;; them.  NEXT is the next need-to-close-down to

                    ;; be examined.  (This is still going

                    ;; top-to-bottom, left-to-right.)

                    (next (dolist (n1 (copy-list (need-to-close-down u)))

                            (if (in-tree (car proofs) n1)

                                (pop (need-to-close-down u))

                              (return n1)))))

               (declare (type unification u))

               ;; If there is nothing else to be proved then we are

               ;; done.  Otherwise call unification-complete.

               (if (null next)

                   (return-from pass-two (setq *done* u))

                 (and (not (eq :fail (unification-complete u)))

                      (return-from pass-two *done*)))))))))



(defun unification-complete (unification)

  (declare (type unification unification))

  ;; Expand UNIFICATION in every possible way.  Here we examine only

  ;; the branches through the leftmost need-to-close-down.  Since

  ;; UNIFICATION is not to be changed, it does not need to examine any

  ;; proofs further to the right.  Its extensions will examine proofs

  ;; further to the right.

;;;  (and *show-calls* (if (zerop (mod (incf *show-calls*) 60))

;;;                         (format t "~%U") (format t "U")))

  (do ((proofs (list (car (need-to-close-down unification)))

               (append (subproofs (car proofs)) (cdr proofs))))

      ;; Unification absolutely does not close PROOF and never

      ;; will.  Otherwise, unification-complete would not have

      ;; been called.  Hopefully, one of its copies will finish

      ;; the entire tree.  If all branches through the leftmost

      ;; need-to-close-down are examined and none of them can be

      ;; used to extend unification in such a way as to finish the

      ;; tree, then fail.

      ((null proofs) :fail)

    (declare (list proofs))

    (dolist (init (successes (car proofs)))

      (let ((f-c (compatible init unification)))

        (declare (type (or symbol list) f-c))

        ;; If init is allowed to be added to unification then

        ;; compatible returns the combination of their

        ;; substitutions.  (Otherwise it returns :fail.)  If it is

        ;; successful, then we combine the two extending

        ;; unification with init.

        (and (listp f-c)

             (let* ((u (combine init (copy-unification unification) f-c))

                    ;; Recall that combine might add to

                    ;; need-to-close-down.

                    (next (dolist (n1 (copy-list (need-to-close-down u)))

                            (if (in-tree (car proofs) n1)

                                (pop (need-to-close-down u))

                              (return n1)))))

               ;; This assures that NEXT is not above (car proofs)

               ;; because such a need-to-close-down would be done.

               ;; This also sets need-to-close-down properly.  If

               ;; there is nothing in need-to-close-down (i.e. (null

               ;; next)) then check dependencies because the proof is

               ;; done.  If dependencies do not check, then some other

               ;; unification will finish the proof, I think.  So this

               ;; dependencies checking is not really needed.

               ;; unifications which are not so checked really do

               ;; finish the proof but they are not nice.  If a

               ;; unification finishes the proof then a nice one does.

               ;; This just makes me wait til I get to the nice one.

               (if (null next)

                   (and (dolist (p (copy-list (need-to-close-up u)) t)

                          ;; The copy-list is there because

                          ;; need-to-close-up is modified inside the

                          ;; loop.  This loop gets splits out of

                          ;; need-to-close-up which are known to be

                          ;; closed by U.  When a need-to-close-up is

                          ;; encountered which is not proved by U then

                          ;; this loop is broken returns NIL.

                          (and (not (and (check-dependencies u)

                                         (pop (need-to-close-up u))))

                               (return nil)))

                        (return-from unification-complete

                          (setq *done* u)))

                 ;; If next pops out from under some close-up then

                 ;; make sure that the close-up was really done.  If

                 ;; it is then continue to the next need-to-close-down

                 ;; by a recursive call.  On the last need-to-close-up

                 ;; it never check dependencies!

                 (and (dolist (p (copy-list (need-to-close-up u))

                                 (error "Please report a bug to~

 shults@cs.wcu.edu.~%Send the theorem you were proving."))

                        (if (in-tree next (split-node p))

                            (return t)

                          (and (not (and (check-dependencies u)

                                         (pop (need-to-close-up u))))

                               (return nil))))

                      (not (eq :fail (unification-complete u)))

                      (return-from unification-complete *done*)))))))))



;;; If check-dependencies fails then the current unification is

;;; invalid.  This checks that each branch through the sequent that is

;;; thought to be finished has an init-sub on it that depends on that

;;; sequent.

(defun check-dependencies (unification)

  (let* ((proof (split-node (car (need-to-close-up unification))))

         (subs (subproofs proof)))

    (do ((inits (init-subs unification) (cdr inits)))

        ((or (null inits) (null subs)) (null subs))

      (let ((s (some #'(lambda (sub)

                         (and (in-tree (sequent (car inits)) sub)

                              (member proof (init-splits (car inits))

                                      :key #'split-node)
                              sub))
                     subs)))

        (and s (setq subs (remove s subs)))))))



;;; When you are on the right side of a need-to-close-up, then SOME

;;; init on each branch must depend on that split.  So we need to

;;; notice when we are done with a need-to-close-up and check if this

;;; is the case.



(defun compatible (init unification &aux (proof (sequent init)))

  ;; INIT cannot be killed by everything in UNIFICATION because it

  ;; must be under something which UNIFICATION needs to close.

  ;; (Otherwise, we wouldn't be here.)  This also ensures that

  ;; everything to the left of (sequent INIT) is taken care of by INIT

  ;; or UNIFICATION.

  (declare (type init-sub init) (type unification unification))

  (if (dolist (n1 (init-splits init) t)

        ;; If N1 is in need-to-close-up then go on.

        (and (not (member n1 (need-to-close-up unification)))

             ;; Otherwise, those subproofs to the left must be

             ;; closed by UNIFICATION.  Makes sure it does not

             ;; have a subproof to the left of the current proof.

             ;; That would be bad because we are not going back

             ;; that way to close anything.

             (to-the-left-of (car (subproofs (split-node n1))) proof)

             ;; If something to the left is not closed then:

             (return nil)))

      ;; The below returns :fail if the substitutions are not

      ;; compatible.

      (compatible-substitutions (f-cache init) (full-subst unification))

    :fail))



(defun close-down-if-kill-all-to-left (init proof &aux down)

  ;; This returns the list of subproofs of the init-splits of INIT

  ;; unless some proof to the left of PROOF is not killed by INIT.  If

  ;; something is not killed then this returns :fail.  The list is

  ;; sorted top-to-bottom, left-to-right.  This basically assumes that

  ;; it is being called by PASS-TWO.  I.e. that PROOF is the SEQUENT

  ;; of INIT.

  (sort

   (dolist (n1 (init-splits init) down)

     (let ((need (subproofs (split-node n1))))

       (and (to-the-left-of (car need) proof)

            (return-from close-down-if-kill-all-to-left :fail))

       (setq down (append (cdr need) down))))

   #'(lambda (p1 p2) (before p1 p2))))



(defun combine (init unification f-c &aux

                     (down (copy-list (need-to-close-down unification))))

  ;; Be sure the old information in the list is retained.  This puts

  ;; init into the init-subs slot of unification and updates the slots

  ;; of unification.  Assumes F-C was created by COMPATIBLE.

  ;; COMPATIBLE must be called before this.

  (declare (type init-sub init)

           (type unification unification) (type (or symbol list) f-c))

  (push init (init-subs unification))   ; it is needed by condense

  (push (sequent init) (closed unification)) ; this is safe, I tested it

  (setf (full-subst unification) f-c)

  (mapc #'(lambda (n1)

            (and (not (member n1 (need-to-close-up unification)))

                 ;; this assumes that the need-to-close-up has not

                 ;; been reset yet.

                 (setq down

                   (merge 'list down

                           (copy-list (cdr (subproofs (split-node n1))))

                           #'(lambda (p1 p2) (before p1 p2))))))

          (init-splits init))

  (setf (need-to-close-down unification) down)

  (setf (need-to-close-up unification)

        (remove-duplicates

         (merge 'list (copy-list (need-to-close-up unification))

                 (copy-list (init-splits init))

                 #'(lambda (p1 p2) (or (in-tree p1 p2) (to-the-left-of p1 p2)))

                 :key #'split-node)))

  ;; The close-up list is sorted left to right, bottom to top.

  unification)



;;; This combines two idempotent substitutions into one idempotent

;;; substitution if possible.  If not then this returns :fail.  I

;;; think there is some clever way to do this but I don't care.  This

;;; is correct and it is quite tricky to make it correct so I say,

;;; leave it alone.  The first argument must be a KNOWN idempotent

;;; substitution.  If it is not then this is incorrect.  I think it is

;;; OK if the second argument is not idempotent but I'm not sure.

(defun compatible-substitutions (old new)

  (let ((*out-of-context* t))

    (let (new1 new2)

      (mapcar #'(lambda (p)

                  (push (car p) new1)

                  (push (cdr p) new2))

              new)

      (unify-terms (cons 'ignore new1)

                   (cons 'ignore new2)

                   old))))



;;; It would be nice if I could do this only on branches which have

;;; been altered since the last time I did this.  Or figure rank

;;; during unification.



;;; A big number means that the leaf does not need as much work.  A

;;; small number means that the leaf needs work.

(defun rate-leaves (&optional (proof *proof*))

  ;; proofs with many inits and short f-caches good.

  (let ((here (reduce #'+ (mapcar #'(lambda (i)

                                      (/ 1 (1+ (length (f-cache i)))))

                                  (successes proof)))))

    (and (subproofs proof)

         (mapc #'(lambda (p) (rate-sequent p here)) (subproofs proof)))

    (setq *leaf-queue* (sort (leaves *proof*) #'< :key #'rank))))



;;; The rank of a branch might also contain information about whether

;;; the lowest copies number of the gammas in that branch.



(defun rate-sequent (proof above)

  (let ((here (reduce #'+ (cons above

                                (mapcar

                                 #'(lambda (i) (/ 1 (1+ (length (f-cache i)))))

                                 (successes proof))))))

    (if (subproofs proof)

        (mapc #'(lambda (p) (rate-sequent p here)) (subproofs proof))

      (and (setf (rank proof) here)

           (zerop (rank proof))

           ;; This will make the deep ones go later.

           (setf (rank proof) (- (/ 1 (depth proof))))))))



