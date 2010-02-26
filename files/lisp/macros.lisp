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

;;; This file contains macro definitions.

(deftype term () '(or symbol cons))

(deftype boolean () '(member t nil))

(defstruct (proof
	    (:print-function
	     (lambda (proof stream print-level)
	       (declare (ignore print-level))
	       (format stream "
Depth:  ~s Path: ~s  
Successes:
~s
Completely unified: ~s
Rank: ~s
Trivial-closures:
~s
It has ~s~:* subproof~P under it.
Its plan is
~s
"
		       (depth proof) (path proof) (successes proof)
		       (completely-unified proof) (rank proof)
		       (trivial-closures proof)
		       (length (subproofs proof)) (plan proof)
		       )))
	    (:conc-name nil))
  ;; SUCCESSES: list of init-subs which work here.  See also
  ;; completely-unified.
  (successes nil :type list)
  (theorem-string "" :type string)
  (theorem-name nil :type symbol)
  (theorem-successes nil :type list) ; list of successes from theorem
				; applications.
  (completely-unified nil :type boolean) ; NIL until all init subs have
				; been found.
;;;  (ups 0 :type (integer 0 *)) ; how many unifs have this as an "up".
  (rank 0 :type number) ; This is a measure of how important it is to
				; keep working here.
  ;; The grammar is just a list of productions.
;;;  grammar                    ; I was thinking that there would be
				; more than one grammar if there were
				; incompatible bindings on the
				; equalities.  Now, I think I am
				; handling that with conditions on the
				; productions.
  ;; Benji, change this to default to 0.
  (index nil :type (or null (integer 1 *)))
				; Which subproof to take at each branching.
  (depth 1 :type (integer 1 *)) ; Depth of the sequent.  This is the
                                ; length of what used to be the location.
  (trivial-closures nil :type list) ; list of trivial inits here
				; This may be gotten rid of.
  (plan nil :type list)		; where did i go from here?
;;;  (script nil :type list)	; user command (used?)
  split				; This contains the split structure
				; pointing to this node.  If a node is
				; spliced in between this node and its
				; split, then this is set to nil and
				; the newly spliced node gets the split.
  (whither nil :type symbol)	; what rule was applied to get the
				; subproofs of this? (used?)
  (involved nil :type list)	; list of involved juncts.
;;;  (used-juncts nil :type list)	; list of juncts which are used on this
				; branch but not necessarily used in general.
  (new-goals nil :type list)	; list of new goals
  (new-hyps nil :type list)	; list of new hypotheses
  (subproofs nil :type list)	; list of subproofs in order
				; corresponding to the order in the
				; emacs buffer.
  (superproof nil)		; proof of which this is a subproof
				; used to keep theorems from being permanent
;;;  (applicable-theorems nil :type list) ; list of theorem names which apply
;;;  (theorem-counter 0 :type number) ; tells which theorem in theorems-to-apply
				; to try next.
  (new-hyps-above nil :type list) ; If options is non-nil, then this will be
				; a list of hyps which are new and
				; above the current node.  I think
				; this is used by the function that
				; finds new options.
  (new-goals-above nil :type list) ; If options is non-nil, then this will be
				; a list of goals which are new and
				; above the current node.
  (options nil :type list)	; list of theorems which may be usefully
				; applied here and for which this is
				; the highest node at which it may be
				; applied.
  (t-init-alist nil :type list)	; The t-init-list up to here.
  (applications-to-branch nil :type list) ; list of math techniques tried here.
				; alist of ( theorem . ( option ... ) )
  )

;;; What's happenning is that in the high sequents where most things
;;; are used lower down, we are getting only the unused (i.e. gamma)
;;; formulas showing.  So, I think the goals-list is needed so that we
;;; can keep track of what was used above the current node.

;;;(defmacro un-if (test form1 &optional form2)
;;;  (if form2
;;;      `(if ,test ,form1 ,form2)
;;;      `(if ,test ,form1 :fail)))

(defmacro return-if (arg)
  (if arg `(return ,arg) arg))

(defmacro time-it (&rest forms)
  `(prog2 (setq *timer* (get-internal-run-time))
       (progn ,@forms)
     (setq *time* (float (+ *time* (/ (- (get-internal-run-time) *timer*)
				      internal-time-units-per-second))))))

(defun ipr-error (string &rest args)
  (throw 'error
    (let ((str (format nil "~?" string args)))
      (emacs-eval `(progn (beep) (message ,(quote-string str))
			  (sleep-for 3))))))

(defvar *warnings* nil)

(defun warn-user (string &rest args)
  ;; returns nil
  ;; this should immediately put a warning into a window
  ;; benji %@#$ if the window is visible then don't overwrite!
  ;; rather append.
  (let ((warning (format nil "~?" string args)))
    (pushnew warning *warnings* :test 'string=)
    (emacs-eval `(ipr-warn ,(quote-string warning)))
    nil))

(defmacro intersects (list1 list2 &key (test 'eql))
  `(some #'(lambda (e1) (member e1 ,list2 :test ,test)) ,list1))

(defmacro hash-table-macro (symbol)
  (let ((hash-table-var (create-name symbol 'hash 'table)))
    `(eval-when (load eval compile)
       (defvar ,hash-table-var (make-hash-table))
       (defmacro ,symbol (x)
	 `(gethash ,x ,',hash-table-var))
       (defun ,(create-name 'clear 'all symbol) ()
	 (clrhash ,hash-table-var)))))

(defmacro equal-hash-table-macro (symbol)
  (let ((hash-table-var (create-name symbol 'hash 'table)))
    `(eval-when (load eval compile)
       (defvar ,hash-table-var (make-hash-table :test #'equal))
       (defmacro ,symbol (x)
	 `(gethash ,x ,',hash-table-var))
       (defun ,(create-name 'clear 'all symbol) ()
	 (clrhash ,hash-table-var)))))

(defmacro property-macro (symbol)
  `(defmacro ,symbol (x)
     (list 'get x '',symbol)))

;;; This should be used when dealing with what might otherwise be a
;;; large structure.  The value of the name of the object is a
;;; hashtable.  This defines keys in the object.

(defmacro hash-key-macro (symbol)
  `(defmacro ,symbol (x)
     (list 'gethash '',symbol x)))

