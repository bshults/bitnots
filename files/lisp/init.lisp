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

;;; This file contains the initialization of global variables.  But
;;; only those that can be initialized without a structure definition.

;;; I want to move almost everything out of here into apropriate places

#-allegro
(xp::install :macro t :package :ipr)

(defvar *theory* t)

(defconstant *allow-interruptions* t) ; make this NIL to speed it up,
				; but T if you want to interrupt the
				; prover during QED-type problems.

;;; These should be made constants when you want to compile for production.

(defconstant *rigid-only* t)		; correct for T
;; universals are not implemented anymore.

(defconstant *renaming-universals* nil) ; correct for NIL
;; universals are not implemented anymore.

(defconstant *tableaux* t)	; T is better

(defconstant *universals* nil)
;; universals are not implemented anymore.

;; high-splitting seems to be causing problems in and-split
(defvar *high-splitting* nil)
(defvar *equality-fetcher* nil)	; use bad=goals in fetching process.

(defvar *alphas* nil "list of alpha formulas")
(defvar *betas* nil "list of beta formulas")
(defvar *deltas* nil "list of delta formulas")
(defvar *gammas* nil "list of gamma formulas")
;;;(defvar *sets* nil "list of an-element formulas")
(defvar *thes* nil "list of set formulas")

(defvar *comprehensions* nil
  "List of formulas to which comprehension applies")

(defvar *member-targets* nil
  "List of membership formulas that are targets for the restricted
paramodulation rule.")

(defvar *rp-productions* nil
  "List of equalities that can be used in the restricted paramodulation rule.
I.e., equalities with a class term on one side.")

(defvar *neg-extensionality-targets* nil
  "List of possibilities for application of the restricted
extensionality rule.  These only make a 2-way branch and don't introduce
free variables.")

(defvar *pos-extensionality-targets* nil
  "List of possibilities for application of the restricted extensionality
rule.  Positive formulas will introduce new variables.")

(defvar *watch* nil)
(defvar *approve-math* nil)
(defvar *interact* nil)

(defvar *hol* t
  "Are we using the `higher-order' classifier notation?")

(defvar *fol* t
  "If this is non-NIL then we are not in propositional logic.")

(defvar *look-for-theorems-now* nil)

(defvar *new-equals* nil "This is used by Brown's Rule.")

(defvar *equality* t)

(defvar *browns-rule* t)

(defvar *keep-origs-after-brown* nil) ; If this is nil, then we
				; mark formulas used if brown's rule
				; has been applied to them.  If this
				; is T, the prover is much slower.

(defvar *default-view* 7)
(defvar *send-messages* t)	; tell-emacs will show results if t
(defvar *interacting* t)	; 
(defvar *use-math* t)
(defvar *simp-fetcher* nil)
(defvar *fol-lemmas* nil)	; should the steps programs use lemmas
				; i.e. the beta-rules.
				; it seems much stupider with lemmas
(defvar *condense* t)
(defvar *out-of-context* nil)

;; NOTE: how-to-unify is never used!
;;;(defvar *how-to-unify* :bt)	; :bt means breadth first.
				; :one means find only one but don't apply it.
				; :apply means apply the first immediately.
				; nil means don't unify.

;;; plans
(defvar *ipr-batch-mode* nil)	; prove theorems as they are loaded
				; using scripts

(defvar *show-calls* 0)		; set this to NIL for production or speed

(defvar *rank-min* 0)
(defvar *leaf-queue* nil)
(defvar *defined-predicates* nil)
(defvar *defined-terms* nil)
(defvar *nothing-to-do* nil)
(defvar *viewing-proof* nil)
(defvar *unbound-vars* nil)
(defvar *time* 0)
(defvar *interactions* 0)	; number of times interacted
(defvar *involved-terms* nil)
(defvar *involved-predicates* nil)
(defvar *unknown-predicates* nil) ; think about keeping track of parity.
(defvar *unknown-terms* nil)	; should contain only terms in the theorem
(defvar *undefined-predicates* nil) ; should contain only preds in the theorem
(defvar *undefined-terms* nil)	; should contain only terms in the theorem
(defvar *dead-proofs* nil)	; a list of locations killed by condense
(defvar *open* t)		; does the user generally want to
				; see only unfinished proofs
;;;(defvar *grammars* (clrhash *grammars*))
(defvar *copy-max* 0)		; global copy-max for halting
				; %@$ start this at 0.
(defvar *view* 7)		; :command argument to send-proof
				; output level
(defvar *same-theorem-max* nil)	; Alist of <theorem . number> pairs.
				; theorem may not be applied more than
				; number times on a single branch.

;; (+ (/ 200 (n+1)) 90) is the perfect score for an option with n bad
;; juncts.
(defconstant *top-score-for*
  (make-array 5 :element-type '(integer 0 *) :initial-contents
	      '(10000 190 (+ (/ 200 3) 90) 140 130)))

(defmacro top-score-for (n)
  `(svref *top-score-for* ,n))

(defvar *options* nil)		; options of the previous run.
(defvar *t-init-alist* nil
  "An alist of pairs < theorem . t-junct-alist > where t-junct-alist
is an alist of pairs < thm-junct . t-init-list >.")

(defvar *merge-t-init-alist* nil
  "This contains the new and old inits of the new theorem juncts
triggered by new inits.")

(defvar *rel-t-init-alist* nil
  "This contains the old inits of the new theorem juncts triggered by
  new inits.")

(defvar *new-t-init-alist* nil
  "An alist of pairs < theorem . t-junct-alist > where t-junct-alist
is an alist of pairs < thm-junct . t-init-list >.")
(defvar *initialize-new-t-init-alist* nil
  "An alist of pairs < theorem . t-junct-alist > where t-junct-alist
is an alist of pairs < thm-junct . t-init-list >.")

(defvar *juncts-max* 4)

(defvar *theorem-max* 2) ;
'''(defvar *theory-grammar* nil) ; grammar where definitions are loaded
(defvar *theory-equalities* nil) ; definitional equalities
(defvar *equality-used* nil)	; was the equality reasoner used to
				; close the branch?

;;; info for user
(defvar *timer* 0)
(defvar *involved-only* t)	; t if you only want to see the juncts
				; involved in a closed branch.

;;; sequents
(defvar *transform-proof* nil)	; the proof for breaking down theorems
(defvar *high-proof* nil)	; the sequent which is the highest in which the
				; "working" formula occurs.  This is
				; nice to have around for the reducer.
(defvar *low-proof* nil)	; the current bottom-level sequent.
;; *unifying-proof* is usually a subproof of *working-proof*
;;  it is the proof we are trying to close.

;;; KB

(defvar *format-only* nil)	; Load only the strings of predicate and
				; term definitions
(defvar *term-formats* nil)	; alist of <term . string> 
(defvar *pred-formats* nil)	; alist of <pred . string> 

(defvar *kb-terms* nil)
(defvar *kb-predicates* nil)
(defvar *kb-theorems* nil)

(defvar *theorems* nil)		; list of theorem names
(defvar *kb-setup* nil)		; has the taxonomy and rule set been installed?

(defvar *the-undefined-thing* '(the-empty-set))

;;; When an equality is added to the hyps the grammar could be
;;; collapsed immediately.  Also, pass prammars down.  Also keep the
;;; theory-grammar current and update the sequent grammars as needed.
;;; Maybe one way to do this is to keep a list of equations.  Perhaps
;;; the theory-grammar should just be a list of equations.
;;;(defun get-grammar (proof)
;;;  (if proof			; this could be combined with theory-grammar
;;;				; to make it sensitive to paths and what has
;;;				; been defined where, if needed.
;;;				; and also include newly-defined terms.
;;;      (or (grammar proof)
;;;	  (get-grammar (superproof proof)))
;;;    (theory-grammar)))

;;;(defvar *working-grammar* nil)	; be careful with this
(defvar *replacement-made* nil)	; used by safe-replace functions

(defvar *unifying-proof* nil)	; so that i don't have to pass proof
				; everywhere in the equality reasoner.
;; the above could be avoided if we just have one grammar existing
;; at any time.  we would have to be careful that the nonterminals
;; are properly reset or something.  so we could have a single grammar,
;; a single symbol-cache, etc.
;; this could be accomplished if i just keep track of all nonterminal
;; symbols.  when i destroy a grammar, i also reset all non-terminal symbols.

;;; The function one-step is never called (even in interface).
;;; Therefore, this is not needed.
;;;(defvar *default-cat-list* '(:a :d :set-theory :b :c :math))
;;;(defvar *cat-list* nil)

;;; =s is not used anywhere
;;; (defvar *=s* nil "list of equality formulas")
;;; now-gammas was set but never used so wasting time.
;;; Appeared to have something to do with Brown's rule.
;;; (defvar *now-gammas* nil)

