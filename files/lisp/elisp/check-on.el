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

;;; bind C-x\k to something which will take care of the
;; binding of *check-on-buff*

;; think about writing a pretty-printing program.
;; when autofill wants to insert a line, this function would
;; be called.  It would go back to the earliest possible
;; parenthesis on that line and try to indent to it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ipr-Chk Mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ipr-chk-mode)

(defvar ipr-chk-mode-map nil)
(setq ipr-chk-mode-map nil)
;;;(defvar *chk-prefix* "\C-z")
(defvar *chk-previous-window* nil)
(defvar *chk-previous-buffer* nil)
(defvar NIL nil)
(defvar *chk-buff-name* "check-on")
(defvar *ipr-proof-directory* "/u/bshults/work/ipr/examples/proofs/fol=set/")
				; directory containing saved proofs
(defvar *chk-aux-buff-name* "chk-aux")
(defvar *chk-buff* nil)
(defvar *chk-aux-buff* nil)
(defvar *ipr-open* t)
(defvar *ipr-condense* t)
(defvar *ipr-fol-lemmas* nil)
(defvar *ipr-keep-repeats* nil)
(defvar *ipr-view* 7)
(defvar *ipr-location-here* '(1))
;;;(defvar *plan* nil)

(setq max-specpdl-size 1000)
(setq max-lisp-eval-depth 800)

(defun setup-chk-mode-map ()	;prefix
  (let ((l (list
	    'narrow-to-page 'narrow-to-region 'recover-file
	    'rename-buffer 'toggle-read-only 'yank 'yank-pop 'zap-to-char
	    )))
    (while (car l)
      (let ((k (where-is-internal (car l))))
	(while (car k)
	  (define-key ipr-chk-mode-map (car k) 'chk-not-implemented)
	  (setq k (cdr k)))
	(setq l (cdr l)))))
  (define-key ipr-chk-mode-map "f" 'chk-into-proof)
  (define-key ipr-chk-mode-map "b" 'chk-out-of-proof)
  (define-key ipr-chk-mode-map "n" 'chk-next-proof)
  (define-key ipr-chk-mode-map "p" 'chk-previous-leaf)
  (define-key ipr-chk-mode-map "n" 'chk-next-leaf)
  (define-key ipr-chk-mode-map "b" 'chk-higher-sequent)
  (define-key ipr-chk-mode-map "f" 'chk-first-subproof)
  (define-key ipr-chk-mode-map "N" 'chk-next-sibling)
  (define-key ipr-chk-mode-map "P" 'chk-previous-sibling)
  (define-key ipr-chk-mode-map "p" 'chk-previous-proof)
  (define-key ipr-chk-mode-map "o" 'chk-output-options)
;;;  (define-key ipr-chk-mode-map "l" 'chk-toggle-fol-lemmas)
  (define-key ipr-chk-mode-map "I" 'chk-toggle-interact)
  (define-key ipr-chk-mode-map "W" 'chk-toggle-watch)
;;;  (define-key ipr-chk-mode-map "U" 'chk-toggle-apply-first)
  ;;  (define-key ipr-chk-mode-map "C-o" 'chk-output-statistics)
  (define-key ipr-chk-mode-map "" 'chk-output-proof)
  (define-key ipr-chk-mode-map "o" 'chk-other-options)
  (define-key ipr-chk-mode-map "u" 'chk-window-up)
  (define-key ipr-chk-mode-map "d" 'chk-window-down)
  (define-key ipr-chk-mode-map "" 'chk-save-buffer)
  (define-key ipr-chk-mode-map "a" 'chk-ask-ipr)
  (define-key ipr-chk-mode-map " " 'chk-proof-step)
  (define-key ipr-chk-mode-map "
" 'chk-proof-step-here)
  (define-key ipr-chk-mode-map "k" 'chk-kill-subproofs)
;;;  (define-key ipr-chk-mode-map "w" 'chk-watch*)
;;;  (define-key ipr-chk-mode-map "W" 'chk-watch)
  (define-key ipr-chk-mode-map "q" 'chk-qed*)
  (define-key ipr-chk-mode-map "Q" 'chk-qed)
  (define-key ipr-chk-mode-map "m" 'chk-math*)
  (define-key ipr-chk-mode-map "M" 'chk-math)
  (define-key ipr-chk-mode-map "t" 'chk-taut*)
  (define-key ipr-chk-mode-map "T" 'chk-taut)
  (define-key ipr-chk-mode-map "s" 'chk-set-prove*)
  (define-key ipr-chk-mode-map "S" 'chk-set-prove)
  (define-key ipr-chk-mode-map "i" 'chk-interrupt)
  (define-key ipr-chk-mode-map "c" 'chk-change-copy-max)
  (define-key ipr-chk-mode-map "C" 'chk-claim)
  (define-key ipr-chk-mode-map "R" 'chk-replace-=)
  (define-key ipr-chk-mode-map "