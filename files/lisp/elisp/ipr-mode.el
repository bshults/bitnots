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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ipr Mode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ipr-mode)

(defvar ipr-mode-map nil)
(defvar *ipr-prefix* "\C-z")
(defvar *ipr-batch-mode* nil)
(defvar *ipr-warn-buff* "ipr-warnings")
(defvar *ipr-reject-view-buff-name* "reject-view")
(defvar *ipr-reject-view-buff* nil)
(defvar *ipr-plan-buff* nil)
(defvar *ipr-plan-buff-name* "*Plans*")
(defvar *ipr-eval-region-info* nil)
(defvar ipr-ilisp-value nil)
(defvar *ipr-forms* '(def-language def-theory prove def-theorem def-axiom
		      def-predicate def-term))

;; Make .ipr files come up in ipr-mode.
;;;(setq auto-mode-alist (append auto-mode-alist
;;;			      '(("\\.ipr[0-9~]*$" . ipr-mode))))

(defun setup-ipr-mode-map (prefix)
  (setq ipr-mode-map (copy-keymap lisp-mode-map))
  (define-key ipr-mode-map (concat prefix "d") 'eval-defun-ipr)
  (define-key ipr-mode-map (concat prefix "V") 'ipr-version)
  (define-key ipr-mode-map (concat prefix "e") 'ipr-equal-verify)
  (define-key ipr-mode-map (concat prefix "p") 'ipr-prove-formula)
  (define-key ipr-mode-map (concat prefix "c") 'ipr-claim)
  )

(if ipr-mode-map nil
  (setup-ipr-mode-map *ipr-prefix*))

(defun ipr-mode ()
  "Major mode for Ipr files."
  (interactive)
  ;; Go into lisp mode for a second
  (lisp-mode)
  ;; Get rid of local variables
  (kill-all-local-variables)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  ;; Install the keyboard map and other standard stuff.
  (use-local-map ipr-mode-map)
  (setq mode-name "Ipr")
  (setq major-mode 'ipr-mode)
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (set-syntax-table lisp-mode-syntax-table)
  (setq mode-line-process 'ilisp-status)
;;;  (put 'forall 'fi:lisp-indent-hook 1)
;;;  (put 'for-some 'fi:lisp-indent-hook 1)
;;;  (put 'the-class-of-all 'fi:lisp-indent-hook 1)
;;;  (put 'the 'fi:lisp-indent-hook 1)
;;;  (put 'implies 'fi:lisp-indent-hook 1)
  (put 'forall 'lisp-indent-hook 1)
  (put 'for-some 'lisp-indent-hook 1)
  (put 'the-class-of-all 'lisp-indent-hook 1)
  (put 'the 'lisp-indent-hook 1)
  (put 'implies 'lisp-indent-hook 1)
;;;  (put 'forall 'common-lisp-indent-hook 1)
;;;  (put 'for-some 'common-lisp-indent-hook 1)
;;;  (put 'the-class-of-all 'common-lisp-indent-hook 1)
;;;  (put 'the 'common-lisp-indent-hook 1)
;;;  (put 'implies 'common-lisp-indent-hook 1)
;;;  (def-lisp-indentation implies 1)
;;;  (def-lisp-indentation forall 1)
;;;  (def-lisp-indentation for-some 1)
;;;  (def-lisp-indentation the-class-of-all 1)
;;;  (def-lisp-indentation the 1)
  (setq max-lisp-eval-depth 400)
  ;; Run any hooks the user has installed.
;;;  (run-hooks 'ipr-mode-hook)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface Support:                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ipr-running-p ()
  (and (fboundp 'ilisp-process)
       ilisp-buffer
       (get-buffer ilisp-buffer)
       (ilisp-process)		; seriously changed june 23 94 --bps
       (setq *ipr-dialect-string* (process-name (ilisp-process)))))

(defun ipr-busy-p ()
  (and (ipr-running-p)
       (not (string= ilisp-status " :ready"))))

(defun init-eval-region ()
;;;  (setq *elapsed-time* 0)
  (setq *ipr-eval-region-info* nil))

;;;Returns the position of the beginning of the first defun in the region
(defun eval-region-next-defun ()
  (nth 0 (car *ipr-eval-region-info*)))

;;;Returns the position of the beginning of the last defun int the region
(defun eval-region-last-defun ()
  (nth 1 (car *ipr-eval-region-info*)))

(defun eval-region-source-buffer ()
  (nth 2 (car *ipr-eval-region-info*)))

(defun eval-region-source-window ()
  (nth 3 (car *ipr-eval-region-info*)))

(defun eval-region-kill-buffer ()
  (nth 4 (car *ipr-eval-region-info*)))

(defun eval-region-with-faith ()
  (nth 5 (car *ipr-eval-region-info*)))

(defun eval-region-require ()
  (nth 6 (car *ipr-eval-region-info*)))

;;;This changes the eval-region-next-defun of the first element
;;;of *e-r-i* to val.
(defun set-eval-region-next-defun (val)
  (setq *ipr-eval-region-info*
    (cons (list val
		(eval-region-last-defun)
		(eval-region-source-buffer)
		(eval-region-source-window)
		(eval-region-kill-buffer)
		(eval-region-with-faith)
		(eval-region-require))
	  (cdr *ipr-eval-region-info*))))

;;;This changes the eval-region-last-defun of the first element
;;;of *e-r-i* to val.
(defun set-eval-region-last-defun (val)
  (setq *ipr-eval-region-info*
    (cons (list (eval-region-next-defun)
		val
		(eval-region-source-buffer)
		(eval-region-source-window)
		(eval-region-kill-buffer)
		(eval-region-with-faith)
		(eval-region-require))
	  (cdr *ipr-eval-region-info*))))

(defun push-eval-region-info (nd ld sb sw kb wf &optional require)
  (setq *ipr-eval-region-info*
    (cons (list nd ld sb sw kb wf require) *ipr-eval-region-info*)))

(defun pop-eval-region-info ()
  (setq *ipr-eval-region-info* (cdr *ipr-eval-region-info*)))

;;;Temporarily go to the (eval-region-source-window) and, if 
;;;everything checks out, then execute forms.
(defmacro ensure-source-window (&rest body)
  (` (let ((cw (selected-window)))
       (if (eval-region-source-buffer)
	   (set-buffer (eval-region-source-buffer)))
       (unwind-protect
	   (progn
	     (if (and (eval-region-source-window)
		      (window-point (eval-region-source-window))
		      (eq (window-buffer (eval-region-source-window))
			  (eval-region-source-buffer)))
		 (select-window (eval-region-source-window)))
	     (,@ body))
	 (select-window cw)))))

(defun ipr-read-input (string)
  "This is the function to use for sending input to ipr
when it is running.  Particularly when it is waiting for input."
  (comint-send-string (ilisp-process) string))

(defun input-string-to-ipr (string)
  "This is the function to use for sending input to ipr
when it is not running."
  (if (ipr-running-p)
      (if (ipr-busy-p)
	  ;; this MUST be an error!
	  (error "Ipr is not waiting for input.")
	(ilisp-eval-string string))
    (ipr)))

(defun add-word (word exp)
  (format "(%s %s)" word exp))

;; Replace s1 with s2 in str.
(defun string-replace (s1 s2 str)
  (let* ((qs1 (regexp-quote s1))
	 (current 0)
	 (ls1 (length s1))
	 (ret "")
	 (match (string-match qs1 str)))
    (while match
      (setq ret (concat ret (substring str current match) s2))
      (setq current (+ match ls1))
      (setq match (string-match qs1 str current)))
    (setq ret (concat ret (substring str current)))
    ret))

(defun last-line-p ()
  (= (save-excursion (beginning-of-line) (point))
     (save-excursion (forward-line) (beginning-of-line) (point))))

(defun next-defun ()
  (if (last-line-p)
      (end-of-line)
    (progn
      (forward-line 1)
      (beginning-of-line)
      (if (re-search-forward "^(" nil t nil)
	  (goto-char (- (point) 1))
	(goto-char (point-max))))))

(defun in-defun-p ()
  (save-excursion
    (let ((point (point)))
      (and (progn (end-of-line) (beginning-of-defun))
	   (progn (forward-sexp 1)
		  (end-of-line) t)
	   (>= (point) point)))))

(defun form-in ()
  "Returns a string which contains the first sexp in the current defun."
  (if (not (in-defun-p))
      nil
    (save-excursion
      (end-of-line)
      (beginning-of-defun)
      (forward-char 1)
      (let ((end (progn (forward-sexp 1) (point))))
	(backward-sexp 1)
	(car (read-from-string (buffer-substring (point) end)))))))

(defun form-type ()
  (let ((fi (form-in)))
    (cond ((null fi) nil)
	  ((eq fi '=) 'equality)
	  ((not (memq fi *ipr-forms*)) 'theorem)
	  (t fi))))

(defun apply-translations (exp)
  (let (;;(translation *translation-alist*)
	translation
	(expr (format "%s" exp)))
    (while translation
      (setq expr (string-replace (car (car translation))
				 (cdr (car translation)) expr))
      (setq translation (cdr translation)))
    expr))

;;;This sets up the value of *e-r-i* properly.
(defun eval-region-setup (start end sel-b sel-w &optional kill-buffer 
							  with-faith symbol)
  (let ((cur-b (current-buffer)))
    (set-buffer sel-b)
    (push-eval-region-info nil nil sel-b sel-w kill-buffer with-faith symbol)
				; Put this defun into *e-r-i* with
				; no next or last defun
    (goto-char end)
    (beginning-of-defun)
    (set-eval-region-last-defun (point)) ; In case there are more than one
				; formulas in the region
    (goto-char start)
    (if (in-defun-p)
	(progn
	  (end-of-line)
	  (beginning-of-defun))
      (next-defun))
    (set-eval-region-next-defun (point))
    (set-buffer cur-b)))

(defun eval-region-continue (&optional kind)
  (if *ipr-eval-region-info*
      (if (<= (eval-region-next-defun) ; These are values of point
	      (eval-region-last-defun))
	  (ensure-source-window
	   (goto-char (eval-region-next-defun))
	   (forward-sexp)
	   (let ((defun-string (buffer-substring (eval-region-next-defun)
						 (point)))
		 (start-defun (eval-region-next-defun))
		 (form-type (form-type)))
	     (cond
	      ((eq kind 'claim)
	       (setq defun-string (add-word 'claim defun-string)))
	      ((eq kind 'equality)
	       (setq defun-string (add-word 'equal-verify defun-string)))
	      ((and (eq kind 'prove) (eq form-type 'def-theorem))
	       ;; this is so that we can prove def-theorems.
	       ;; normally we just evaluate them.
	       (setq defun-string
		 (add-word
		  'prove
		  (format
		   "%s" (car (cdr (cdr (car (read-from-string
					     defun-string)))))))))
	      ((and (eq kind 'prove) (not (eq form-type 'prove)))
	       (setq defun-string (add-word 'prove defun-string))))
	     (next-defun)
	     (set-eval-region-next-defun (point))
	     (goto-char start-defun)
	     (ilisp-eval-string (apply-translations defun-string))
	     (eval-region-continue kind)
	     ))
	(progn
	  (if (eval-region-kill-buffer)
	      (kill-buffer (eval-region-source-buffer)))
	  (cond ((null (cdr *ipr-eval-region-info*))
		 (pop-eval-region-info))
;;;		  ((eval-region-require)
;;;		   (let ((req (eval-region-require)))
;;;		     (pop-eval-region-info)
;;;		     (ilisp-eval-string 
;;;		      (add-emacs-catch (format "(check-provided '%s)" req)))))
		(t (pop-eval-region-info)
		   (eval-region-continue kind)))))))

(defun prepare-chk-buff ()
  (condition-case nil
      (set-buffer *chk-buff*)
    (error (set-buffer (make-new-chk-buff))))
  (if (> (buffer-size) 2)
      (if (y-or-n-p "Do you want to discard the previous proof?")
	  (ipr-erase-r-o-buffer)
	(set-buffer (make-new-chk-buff)))
    (ipr-erase-r-o-buffer)))

(defun switch-to-chk ()
  (if (not (eq (window-buffer) *chk-buff*))
      (switch-to-buffer-other-window *chk-buff*)))

(defvar *hlps-status-stack* nil)

(defun hlps-push-status ()
  (setq *hlps-status-stack* (cons ilisp-status *hlps-status-stack*)))

(defun hlps-pop-status ()
  (setq ilisp-status (car *hlps-status-stack*))
  (setq *hlps-status-stack* (cdr *hlps-status-stack*))
  (if comint-show-status
      (progn
	(save-excursion (set-buffer (other-buffer)))
	(sit-for 0))))

(defun hlps-lisp-gc-start ()
  (hlps-push-status)
  (ilisp-update-status 'GC))

(defun hlps-lisp-gc-end ()
  (hlps-pop-status))

(defun hlps-set-runbar ()
  (hlps-push-status)
  (ilisp-update-status 'think))

(defun hlps-clear-runbar ()
  (hlps-pop-status))

(defun hlps-lisp-compile-start ()
  (hlps-push-status)
  (ilisp-update-status 'compile))

(defun hlps-lisp-compile-end ()
  (hlps-pop-status))

(defun hlps-lisp-compile-final-start ()
  (hlps-push-status)
  (ilisp-update-status 'compile-final))

(defun hlps-lisp-match-start ()
  (hlps-push-status)
  (ilisp-update-status 'match))

(defun hlps-lisp-match-end ()
  (hlps-pop-status))

(defun hlps-lisp-unify-some-start ()
  (hlps-push-status)
  (ilisp-update-status 'unify-some))

(defun hlps-lisp-unify-some-end ()
  (hlps-pop-status))

(defun hlps-lisp-unify-start ()
  (hlps-push-status)
  (ilisp-update-status 'unify))

(defun hlps-lisp-unify-end ()
  (hlps-pop-status))

(defun hlps-lisp-delete-start ()
  (hlps-push-status)
  (ilisp-update-status 'delete))

(defun hlps-lisp-delete-end ()
  (hlps-pop-status))

(defun hlps-lisp-equality-start ()
  (hlps-push-status)
  (ilisp-update-status 'equality))

(defun hlps-lisp-equality-end ()
  (hlps-pop-status))

(defun hlps-lisp-kb-start ()
  (hlps-push-status)
  (ilisp-update-status 'kb))

(defun hlps-lisp-kb-end ()
  (hlps-pop-status))

(defun ipr-erase-r-o-buffer ()
  (let ((buffer-read-only nil))
    (erase-buffer)
    (set-buffer-modified-p nil)))

(defun insert-sexps (plan)
  (insert (pp-to-string plan)))

;;;(defun send-plan (plan)
;;;  (condition-case err
;;;      (progn
;;;	(let ((old-buff (current-buffer)) start end)
;;;	  (setq *ipr-plan-buff* (get-buffer-create *ipr-plan-buff-name*))
;;;	  (set-buffer *ipr-plan-buff*)	;((get-buffer buffer))
;;;	  (goto-char (point-max))
;;;	  (insert-sexps plan)
;;;	  (newline)
;;;	  (switch-to-buffer-other-window *ipr-plan-buff*)))
;;;    (end-of-file (message (format "No plan to display.")) (sit-for 3))
;;;    (wrong-type-argument (message (format "Wrong type argument %s" err)))
;;;    (void-variable (message (format "No plan to display.")) (sit-for 3))))

(defun send-plan (plan)
  (condition-case err
      (progn
	(let ((old-buff (current-buffer)) start end)
	  (setq *ipr-plan-buff* (get-buffer-create *ipr-plan-buff-name*))
	  (set-buffer *ipr-plan-buff*) ;((get-buffer buffer))
	  (goto-char (point-max))
	  (insert plan)
	  (newline)
	  (switch-to-buffer-other-window *ipr-plan-buff*)))
    (end-of-file (message (format "No plan to display.")) (sit-for 3))
    (wrong-type-argument (message (format "Wrong type argument %s" err)))
    (void-variable (message (format "No plan to display.")) (sit-for 3))))

(defun ipr-put-string-into-proof-buffer (string)
  (condition-case err
      (let ((old-buff (current-buffer)) start end)
	(set-buffer *chk-buff*)	;((get-buffer buffer))
	(ipr-erase-r-o-buffer)
	(editing-read-only-buffer
	 (insert string)
	 (newline))
	(goto-char (point-min))
	(set-buffer old-buff))
    (end-of-file 
     (message (format "Nothing to display.  %s" string))
     (sit-for 3))
    (void-variable 
     (message (format "Nothing to display."))
     (sit-for 3))
    (error (message (format "in ipr-put-string-into-proof-buffer"))
	   (sit-for 3))))

(defun ipr-send-proof (emacs-proof &optional num reject) ;( &optional buffer)
  "This sends the emacs-proof to the check-on buffer and then sets
point at the numth proof-form."
  (if reject
      (let ((old-buff (current-buffer)))
	(setq *ipr-reject-view-buff*
	  (get-buffer-create *ipr-reject-view-buff-name*))
	(set-buffer *ipr-reject-view-buff*)
	(lisp-mode)
	(erase-buffer)
	(chk-pretty-insert emacs-proof)
	(newline)
	(goto-char (point-min))
	(set-buffer old-buff)
	(if (minibuffer-window-selected-p)
	    (set-window-buffer (split-window (previous-window))
			       *ipr-reject-view-buff*)
	  (set-window-buffer (split-window) *ipr-reject-view-buff*)))
    (condition-case err
	(let ((old-buff (current-buffer)) start end)
	  (set-buffer *chk-buff*) ;((get-buffer buffer))
	  (ipr-erase-r-o-buffer)
	  (editing-read-only-buffer
	   (chk-pretty-insert emacs-proof)
	   (newline))
	  (if num (chk-goto num)
	    (goto-char (point-min)))
	  (set-buffer old-buff))
      (end-of-file 
       (message 
	(format
	 "No proof to display.  Please request another output option."))
       (sit-for 3))
      (void-variable 
       (message 
	(format
	 "No proof to display.  Please request another output option."))
       (sit-for 3))
      (error (error "in ipr-send-proof")))))

(defun ipr-warn (string)
  (let ((cur-b (current-buffer))
	(warn-b (get-buffer-create *ipr-warn-buff*)))
    (set-buffer warn-b)
    (goto-char (point-max))
    (insert "
")
    (insert string)
    (if (one-window-p t)
	(if (minibuffer-window-selected-p)
	    (progn (set-window-buffer (previous-window) warn-b)
		   (set-window-buffer (split-window) cur-b))
	  (set-window-buffer (split-window) warn-b))
      (set-window-buffer (previous-window) warn-b))
    (set-buffer cur-b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface Commands:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I need this simply to evaluate the region in the ipr-lisp
;;; process.

(defun eval-region-ipr (start end)
  (interactive "r\n")
  (if (ipr-running-p)
      (if (ipr-busy-p)
	  (error "Ipr is busy.")
	(init-eval-region)
	(eval-region-setup
	 start end (current-buffer) (selected-window) nil
	 (and current-prefix-arg t))
	(eval-region-continue))
    (ipr)))

(defun eval-defun-ipr ()
  (interactive)
  (let ((form (form-type))
	(end nil))
    (if (not form)
	(error "Not inside a proper form."))
    (end-of-defun)
    (setq end (point))
    (beginning-of-defun)
    (eval-region-ipr (point) end)))

(defun ipr-toggle-batch-mode ()
  (interactive)
  (input-string-to-ipr
   (format "(setq *ipr-batch-mode* (not %s))" *ipr-batch-mode*))
  (setq *ipr-batch-mode* (not *ipr-batch-mode*))
  (message (format "You are %s batch-mode."
		   (if *ipr-batch-mode* "in" "not in")))
  (sleep-for 1)
  )

(defun ipr-prove-formula ()
  ;; in the future this will use skip-chars-forward and skip-chars-backward
  ;; with argument "^\"" to set the region.  This will extract the string
  ;; containing point.  (or not).
  (interactive)
  (if (ipr-running-p)
      (if (ipr-busy-p) (error "Ipr is busy.")
	(let ((form (form-type)) (end nil))
	  (if (not form)
	      (error "Please position the cursor in a proper formula."))
	  (setq form 'prove)
	  (end-of-defun)
	  (setq end (point))
	  (beginning-of-defun)
	  (init-eval-region)
	  (eval-region-setup
	   (point) end (current-buffer) (selected-window) nil
	   (and current-prefix-arg t))
	  (setq *chk-buff* (generate-new-buffer *chk-buff-name*))
	  (switch-to-buffer *chk-buff*)
	  (insert "Waiting for proof from ipr.")
	  ;; This should be erased when the proof arrives.
	  (ipr-chk-mode)
;;;	  (chk-equalize-variables)
	  (eval-region-continue 'prove)
;;;	  (chk-equalize-variables)
	  ))
    (ipr)))

;;;(defun ipr-prove-statement (statement proof)
;;;  ;; This should execute proof.  If it fails then do something.
;;;  ;; PROOF is of the form '(proof . plan)
;;;  (setq *chk-buff* (generate-new-buffer *chk-buff-name*))
;;;  (switch-to-buffer *chk-buff*)
;;;  (set-buffer *chk-buff*)
;;;  (insert "Waiting for proof from ipr.")
;;;  ;; This should be erased when the proof arrives.
;;;  (ipr-chk-mode)
;;;;;;  (chk-equalize-variables)
;;;;;;  (eval-region-continue 'prove)
;;;  (ilisp-eval-string (apply-translations (` (prove-fun '(, statement)
;;;						       '
;;;  )

(defun ipr-claim ()
  (interactive)
  (if (ipr-running-p)
      (if (ipr-busy-p)
	  (error "Ipr is busy.")
	(let ((form (form-type))
	      (end nil))
	  (if (not (in-defun-p))
	      (error "Please position the cursor in a proper formula."))
	  (setq form 'claim)
	  (end-of-defun)
	  (setq end (point))
	  (beginning-of-defun)
	  (init-eval-region)
	  (eval-region-setup
	   (point) end (current-buffer) (selected-window) nil
	   (and current-prefix-arg t)))
	(input-string-to-ipr (format "(setq *location-here* '%s)"
				     (*ipr-location-here*)))
	(eval-region-continue 'claim)))
  (ipr))

(defun ipr-equal-verify ()
  (interactive)
  (if (ipr-running-p)
      (if (ipr-busy-p)
	  (error "Ipr is busy.")
	(let ((form (form-type))
	      (end nil))
	  (if (not (in-defun-p))
	      (error "Please position the cursor in a proper formula."))
	  (if (not (eq form 'equality))
	      (error "Please enter an equality."))
	  (end-of-defun)
	  (setq end (point))
	  (beginning-of-defun)
	  (init-eval-region)
	  (eval-region-setup
	   (point) end (current-buffer) (selected-window) nil
	   (and current-prefix-arg t)))
	(input-string-to-ipr (format "(setq *location-here* '%s)"
				     (*ipr-location-here*)))
	;; calls equal-verify.
	(eval-region-continue 'equality))
    (ipr)))

(defun ipr-quit-that ()
  (interactive)
  (input-string-to-ipr (format "(setq *bledsoe* ':quit)")))
;;input-string-to-ipr "(progn (setq *quit* t) (setq *bledsoe* :quit))")

(defun ipr-check-on ()
  (interactive)
  (prepare-chk-buff)
  (switch-to-chk)
  (message "Waiting for ipr to send a proof.  Please be patient.")
  (input-string-to-ipr "c"))

(defun ipr-init ()
  (interactive)
  (if (ipr-running-p)
      (if (ipr-busy-p)
	  (error "Ipr is busy.")
	(let ((ans (read-minibuffer "Initialize Ipr Completely (y/n): ")))
	  (if (eq ans 'y)
	      (ilisp-eval-string
	       "(lisp:progn (ipr-init)
                     (util:emacs-eval
                      '(message
                          \"\\\"Ipr reset to initial state\\\"\")))"))))
    (ipr)))

(defun ipr-version ()
  (interactive)
  (if (ipr-running-p)
      (message "Ipr Version %s." *ipr-release*)
    (error "Ipr is not running.")));;why error?

(defun ipr-start ()
  (interactive)
  (if (ipr-running-p)
      (error "Ipr is already running.")
    (ipr)))

