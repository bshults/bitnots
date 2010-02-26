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
;;; Emacs Eval independent module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'ilisp)
(require 'bridge)
(provide 'emacs-eval)

(defvar *lisp-eval-buffer* "")
;;;(defvar *lisp-eval-buffer* (buffer-name (process-buffer (ilisp-process))))
(defvar *no-eval-echo* nil)	; set to T for release!

;; Always install bridge.
(add-hook 'clisp-hook '(lambda ()
			(add-hook 'ilisp-init-hook
			 '(lambda () (install-bridge)))))

;; Set up bridge properly.
(setq bridge-hook 
      '(lambda ()
	;; Don't insert in source or destination buffer
	(setq bridge-source-insert nil)
	(setq bridge-destination-insert nil)
;;;	(setq bridge-chunk-size 250)
	;; Set up bridge handler
	(setq bridge-handlers
	 '((".*" . catch-lisp-eval)))))

;; Process info that comes to emacs from the bridge.
(defun catch-lisp-eval (process string)
  (if string
      (setq *lisp-eval-buffer* (concat *lisp-eval-buffer* string))
      (let ((buffer (current-buffer))
	    (eval-string *lisp-eval-buffer*))
	(setq *lisp-eval-buffer* "")
	(condition-case err
	    (eval (read eval-string))
	  (error 
	   (with-output-to-temp-buffer "*Ipr-messages*"
	     (print 
	      (format "Bridge Error: %s on eval of: %s" err eval-string)))))
	(set-buffer buffer))))

;; The following evaluates the string in lisp taking into account
;; the package and file of the current buffer.
;; *no-eval-echo* for keeping the ilisp buffer clear of output.
(defun ilisp-eval-string (string)
  (comint-send
   (ilisp-process)
   (format (ilisp-value 'ilisp-eval-command)
	   (lisp-slashify string)
	   (lisp-buffer-package)
	   (buffer-file-name))
   *no-eval-echo*))

;; Use the following when trying to evaluate an expression in LISP.
(defun ilisp-eval (exp)
  (ilisp-eval-string (format "%s" exp)))

(defun ipr-eval (exp)
  (append-string-to-buffer ilisp-buffer (format "%s" exp))
  (read ilisp-buffer))

(defun append-string-to-buffer (buffer string)
  (save-excursion
    (set-buffer (get-buffer-create buffer))
    (goto-char (point-max))
    (insert string)))

