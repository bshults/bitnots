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

(require 'ilisp)

(setq load-path (append load-path (list *ipr-interface-directory*)))

(require 'emacs-eval)
(require 'ipr-mode)
(require 'ipr-chk-mode "check-on")

(defvar *ipr-dialect*)
(defvar *ipr-dialect-string* "")
(defvar *binary-load* nil)
(setq ilisp-motd nil)

(defun ipr ()
  (interactive)
  (if (ipr-running-p)
      (message "Ipr is already running.")

;;; Customization:

;;; Comment the following code and uncomment one of the lines below
;;; according to the CL implementation you will use.

    (setq *ipr-dialect*
      (let ((lisp
	     (read-from-minibuffer
	      "What lisp? (akcl lispworks gcl lucid clisp-hs acl cmu) [gcl]: ")))
	(if (string= lisp "") 'ipr-gcl (intern (concat "ipr-" lisp)))))

;;; For Allegro CL uncomment:
;;;     (setq *ipr-dialect* 'ipr-acl)
;;; For GNU CL uncomment:
;;;     (setq *ipr-dialect* 'ipr-gcl)
;;; For AKCL uncomment:
;;;     (setq *ipr-dialect* 'ipr-akcl)
;;; For Lucid uncomment:
;;;     (setq *ipr-dialect* 'ipr-lucid)
;;; For CLISP uncomment:
;;;     (setq *ipr-dialect* 'ipr-clisp-hs)
;;; For CMU CL uncomment:
;;;     (setq *ipr-dialect* 'ipr-cmu)
;;; For Harlequin LispWorks uncomment:
;;;     (setq *ipr-dialect* 'ipr-lispworks)
;;; End customization.

    ;; Start up ipr quitely.
    (setq mode-line-process 'ilisp-status)
    (setq *binary-load* nil)
    (if current-prefix-arg
	(progn (setq current-prefix-arg nil) (setq *binary-load* t)))
    (let ((lptb (symbol-function 'lisp-pop-to-buffer)))
      (unwind-protect
	  (progn (fset 'lisp-pop-to-buffer '(lambda (buffer) buffer))
					; this keeps it from going to the
					; ilisp buffer
		 (funcall (symbol-function *ipr-dialect*)))
					; this just calls ipr-lucid or ipr-akcl
	(fset 'lisp-pop-to-buffer lptb)))))

(defdialect ipr-lucid "Lucid Common LISP running Ipr"
  lucid
  (ilisp-load-init (format "%s" 'ipr-lucid)
		   (format "%s/ipr.lisp" *ipr-interface-directory*))
  (setq comint-prompt-regexp "^\\(->\\)+ \\|^[^> \^[\^]]*> "
	comint-fix-error ":a"
	ilisp-reset ":a :t"
	comint-continue ":c"
	comint-interrupt-regexp ">>Break: Keyboard interrupt"
	comint-prompt-status 
	(function (lambda (old line)
		    (comint-prompt-status old line 'lucid-check-prompt))))
  (setq ilisp-error-regexp "ILISP:[^\"]*\\|>>[^\n]*")
  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-find-source-command 
	"(ILISP:ilisp-source-files \"%s\" \"%s\" \"%s\")")
  (setq ilisp-binary-command 
	"(first (last lucid::*load-binary-pathname-types*))"))


(defdialect ipr-akcl "AKCL Common LISP running Ipr"
  akcl
  (ilisp-load-init (format "%s" 'ipr-akcl) 
		   (format "%s/ipr.lisp" *ipr-interface-directory*)))

(defdialect ipr-lispworks "Lispworks Common LISP running Ipr"
  lispworks
  (ilisp-load-init (format "%s" 'ipr-lispworks)
		   (format "%s/ipr.lisp" *ipr-interface-directory*)))

(defdialect ipr-gcl "GNU Common LISP running Ipr"
  gcl
  (ilisp-load-init (format "%s" 'ipr-gcl) 
		   (format "%s/ipr.lisp" *ipr-interface-directory*)))

(defdialect ipr-acl "Allegro Common LISP running Ipr"
  allegro
  (ilisp-load-init (format "%s" 'ipr-acl) 
		   (format "%s/ipr.lisp" *ipr-interface-directory*)))

(defdialect ipr-clisp-hs "Haible, Stoll's CLISP Common LISP running Ipr"
  clisp-hs
  (ilisp-load-init (format "%s" 'ipr-clisp-hs)
		   (format "%s/ipr.lisp" *ipr-interface-directory*)))

(defdialect ipr-cmu "CMU Common LISP running Ipr"
  cmulisp
  (ilisp-load-init (format "%s" 'ipr-cmulisp) 
		   (format "%s/ipr.lisp" *ipr-interface-directory*)))

