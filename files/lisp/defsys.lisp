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
;;; Defsys for Ipr theorem-using prover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(use-package :ipr)

;;; The package has already been created if needed by ../interface/ipr.lisp.

(in-package :ipr)

;;; This file loads ipr.

;;; This should be T in the distribution.
(defvar *production-compile* nil)

(proclaim '(optimize speed))
(proclaim '(optimize (compilation-speed 0)))
(proclaim '(optimize (debug 0)))
(proclaim '(optimize (space 0)))
(proclaim '(optimize (safety 0)))

(proclaim '(inline patch-in match-form-with-theorem o-combine list-equal
		   formula-set-applicable eligible? finish-gamma new-juncts
		   last-part-gamma map-reps-hyps map-pr new-param-p
		   hypotheses goals set-splits formulas unify-obj-formulas
		   add-to-f-cache unify-formulas send-formulas-to-unifier
		   same-length unify-terms non-silly-option
		   ))

(defvar *time-stamps* nil)

(defun expand-names (files)
  (mapcar #'(lambda (file)
	      (concatenate 'string *ipr-directory* file))
	  files))

(defvar *util-files*
    (expand-names '(
		    "/src/init"
		    "/src/util"
		    "/src/macros"
		    "/src/structures"
		    )))

(defvar *final-files*
    ;; this should contain any and all files which should be recompiled
    ;; when a macro is changed.
    (expand-names '(
;;;		    "/src/lower-level"
		    "/src/alpha-steps"
		    "/src/beta-steps"
		    "/src/delta-steps"
		    "/src/gamma-steps"
		    "/src/hol-steps"
		    "/src/theory-steps"
		    "/src/brown-steps"
;;;		    "/src/equality"
		    "/src/high-level"
		    )))

(defvar *ipr-files*
    (expand-names '(
		    "/src/variables"
		    "/src/formulas" 
		    "/src/output"
		    "/src/condense"
		    "/src/reducer"
		    "/src/grammar"
		    "/src/bf-unify"
		    "/src/unify"
		    )))

(defvar *kb-files*
    (expand-names '(
		    "/src/kb-interface"
		    "/src/define"
		    "/src/find-theorem"
		    "/src/fetcher"
		    )))

(defun source-file-name (file)
  (concatenate 'string file ".lisp"))

(defun binary-file-name (file)
  (concatenate 'string file
;;; I need Lispworks here.
	       #+lucid ".sbin"
	       #+(and cmu (not x86)) ".sparcf"
	       #+(and cmu x86) ".x86f"
	       #+allegro ".fasl"
	       #+clisp ".fas"
	       #+(or gcl akcl) ".o"
	       #-(or lucid gcl cmu clisp allegro akcl)
	       (error "Unknown system type")))

(defun load-files (files &key force-binary force-src)
  #+:gcl
  (si::use-fast-links (not force-src))
  (mapc #'(lambda (x)
	    (if force-binary (load (binary-file-name x))
	      (if force-src (load (source-file-name x))
		(let ((bin-date (and (probe-file (binary-file-name x))
				     (file-write-date (binary-file-name x))))
		      (src-date (file-write-date (source-file-name x))))
		  (when (or (null bin-date) (> src-date bin-date))
		    (emacs-eval '(hlps-lisp-compile-start))
		    (compile-file (source-file-name x))
		    (emacs-eval '(hlps-lisp-compile-end)))
		  (load (binary-file-name x)))))
	    (push (file-write-date (binary-file-name x)) *time-stamps*))
	files))

(defun load-files-no-timestamps (files &key force-binary)
  (mapc #'(lambda (x)
	    (if force-binary
		(if (probe-file (binary-file-name x))
		    (load (binary-file-name x))
		  (progn (compile-file (source-file-name x))
			 (load (binary-file-name x))))
	      (let ((bin-date (file-write-date (binary-file-name x)))
		    (src-date (file-write-date (source-file-name x))))
		(if (or (null bin-date) (> src-date bin-date))
		    (progn (emacs-eval '(hlps-lisp-compile-start))
			   (compile-file (source-file-name x))
			   (emacs-eval '(hlps-lisp-compile-end))))
		(load x))))
	files))

(defun compile-files (files &key final)
  (mapc #'(lambda (x)
	    (if final
		(emacs-eval '(hlps-lisp-compile-final-start))
	      (emacs-eval '(hlps-lisp-compile-start)))
	    (compile-file (source-file-name x))
	    (emacs-eval '(hlps-lisp-compile-end))
	    (load x)
	    (push (file-write-date
		   (binary-file-name x)) *time-stamps*))
	files))

(defun compile-final-file ()
  (with-open-file (tsfile (concatenate 'string *ipr-directory*
				       "/src/timestamps")
		   :direction :output
		   :if-exists :supersede)
    (write *time-stamps* :stream tsfile)
    (compile-files *final-files* :final t)))

(defun load-ipr (&key force-binary force-src)
  (setf *time-stamps* nil)
  (emacs-eval '(ilisp-update-status ':util))
  ;; Load xp if pretty printing does not exist.
  #-(or allegro xp)
  (if #-kcl t #+kcl nil
      (handler-bind
       ((t #'(lambda (c) (declare (ignore c))
	       (when (find-restart 'continue) (invoke-restart 'continue)))))
       (load-files (list (format nil "~a/src/xp-code"
				 *ipr-directory*)) :force-binary force-binary
				 :force-src force-src))
      (load-files (list (format nil "~a/src/xp-code"
				*ipr-directory*)) :force-binary force-binary
				:force-src force-src))
  #-kcl
  (handler-bind
   ((t #'(lambda (c) (declare (ignore c))
	   (when (find-restart 'continue) (invoke-restart 'continue)))))
   (load-files *util-files* :force-binary force-binary :force-src force-src))
  #+kcl
  (load-files *util-files* :force-binary force-binary :force-src force-src)
  (emacs-eval '(ilisp-update-status ':kb))
  (load-files *kb-files* :force-binary force-binary :force-src force-src)
  (emacs-eval '(ilisp-update-status ':ipr))
  (load-files *ipr-files* :force-binary force-binary :force-src force-src)
  (emacs-eval '(ilisp-update-status ':final))
  (load-files *final-files* :force-binary force-binary :force-src force-src)
;;;  (let ((*print-length* nil))
;;;    (if force-binary (load-files *final-files* :force-binary t)
;;;	(with-open-file (tsfile (concatenate
;;;				 'string *ipr-directory*
;;;				 "/src/timestamps")
;;;				:if-does-not-exist :create)
;;;	  (let ((old-time-stamps (read tsfile nil)))
;;;	    (if (equal *time-stamps* old-time-stamps)
;;;		(load-files *final-files*)
;;;		(compile-final-file))))))
;;;  (emacs-eval '(ilisp-update-status ':init))
;;;  (ipr-init)
;;;  (proclaim '(optimize (speed 2)))
;;;  (proclaim '(optimize (compilation-speed 3)))
;;;  (proclaim '(optimize (safety 3)))
  )

(defun compile-ipr ()
  (when *production-compile*
    (proclaim '(optimize speed))
    (proclaim '(optimize (compilation-speed 0)))
    (proclaim '(optimize (safety 1))))
  (setf *time-stamps* nil)
  (compile-files *util-files*)
;;;  (compile-files *meta-files*)
  (compile-files *ipr-files*)
  (compile-final-file)
;;;  (ipr-init)
  (proclaim '(optimize (speed 2)))
  (proclaim '(optimize (compilation-speed 3)))
  (proclaim '(optimize (safety 3))))

