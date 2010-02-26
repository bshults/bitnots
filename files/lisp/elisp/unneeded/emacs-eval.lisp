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

(in-package :util)

;; Doesn't bother to check for quotes in a string.  This doesn't
;; downcase things that ought not be downcased in GCL and CLISP-HS.
;; It ought to work in Allegro too.
(defun simple-downcase (string)
  (let ((in-string nil) (next nil))
    (dotimes (x (length string))
      (setq next (aref string x))
      (cond ((and (not in-string) (char= next #\"))
	     (setq in-string t))
	    ((and in-string (char= next #\"))
	     (setq in-string nil))
	    ((not in-string)
	     (setf (aref string x) (char-downcase next)))))
    string))

(export 'emacs-eval)

(defvar *last-message-time* (get-internal-real-time))
(defvar *message-prefix* (format nil "~a" (character 27)))

(defvar *message-suffix* (format nil "~a" (character 29)))

;; If things stop working, exchange simple-downcase with string-downcase.
(defun emacs-eval (exp)
  "This sends output in a form which will be picked up by the bridge."
  (setf *last-message-time* (get-internal-real-time))
  (format t "~a~a~a"
	  *message-prefix*
;;;	  (string-downcase (format nil "~a" exp))
	  (simple-downcase (format nil "~a" exp))
	  *message-suffix*))

(export 'emacs-indent)

(defmacro emacs-indent (name number-to-skip)
  `(emacs-eval '(put ',name 'lisp-indent-hook ,number-to-skip)))

(export 'emacs)

(export '*visible-evaluation?*)

(defvar *visible-evaluation?* nil)

(export 'quote-string)

(defun quote-string (string)
  (format nil "\"~a\"" string))

(export 'when-visible-eval)

(defun when-visible-eval (exp)
  (when *visible-evaluation?*
    (emacs-eval exp)))

(export 'get-value-from-emacs)

(defun get-value-from-emacs (&key message function)
  (if message
      (emacs-eval
       `(progn (beep)
	       (comint-send-string
		(ilisp-process)
		(concat
		 (read-from-minibuffer 
		  ,(quote-string message)) \"\\\n\"))))
    (emacs-eval
     `(comint-send-string
       (ilisp-process)
       (format \"%s\\\n\" (funcall ,function)))))
  (read *terminal-io*))

(export 'give-value-to-emacs)

(defun give-value-to-emacs (command)
  (emacs-eval `(setq ipr-ilisp-value ',command)))

