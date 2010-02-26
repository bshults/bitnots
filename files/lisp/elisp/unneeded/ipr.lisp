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
;;; Initializations for Ipr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-allegro
(unless (find-package :util)
  (make-package :util :use '(lisp)))

#-allegro
(unless (find-package :ipr)
  (make-package :ipr :use '(util lisp)))

#+allegro
(defpackage :util (:use :common-lisp))
#+allegro
(defpackage :ipr (:use :util :common-lisp))

(in-package :ipr)

;;; Customize:
;;; Replace the string below with the location of the IPR directory on
;;; your system.
(defvar *ipr-directory* "/home/bshults/work/ipr/test/newest")
;;; End customize.

;; Load lisp end of emacs-eval.
(load (format nil "~a/interface/emacs-eval.lisp" *ipr-directory*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Lucid compiler seems to have problems if the memory size
;;; isn't grown.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the correct defsystem.
(load (format nil "~a/src/defsys.lisp" *ipr-directory*))

;; Load up Ipr.
(load-ipr)

