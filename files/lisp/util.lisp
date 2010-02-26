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

;;; This file contains various utilities.  Some of them may not be
;;; used!

(defmacro nor (&body body)
  `(not (or ,@body)))

(defmacro aif (if then else)
  `(let ((aif-it ,if)) (if aif-it ,then ,else)))

(emacs-indent while 1)

(defun mappend (fn &rest lists)
  (apply #'append (apply #'mapcar fn lists)))

(defmacro while (form &body body)
  `(loop
    (or ,form (return nil))
    ,@body))

(defun copy-equal-hash-table (hash)
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p hash)
      (maphash #'(lambda (key value)
		   (setf (gethash key copy) value)) hash))
    copy))

(defun copy-hash-table (hash)
  (let ((copy (make-hash-table)))
    (maphash #'(lambda (key value)
		 (setf (gethash key copy) value)) hash)
    copy))

(defun print-hashtable (hashtable)
  (maphash #'(lambda (key dat)
	       (format t "~%key:  ~S~%datum:  ~S~%" key dat))
	   hashtable))

(defmacro do-tails ((var list) &body body)
  `(do ((,var ,list (cdr ,var)))
       ((null ,var))
     ,@body))

(defmacro do-tails-t ((var list) &body body)
  `(do ((,var ,list (cdr ,var)))
       ((null ,var) t)
     ,@body))

(defun concat-symbols (s1 s2 &optional (package *package*))
  (intern (concatenate 'string (string s1) (string s2))
          package))

(defun combine-symbols (s1 s2 &optional (package *package*))
  (intern (concatenate 'string (string s1) "-" (string s2))
          package))

(defun combine-symbol-list (s-list &optional (package *package*))
  (cond ((null s-list) nil)
        ((null (rest s-list)) (car s-list))
        (t (combine-symbols (car s-list)
                            (combine-symbol-list (cdr s-list) package)
                            package))))

(defun create-name (&rest symbols)
  (combine-symbol-list symbols))

(defun pop-nthcdr (num list)
  (let ((a (nthcdr (- num 1) list)))
    (pop (cdr a))))

(defun push-nthcdr (arg num list)
  (let ((a (nthcdr (- num 1) list)))
    (push arg (cdr a)))
  list)

(defun remove-nth (num list)
  (pop-nthcdr num list)
  list)

(defun pushnew-nthcdr* (arg num list &key (test #'eql))
  "This pushes ARG onto the NUMth cdr of LIST if ARG is not
a member of LIST according to the :TEST which defaults to #'eql."
  (let ((a (nthcdr (- num 1) list)))
    (if (member arg list :test test) arg
      (push arg (cdr a)))))

(defun the* (test list)
  ;; if there is exactly one element of LIST satisfying TEST then
  ;; it is returned.  Otherwise NIL.
  (let ((the1 (member-if test list)))
    (if (and the1 (not (member-if test (cdr the1))))
	(car the1))))

