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

;;; This file contains the functions for manipulating and analysing
;;; terms and formulas.

(defun paths-to-occurrences-of-car (symbol expression)
  (declare (symbol symbol) (type (or symbol list) expression))
  ;; this returns a list, each element of which is a path
  ;; to a cons in expression whose car is symbol.
  ;; e.g. (paths-to-occurrence-of-car 'p '(p (p x (p (p x y) z)) (t (p x y))))
  ;; returns '(nil (1) (1 2) (1 2 1) (2 1))
  (when (consp expression)
    (do ((components (cdr expression) (cdr components))
	 (index 1 (1+ index))
	 (paths (when (eq (car expression) symbol) (list nil))
		(append paths (mapcar #'(lambda (path) (cons index path))
				      (paths-to-occurrences-of-car
				       symbol (car components))))))
	((null components) paths))))

(defun path-to-nth-occurrence-of-car (n exp car)
  ;; this could be more efficient
  (declare (type (integer 1) n) (type (or symbol list) exp) (symbol car))
  (nth (1- n) (paths-to-occurrences-of-car car exp)))

(defun paths-to-occurrences-of-term1 (term expression)
  ;; this returns a list, each element of which is a path
  ;; to a cons in expression which is term-equal to term
  ;; e.g. (paths-to-occurrence-of-term
  ;;        '(p x y) '(p (p x (p (p x y) z)) (t (p x y))))
  ;; returns '((1 2 1) (2 1)) (in reverse order).
  (declare (type (or symbol list) expression term))
  (if (consp expression)
      (do ((components (cdr expression) (cdr components)) (index 1 (1+ index))
	   (paths (when (term-equal expression term) (list nil))
		  (append paths (mapcar #'(lambda (path) (cons index path))
					(paths-to-occurrences-of-term
					 term (car components))))))
	  ((null components) paths)
	(declare (list components paths) (type (integer 1) index)))
    (if (term-equal expression term)
	;; if this is going to be term-equal instead of just equal
	;; then why not use the equality theory as well?
	(list nil))))

(defun path-to-nth-occurrence-of-term (n exp term)
  (declare (type (integer 1) n) (type (or symbol list) exp term))
  ;; this could be more efficient
  (nth (1- n) (paths-to-occurrences-of-term term exp)))

;;; This may need to worry about the, the-class-of-all, forall, etc.
(defun follow-path (expr path)
  (if (null path) expr (follow-path (nth (car path) expr) (cdr path))))

(defun substitute-at-path (expr path replacement)
  (if (null path) replacement
    (subst-nth (car path) expr
	       (substitute-at-path
		(nth (car path) expr) (cdr path) replacement))))

(defun subst-nth (num expr subst)
  (let ((temp (copy-list expr))) (setf (nth num temp) subst) temp))

(defun expression-height (expression)
  (if (listp expression)
      (1+ (reduce 'max (mapcar #'expression-height expression))) 0))

;;; copies starts at 0.  when a copy is made it is increased.  The
;;; outermost var in the new formula is allowed to accept one more
;;; term to its reject list.  So the reject list may be no longer that
;;; (copies formula)

(defun term-occurs-in-term (term1 term2)
  ;; everything is internal
  (declare (type term term1 term2))
  (if (not (consp term1))
      (occurs-in-term term1 term2 nil)
    (cond ((not (consp term2)) nil)
	  ((equal term1 term2) t)
	  ((member (car term2) '(the the-class-of-all))
	   (term-occurs-in-formula term1 (caddr term2)))
;;;	((and *rigid-only* (skolem (car term)))
;;;	 (member term1 (depends-on (car term))))
	  (t (some #'(lambda (tr) (term-occurs-in-term term1 tr))
		   (cdr term2))))))

(defun term-occurs-in-formula (term formula)
  ;; everything is internal
  (if (not (consp term))
      (occurs-in-formula term formula nil)
    (cond ((member (car formula) '(forall for-some))
	   (term-occurs-in-formula term (caddr formula)))
	  ((member (car formula) '(and or if iff not implies))
	   (some #'(lambda (f) (term-occurs-in-formula term f))
		 (cdr formula)))
	  (t (some #'(lambda (tr) (term-occurs-in-term term tr))
		   (cdr formula))))))

(defun instantiate-formula (formula f-cache)
  (declare (list formula f-cache))
  (if f-cache (instantiate-formula1 formula f-cache) formula))

(defun instantiate-formula1 (formula f-cache)
  (case (car formula)
    ((forall for-some) (list (car formula) (cadr formula)
			     (instantiate-formula1 (caddr formula) f-cache)))
    ((and or iff implies if not)
     `(,(car formula) ,@(mapcar #'(lambda (form) (declare (list form))
				    (instantiate-formula1 form f-cache))
				(cdr formula))))
    (t `(,(car formula)
	 ,@(mapcar #'(lambda (term) (declare (type (or symbol list) term))
		       (instantiate-term1 term f-cache))
		   (cdr formula))))))

(defun instantiate-term (term f-cache)
  (declare (type (or symbol list) term) (list f-cache))
  (if f-cache (instantiate-term1 term f-cache) term))

(defun instantiate-term1 (term f-cache)
  (if (consp term)
      (case (car term)
	((the the-class-of-all)
	 `(,(car term) (,(caadr term))
	   ,(instantiate-formula1 (caddr term) f-cache)))
	(t (cons (car term)
		 (mapcar #'(lambda (trm) (declare (type (or symbol list) trm))
			     (instantiate-term1 trm f-cache))
			 (cdr term)))))
    (or (cdr (assoc term f-cache)) term)))

(defun class-term-p (term)
  (and (consp term)
       (eq (car term) 'the-class-of-all)))

(defun obj-form-equal (obj1 obj2)
;;; FORMS-OF-THEOREM
  (declare (type formula obj1 obj2))
  (form-equal (formula obj1) (formula obj2)))

(defun form-equal (form1 form2)
  ;; this does not make anything external nor does it instantiate anything.
  ;; If that is desired, then it should be done before calling this.
  ;; Once we get rigid and universal variables:
  ;;this can be much improved
  (declare (list form1 form2))
  (when (eq (car form1) (car form2))
    (case (car form1)
      ((and or)
       (and (= (length form1) (length form2))
	    (subsetp (cdr form1) (cdr form2) :test #'form-equal)
	    (subsetp (cdr form2) (cdr form1) :test #'form-equal)))
      (iff (or (and (form-equal (cadr form1) (cadr form2))
		    (form-equal (caddr form1) (caddr form2)))
	       (and (form-equal (caddr form1) (cadr form2))
		    (form-equal (cadr form1) (caddr form2)))))
      ((not implies if) (every #'form-equal (cdr form1) (cdr form2)))
      ((forall for-some)
       (alpha-congruent-formulas (cdr form1) (cdr form2)))
      (= (or (and (term-equal (cadr form1) (cadr form2))
		  (term-equal (caddr form1) (caddr form2)))
	     (and (term-equal (caddr form1) (cadr form2))
		  (term-equal (cadr form1) (caddr form2)))))
      (t (every #'term-equal (cdr form1) (cdr form2))))))

(defun form-equal-spec (form1 form2)
;;; SEND-FORMULAS-TO-UNIFIER 
;;; ALPHA-CONGRUENT-TERMS-SPEC 
;;; ALPHA-CONGRUENT-FORMULAS-SPEC 
  ;; :clash means not unifiable without equality.
  ;; e.g. (g a) (f a)
  ;; :fail means maybe unifiable without equality.
  ;; e.g. (f a) (f (x a))
  ;; :succeed means alpha-congruent without equality.
  ;; e.g. (f a) (f a)
  (declare (list form1 form2))
  (if (eql (car form1) (car form2))
      (case (car form1)
	((and or iff not implies if)
	 (let ((val :succeed))
	   (do ((f1 (cdr form1) (cdr f1)) (f2 (cdr form2) (cdr f2)))
               ((null f1) val)
             (declare (list f1 f2))
             (case (form-equal-spec (car f1) (car f2))
               (:clash (return :clash)) (:fail (setq val :fail))))))
        ((forall for-some)
         (alpha-congruent-formulas-spec (cdr form1) (cdr form2)))
        (t (let ((val :succeed))
             (declare (symbol val))
             (do ((f1 (cdr form1) (cdr f1)) (f2 (cdr form2) (cdr f2)))
                 ((null f1) val)
               (declare (list f1 f2))
               (case (term-equal-spec (car f1) (car f2))
                 (:clash (return :clash)) (:fail (setq val :fail)))))))
    :clash))

(defun term-equal (term1 term2)
;;; PATHS-TO-OCCURRENCES-OF-TERM1 
;;; TERM-EQUAL 
;;; MAKE-SIMPLE-ALIST 
  ;; this does not make anything external nor does it instantiate anything.
  ;; If that is desired, then it should be done before calling this.
  (declare (type (or symbol list) term1 term2))
  (if (consp term1)
      (and (consp term2)
           (eq (car term1) (car term2))
           (case (car term1)
             ((the the-class-of-all)
              (alpha-congruent-terms (cdr term1) (cdr term2)))
             (t (every #'term-equal (cdr term1) (cdr term2)))))
    (and (not (consp term2)) (eq term1 term2))))

(defun term-equal-spec (term1 term2)
;;; UNIFY-TERMS 
;;; FORM-EQUAL-SPEC 
  ;; this does not make anything external nor does it instantiate anything.
  ;; If that is desired, then it should be done before calling this.
  (declare (type (or symbol list) term1 term2))
  (if (consp term1)
      (if (consp term2)
          (if (eq (car term1) (car term2))
;;;           (if (and *rigid-only* (skolem (car term1)))
;;;               :succeed)
              (case (car term1)
                ((the the-class-of-all)
                 (alpha-congruent-terms-spec (cdr term1) (cdr term2)))
                (t (let ((val :succeed))
                     (declare (symbol val))
                     (do ((t1 (cdr term1) (cdr t1))
                          (t2 (cdr term2) (cdr t2))) ((null t1) val)
                       (declare (list t1 t2))
                       (case (term-equal-spec (car t1) (car t2))
                         (:clash (return :clash))
                	 (:fail (setq val :fail)))))))
            :clash)
        :fail)
    (if (consp term2) :fail (if (eq term1 term2) :succeed :fail))))

;; I want this to ignore bound variables intelligently
;; I.e. find the first match then subst with this match and
;; try to match the next var.
;; This might ought to take the whole formula and compound foralls together
(defun alpha-congruent-formulas (exp1 exp2)
;;; FORM-EQUAL
  (declare (list exp1 exp2))
  ;; exps are of the form '(((v1) (v2) (v3) ... (vn)) (pred t1 t2 .. tm))
  (when (= (length (car exp1)) (length (car exp2)))
    ;; we don't need (list (gentemp)) below because symbols can't cause probs
    (let* ((alist1 (mapcar #'(lambda (var1)
			       (declare (symbol var1))
			       (cons var1 (gentemp)))
			   (mapcar #'car (car exp1))))
	   (incnum 0)
	   (alist2 (mapcar #'(lambda (var2)
			       (declare (symbol var2))
			       (prog1 (cons var2 (cdr (nth incnum alist1)))
				 (incf incnum))) (mapcar #'car (car exp2)))))
      (declare (list alist1 alist2) (type (integer 0) incnum))
      (form-equal (instantiate-formula (cadr exp1) alist1)
		  (instantiate-formula (cadr exp2) alist2)))))

(defun alpha-congruent-formulas-spec (exp1 exp2)
;;; FORM-EQUAL-SPEC
  ;; exps are of the form '(((v1) (v2) (v3) ... (vn)) (pred t1 t2 .. tm))
  (declare (list exp1 exp2))
  (if (= (length (car exp1)) (length (car exp2)))
      (let* ((alist1 (mapcar #'(lambda (var1)
				 (declare (symbol var1))
				 (cons var1 (list (gentemp))))
			     (mapcar #'car (car exp1))))
	     (incnum 0)
	     (alist2 (mapcar #'(lambda (var2)
				 (declare (symbol var2))
				 (prog1 (cons var2 (cdr (nth incnum alist1)))
				   (incf incnum))) (mapcar #'car (car exp2)))))
	(declare (list alist1 alist2) (type (integer 0) incnum))
	(form-equal-spec
	 (instantiate-formula (cadr exp1) alist1)
	 (instantiate-formula (cadr exp2) alist2)))
    :clash))

(defun alpha-congruent-terms (exp1 exp2)
;;; FIND-EQUIV-CLASS 
;;; FIND-EQUIV-THE 
;;; TERM-EQUAL 
  (declare (list exp1 exp2))
  ;; exps are of the form '((v) (pred t1 t2 .. tm))
  (let* ((alist1 (list (cons (caar exp1) (gentemp))))
	 (alist2 (list (cons (caar exp2) (cdar alist1)))))
    (declare (list alist1 alist2))
    (form-equal (instantiate-formula (cadr exp1) alist1)
		(instantiate-formula (cadr exp2) alist2))))

(defun alpha-congruent-terms-spec (exp1 exp2)
;;; TERM-EQUAL-SPEC
  ;; exps are of the form '((v) (pred t1 t2 .. tm))
  (declare (list exp1 exp2))
  (let* ((alist1 (list (cons (caar exp1) (list (gentemp)))))
	 ;; in the spec case, we replace bound variables with singletons
	 ;; so that they cannot be instantiated and the clash checking
	 ;; will work.
	 (alist2 (list (cons (caar exp2) (cdar alist1)))))
    (declare (list alist1 alist2))
    (form-equal-spec 
     (instantiate-formula (cadr exp1) alist1)
     (instantiate-formula (cadr exp2) alist2))))

(defun externalize-formula (formula)
  ;; this ignores forces
  (declare (list formula))
  (case (car formula)
    ((forall for-some)
     `(,(car formula)
       ,(mapcar #'(lambda (v) (declare (list v)) (list (ext-name (car v))))
		(cadr formula))
       ,(externalize-formula (caddr formula))))
    ((and or iff not implies if)
     `(,(car formula) ,@(mapcar #'externalize-formula (cdr formula))))
    (t `(,(car formula) ,@(mapcar #'externalize-term (cdr formula))))))

(defun externalize-term (term)
  ;; this ignores forces
  ;; I may want to allow it to follow 1 force or 2 at the user's discretion
  (declare (type (or symbol list) term))
  (if (consp term)
      (case (car term)
	((the the-class-of-all)
	 `(,(car term)
	   (,(ext-name (caadr term)))
	   ,(externalize-formula (caddr term))))
	(t `(,(car term) ,@(mapcar #'externalize-term (cdr term)))))
    (ext-name term)))

(defun externalize-f-cache (f-cache &optional for-real)
  (if for-real
      (mapcar #'(lambda (pair)
		  `(,(ext-name (car pair)) ,@(externalize-term (cdr pair))))
	      f-cache)
    (mapcar #'(lambda (pair)
		`(,(ext-name (car pair)) ,(externalize-term (cdr pair))))
	    f-cache)))

(defun check-formula (formula &aux alist)
  ;; initialization. First, make-formula-internal: check a little
  ;; syntax and replace bound variables and collect unbound variables
  ;; into *unbound-vars* and install terms and preds. defined in kb
  ;; for use.  Second, check names of unbound vars and replace them
  ;; with skolem constants.  Return the new formula.
  (declare (list formula alist))
  (setq formula (make-formula-internal formula))
  (if (setq *unbound-vars* (delete-duplicates *unbound-vars*))
      (progn
	(warn-user (format nil "~%Unbound variables: ~s" *unbound-vars*))
	(instantiate-formula
	 formula
	 (dolist (var *unbound-vars* alist)
	   (declare (symbol var))
	   (if (char= (char (symbol-name var) 0) #\_)
	       ;; see make-bound-var for more restrictions above.
	       (ipr-error
		"~%Unbound variables are not allowed to begin with ~
`_': ~s" var)
	     (push (cons var (list (make-skolem-function-symbol
				    :ext-name (concat-symbols '_ var))))
		   alist))))) formula))

;;; These functions are only called by check-formula which is only
;;; called when the proving process begins.

(defun make-formula-internal (form)
  (declare (list form))
  (setq form (make-formula-internal1 form nil))
;;;  (dolist (term-const *unknown-terms*)
;;;    (declare (symbol term-const))
;;;    (let ((mem (member-if #'(lambda (e) (declare (symbol e))
;;;				    (eq (kb-term-name e) term-const))
;;;			  *kb-terms*)))
;;;      (when mem (install-term-for-proving (car mem)))))
;;;  (dolist (pred-const *unknown-predicates*)
;;;    (declare (symbol pred-const))
;;;    (let ((mem (member-if #'(lambda (e)
;;;			      (eq (kb-predicate-name e) pred-const))
;;;			  *kb-predicates*)))
;;;      (declare (list mem))
;;;      (when mem (install-predicate-for-proving (car mem)))))
  (when *undefined-terms*
    (warn-user (format nil "~%The following terms are undefined:~%~S"
		       *undefined-terms*)))
  (when *undefined-predicates*
    (warn-user (format nil "~%The following predicates are undefined:~%~S"
		       *undefined-predicates*)))
  form)

(defun make-formula-internal1 (formula bound-vars &aux car alist)
  ;; bound-vars is the INTERNAL NAMES of variables (e.g. *var1)
  ;; in the future this may also count arguments and check to see if
  ;; everything is defined.
  (declare (list formula bound-vars alist) (symbol car))
  (if (consp formula)
      (case (setq car (car formula))
	((and or not iff implies if)
	 (case car
	   ((implies iff)
	    (when (not (= (length formula) 3))
	      (error (format nil "Badly formed formula: ~s" formula))))
	   ((and or)
	    (when (not (cdr formula))
	      (error "Badly formed fomrula: ~s" formula)))
	   (if (when (not (= (length formula) 4))
		 (error "Badly formed formula: ~s" formula)))
	   (not (when (not (= (length formula) 2))
		  (error (format nil "Badly formed formula: ~s" formula)))))
	 `(,car
	   ,@(mapcar #'(lambda (form) (declare (list form))
			 (make-formula-internal1 form bound-vars))
		     (cdr formula))))
	((for-some forall)
	 `(,car ,(mapcar	; this is the new cadr
		  #'(lambda (var) (declare (list var))
		      (if (symbolp (car var))
			  (let ((new-var (make-bound-var (car var))))
			    (push new-var bound-vars)
			    (push (cons (car var) new-var) alist)
			    (list new-var))
			(ipr-error "~%badly formed theorem: ~s" formula)))
		  (or (cadr formula)
		      (error "No variables.  ~a" formula)))
		,(make-formula-internal1
		  (instantiate-formula (caddr formula) alist) bound-vars)))
	(t (when (not (eq car '=))
	     (pushnew car *unknown-predicates*)
	     (when (not (member-if #'(lambda (e) (eq (kb-pred-name e) car))
				   *kb-predicates*))
	       (pushnew car *undefined-predicates*))
	     (pushnew car *involved-predicates*))
	   `(,car
	     ,@(mapcar #'(lambda (term) (declare (type (or symbol list)))
				 (make-term-internal term bound-vars))
		       (cdr formula)))))
    (progn (warn-user "Propositions should be treated as predicates with no ~
arguments: ~s" formula)
	   (list formula))))

(defun make-term-internal (term bound-vars) ; bound-vars is the internal names
  (declare (type (or symbol list) term) (list bound-vars))
  (if (consp term)
      (case (car term)
	((the-class-of-all the)
	 (when (eq (car term) 'the) (setq *thes* t))
	 (let* ((var (caadr term))
		(new-var 
		 (if (symbolp var)
		     (make-bound-var var)
		   (error "I can't deal with that yet: ~s" term)))
		(cadr (progn (push new-var bound-vars) (list new-var)))
		(caddr (replace-unbound-occurrences-in-formula
			new-var var (caddr term))))
	   (declare (symbol var new-var) (list cadr caddr))
	   `(,(car term) ,cadr ,(make-formula-internal1 caddr bound-vars))))
	(t
	 (pushnew (car term) *unknown-terms*)
	 (when (not (member-if #'(lambda (e) (declare (symbol e))
					 (eq (kb-term-name e) (car term)))
			       *kb-terms*))
	   (pushnew (car term) *undefined-terms*))
	 (pushnew (car term) *involved-terms*)
	 `(,(car term) ,@(mapcar #'(lambda (trm)
				     (declare (type (or symbol list) trm))
				     (make-term-internal trm bound-vars))
				 (cdr term)))))
    (progn
      (when (not (member term bound-vars))
	(push term *unbound-vars*))
      term)))

;;; Next, I need to work on the unifier for theorems and definitions.
;;; I need to work out how I am going to handle free and bound variables
;;; in theorems and definitions.

(defun check-syntax (type &rest args)
;;; DEF-TERM-FUN 
;;; DEF-PREDICATE-FUN 
  ;; this will check that variables are of the right form, and that
  ;; everything makes sense.  variables stored in theorems and
  ;; definitions may not be of the form *param[N] or *var[N]
  (declare (ignore type args))
;;;  (case type
;;;    (:pred
;;;     (let ((pred (car args))
;;;	   (pattern (cadr args))
;;;	   (definition (caddr args))
;;;	   (body (cadddr args)))
;;;       ())))
  t)

