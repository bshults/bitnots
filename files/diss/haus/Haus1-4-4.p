I intend to translate this to TPTP form.

(setq *format-only* t)

(def-predicate (a-hausdorff-top-space x)
  (forall ((a) (b))
    (implies (and (a-member-of a (coerce-to-class x))
		  (a-member-of b (coerce-to-class x))
		  (not (= a b)))
      (for-some ((g1) (g2))
	(and (open-in g1 x) (open-in g2 x)
	     (a-member-of a g1) (a-member-of b g2)
	     (disjoint g1 g2)))))
  (string "the definition of a Hausdorff space")
  (format "~a is a Hausdorff space"))

(def-term (the-diagonal-top s)
  ()
  (format "the diagonal of ~a"))

(def-predicate (closed-in a b)
  (open-in (the-set-difference-of b a) b)
  (format "~a is a closed subset of ~a"))

(def-term (the-graph-of f)
  ()
  (format "the graph of ~a"))

(def-term (the-domain-of f)
  ()
  (format "the domain of ~a"))

(def-term (the-product-top-space-of a b)
  ()
  (format "the product topological space of ~a and ~a"))

(def-term (the-product-of a b)
  ()
  (format "the cartesian product of ~a and ~a"))

(def-term (apply a b)
  ()
  (format "{~a}(~a)"))

(def-term (the-ordered-pair a b)
  ()
  (format "<~a, ~a>"))

(def-term (coerce-to-class x)
  ()
  (format "the class associated with ~a")
  )

(def-predicate (open-in a x)
  ()
  (format "~a is open in ~a")
  )

(def-predicate (a-function-from-to f x y)
  ()
  (format "~a is a function from ~a to ~a")
  )

(def-term (the-set-inverse-image-of f a)
  ()
  (format "{~a}^{-1}(~a)")
  )

(setq *format-only* nil)

(def-theorem closed-subset-thm
  (implies (forall ((y)) (implies (and (a-member-of y (coerce-to-class x))
				       (not (a-member-of y a)))
			   (for-some ((g))
			     (and (a-member-of y g)
				  (open-in g x)
				  (disjoint g a)))))
    (closed-in a x))
  (string "a simple theorem about closed sets"))

(def-theorem hausdorff
  (implies (a-hausdorff-top-space x)
    (forall ((a) (b))
      (implies (and (a-member-of a (coerce-to-class x))
		    (a-member-of b (coerce-to-class x))
		    (not (= a b)))
	(for-some ((g1) (g2))
	  (and (open-in g1 x) (open-in g2 x)
	       (a-member-of a g1) (a-member-of b g2)
	       (disjoint g1 g2))))))
  (string "the definition of a Hausdorff space"))

(def-theorem product-of-open-sets
  (implies (and (open-in a x) (open-in b y))
    (open-in (the-product-of a b) (the-product-top-space-of x y)))
  (string "a basic fact about the product topology"))

(def-theorem product-top		; a definition
  (implies (a-member-of x (coerce-to-class (the-product-top-space-of s t)))
    (for-some ((a) (b))
      (and (a-member-of a (coerce-to-class s))
	   (a-member-of b (coerce-to-class t))
	   (= x (the-ordered-pair a b)))))
  (string "a basic fact about product topologies"))

(def-theorem product			; a definition
  (iff (a-member-of x (the-product-of s t))
       (for-some ((a) (b))
	 (and (a-member-of a s) (a-member-of b t)
	      (= x (the-ordered-pair a b)))))
  (string "a basic fact about products"))

(def-predicate (disjoint a b)
  (not (for-some ((y)) (and (a-member-of y a) (a-member-of y b))))
  (string "the definition of disjoint")
  (format "~a and ~a are disjoint"))

(def-theorem ordered-pair
  (implies (= (the-ordered-pair a b) (the-ordered-pair c d))
    (and (= a c) (= b d)))
  (string "a basic fact about ordered pairs"))

(def-theorem diagonal-top
  (iff (a-member-of x (coerce-to-class (the-diagonal-top s)))
       (for-some ((a))
	 (and (a-member-of a (coerce-to-class s))
	      (= x (the-ordered-pair a a)))))
  (string "the definition of the diagonal"))

;;; from Abraham, Marsden, Raitu
;;; Manifolds, Tensor Analysis and Applications
(def-theorem challenge-AMR-1-4-4
  (implies (a-hausdorff-top-space S)
    (closed-in (coerce-to-class (the-diagonal-top S))
	       (the-product-top-space-of S S)))
  (string "Proposition 1.4.4 in A/M/R"))

