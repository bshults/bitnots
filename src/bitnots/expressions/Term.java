package bitnots.expressions;

import java.util.*;
import bitnots.equality.*;

/**
 * A term in the logical language.  You cannot extend this class
 * directly.  Instead, extend either Variable, BoundVariable, or
 * ComplexTerm.  A term is either a bound variable, a variable or a
 * complex term.  Functions and constants are complex terms as are
 * other terms such as class constructors.
 * 
 * @author Benjamin Shults
 * @version .2
 */

// TODO TAG efficient-unification
// TODO efficient-unification Step 1.  Write a battery of unification tests
// TODO efficient-unification Step 2.  Time the current algorithm.
// TODO efficient-unification Step 3.  Implement a DAG structure for terms and formulas.
// TODO efficient-unification Step 4.  Implement Corbin and Bidoit algorithm.
// TODO efficient-unification Step 5.  Implement Baader and Siekmann's Substitutions.
// TODO efficient-unification Step 6.  Implement Baader and Snyder's almost-linear algorithm
// TODO efficient-unification Step 7.  Check out Champeaux 1986.
// TODO efficient-unification Step 8.  Time all these and compare.

public abstract class Term implements java.io.Serializable {
  
  public abstract int hashCode();
  public abstract boolean equals(Object o);

  /**
   * Return the set of free variables in the receiver.
   * @return the set of free variables in the receiver.
   */
  public abstract Set freeVars();

  /**
   * Return the set of free variables in the receiver excluding
   * variables in <b>notFree</b>.
   * @return the set of free variables in the receiver excluding
   * variables in <b>notFree</b>.  */
  public abstract Set freeVars(Collection notFree);

  /**
   * Returns a new Term that is identical to the reciever except that
   * all sequent variables in the receiver have been replaced with new
   * free tableau variables.
   * @param map maps a sequent variable to the new free tableau
   * variable that the sequent variable is being replaced with.
   */
  public abstract Term replaceVariables(Map map);

  /**
   * Try to unify <b>t</b> with the receiver if possbile.  Return a
   * most general, idempotent unifier for the receiver and the first
   * argument if there is one.  This unifier must be compatible with
   * and will have been composed with <b>s</b>.  Otherwise, return
   * null.
   * @param t the term to be unified with the receiver.
   * @param s a substitution that is to be applied to both terms
   * before unification and to be compatible and composed with the
   * result.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there
   * is one.
   */
  public abstract Substitution unify(Term t, Substitution s);
  public abstract Substitution unify(Term t, Substitution s, DSTGraph cc);

  /**
   * Try to unify <b>t</b> with the receiver if possbile.  Return a
   * most general, idempotent unifier for the receiver and the first
   * argument if there is one.  This unifier will have been compatibly
   * composed with <b>s</b>.  Otherwise, return null.
   * @param t the term to be unified with the receiver.
   * @param s a substitution that is to be applied to both terms
   * before unification and to be compatibly composed with the result.
   * @param bvsRec a substitution of bound variables to new constants
   * that is to be applied to the receiver.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is
   * one. */
  public abstract Substitution unify(Term t, Substitution s,
                                     Substitution bvsRec);
  public abstract Substitution unify(Term t, Substitution s,
                                     Substitution bvsRec, DSTGraph cc);

  /**
   * Try to unify <b>t</b> with the receiver if possbile.  Return a
   * most general, idempotent unifier for the receiver and the first
   * argument if there is one.  This unifier will have been compatibly
   * composed with <b>s</b>.  Otherwise, return null.  This assumes
   * that s has already been applied to the receiver.
   * @param t the term to be unified with the receiver.
   * @param s a substitution that is to be applied to both terms
   * before unification and to be compatibly composed with the result.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is one.
   */
  abstract Substitution unifyRecApplied(Term t, Substitution s);
  abstract Substitution unifyRecApplied(Term t, Substitution s, DSTGraph cc);

  /**
   * Try to unify <b>t</b> with the receiver if possbile.  Return a
   * most general, idempotent unifier for the receiver and the first
   * argument if there is one.  This unifier will have been compatibly
   * composed with <b>s</b>.  Otherwise, return null.  This assumes
   * that s has already been applied to the receiver.
   * @param t the term to be unified with the receiver.
   * @param s a substitution that is to be applied to both terms
   * before unification and to be compatibly composed with the result.
   * @param bvs a substitution of new constants for bound variables.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is one.
   */
  abstract Substitution unifyRecApplied(Term t, Substitution s,
                                        Substitution bvs);
  abstract Substitution unifyRecApplied(Term t, Substitution s,
                                        Substitution bvs, DSTGraph cc);

  /**
   * Try to unify <b>t</b> with the receiver if possbile.  Return a
   * most general, idempotent unifier for the receiver and the first
   * argument if there is one.  This unifier will have been compatibly
   * composed with <b>s</b>.  Otherwise, return null.  This assumes
   * that s has already been applied to the receiver and to the first
   * argument.
   * @param t the term to be unified with the receiver.
   * @param s a substitution that is has been applied to both terms
   * and is to be compatibly composed with the result.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is
   * one. */
  public abstract Substitution unifyApplied(Term t, Substitution s);
  public abstract Substitution unifyApplied(Term t, Substitution s,
                                            DSTGraph cc);

  /**
   * Try to unify <b>t</b> with the receiver if possbile.  Return a
   * most general, idempotent unifier for the receiver and the first
   * argument if there is one.  This unifier will have been compatibly
   * composed with <b>s</b>.  Otherwise, return null.  This assumes
   * that s has already been applied to the receiver and to the first
   * argument.
   * @param t the term to be unified with the receiver.
   * @param s a substitution that has been applied to both terms and
   * is to be compatibly composed with the result.
   * @param bvs a substitution of new constants for bound variables.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is one. */
  abstract Substitution unifyApplied(Term t, Substitution s,
                                     Substitution bvs);
  abstract Substitution unifyApplied(Term t, Substitution s,
                                     Substitution bvs, DSTGraph cc);

  /**
   * Return <code>true</code> if the receiver and the argument are
   * equal up to renaming of bound variables.
   * @param t the term to be compared with the receiver.
   * @return <code>true</code> if the receiver and the argument are
   * equal up to renaming of bound variables.
   */
  public boolean alphaCongruent(Term t) {
    // TODO: this could be made faster by laying it off to the child
    // classes.
    return this.failFast(t) == Substitution.ALPHA;
  }

  /**
   * A quick check to see if terms are unifiable.  Returns
   * Substitution.ALPHA, Substitution.DIFFERENCE, or
   * Substitution.CLASH as appropriate.
   * @param t the term with which the receiver is compared.
   * @return
   * <ul>
   *   <li><b>Substitution.CLASH</b> if there is a clash.
   *   <li><b>Substitution.DIFFERENCE</b> if there is no
   *       clash but the two are not alpha-congruent.
   *   <li><b>Substitution.ALPHA</b> if the two are
   *       alpha-congruent.
   * </ul>
   */
  public abstract int failFast(Term t);

  /**
   * This returns the the result of applying <b>s</b> to the receiver.
   * The result may share structure with or be identical to the
   * receiver and/or other terms but the receiver will <b>not</b>
   * <b>be</b> <b>modified</b>.
   * @param s the Substitution to be applied to the receiver.
   * @return the result of applying <b>s</b> to the receiver.
   */
  public abstract Term apply(Substitution s);

  /**
   * Returns the result of replacing every free occurrence of <b>v</b>
   * in the receiver with <b>t</b>.  This does not modify the
   * receiver.
   * @param v the target variable to be replaced.
   * @param t the term with which <b>v</b> will be replaced.
   * @return the result of replacing every free occurrence of <b>v</b>
   * in the receiver with <b>t</b>.
   */
  // TODO: see if this is called.
  public abstract Term replaceUnboundOccurrencesWith(Variable v, Term t);

  /**
   * Returns the result of replacing every occurrence of the constant
   * <b>old</b> in the receiver with the constant <b>newC</b>.  This
   * does not modify the receiver.
   * @param old the constant to be replaced.
   * @param newC the constant with which <b>old</b> will be replaced.
   * @return the result of replacing every occurrence of the constant
   * <b>old</b> in the receiver with the constant <b>newC</b>.  This
   * does not modify the receiver.
   */
  // TODO: see if this is called.
  public abstract Term replaceAllConstants(Function old, Function newC);

  /**
   * Return true if and only if the receiver contains the term.  Every
   * free variable in <b>t</b> must be free in the place in the
   * receiver where it contains <b>t</b>.
   * @param t the term sought.
   * @return true if and only if the receiver contains the term.
   */
  // TODO: add versions of this that take Substitutions because this
  // is rarely, if ever called except right after a call to apply.
  public abstract boolean contains(Term t);

  /**
   * Return true if and only if the receiver contains the formula.
   * Every free variable in <b>f</b> must be free in the place in the
   * receiver where it contains <b>f</b>.
   * @param f the formula sought.
   * @return true if and only if the receiver contains the formula.
   */
  // TODO: I'm interested in knowing if this ever gets called.
  // TODO: add versions of this that take Substitutions.
  public abstract boolean contains(Formula f);

  // This prevents classes, other than those included in this package,
  // from extending Term.
  Term() {}

} // Term

