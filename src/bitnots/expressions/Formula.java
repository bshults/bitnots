package bitnots.expressions;

import bitnots.equality.*;
import java.util.*;
import java.io.*;

/**
 * <p>A Formula is either a logical formula, or a predicate.  I.e., it
 * represents a sentence that may be true or false.
 *
 * @author Benjamin Shults
 * @version .2
 */

public abstract class Formula implements Serializable {

  protected FormulaConstructor constructor;

  @Override
  public abstract int hashCode();
  @Override
  public abstract boolean equals(Object o);
  
  // TODO add a "to natural language string" method.

  /**
   * Checks if the two Formulae are equal if the given Substitution is
   * applied to both.
   * @param f The Formula that this object may be equal to...
   * @param s ... if this Substitution is applied to both.
   * @return true if they are equal, given the Substitution.
   */
  public abstract boolean equalsUnderSub(Formula f, Substitution s);

  /**
   * @return if <b>ax</b> and <b>at</b> match except that there is a
   * term <b>t</b> that occurs in <b>at</b> everywhere <b>x</b>
   * occurs in <b>ax</b>, then this returns <b>t</b>.  Otherwise,
   * null. */
  public static Term disagreement(Formula ax, Variable x, Formula at) {
    Substitution s = ax.unifyApplied(at, Substitution.createSubstitution());
    // if the resulting substitution is a singleton, then return the
    // rhs of that singleton.
    if (s.size() == 1) {
      Map.Entry pair = (Map.Entry) s.iterator().next();
      if (pair.getKey() == x)
        return (Term) pair.getKey();
      else if (pair.getValue() == x)
        return (Term) pair.getValue();
    }
    return null;
  }

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
   * Returns a new Formula that is identical to the reciever except
   * that all sequent variables in the receiver have been replaced
   * with new free tableau variables.
   * @param map maps a sequent variable to the new free tableau
   * variable that the sequent variable is being replaced with.
   */
  public abstract Formula replaceVariables(Map map);

  /**
   * Try to unify them under <b>s</b>.  Return a most general,
   * idempotent unifier for the receiver and the first argument if
   * there is one.  This unifier will have been compatibly composed
   * with <b>s</b>.  Otherwise, return null.
   * @param f the formula to be unified with the receiver.
   * @param s a substitution that is to be applied to both formulas
   * before unification and to be compatibly composed with the result.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly compatibly composed with <b>s</b>
   * if there is one.
   */
  public abstract Substitution unify(Formula f, Substitution s);
  public abstract Substitution unify(Formula f, Substitution s, DSTGraph cc);

  /**
   * Try to unify them under <b>s</b> and <b>bvs</b>.  Return a most
   * general, idempotent unifier for the receiver and the first
   * argument if there is one.  This unifier will have been compatibly
   * composed with <b>s</b>.  Otherwise, return null.
   * @param f the formula to be unified with the receiver.
   * @param s a substitution that is to be applied to both formulas
   * before unification and to be compatibly composed with the result.
   * @param bvs a substitution of bound variables to constants.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly compatibly composed with <b>s</b>
   * if there is one.
   */
  public abstract Substitution unify(Formula f, Substitution s,
                                     Substitution bvs);
  public abstract Substitution unify(Formula f, Substitution s,
                                     Substitution bvs, DSTGraph cc);

  /**
   * Try to unify them.  Return a most general, idempotent unifier for
   * the receiver and the first argument if there is one.  This
   * unifier will have been compatibly composed with <b>s</b>.
   * Otherwise, return null.  The caller guarantees
   * that <b>s</b> has been applied to the receiver.
   * @param f the formula to be unified with the receiver.
   * @param s a substitution that has been applied to the receiver.
   * It will be compatibly composed with the result.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is one.
   */
  public abstract Substitution unifyRecApplied(Formula f, Substitution s);
  /**
   * Try to unify them.  Return a most general, idempotent unifier for
   * the receiver and the first argument if there is one.  This
   * unifier will have been compatibly composed with <b>s</b>.
   * Otherwise, return null.  The caller guarantees
   * that <b>s</b> has been applied to the receiver.
   * @param f the formula to be unified with the receiver.
   * @param s a substitution that has been applied to the receiver.
   * It will be compatibly composed with the result.
   * @param cc equalities on the path to the root (or from the KB?)
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is one.
   */
  public abstract Substitution unifyRecApplied(Formula f, Substitution s,
                                               DSTGraph cc);

  /**
   * Try to unify them.  Return a most general, idempotent unifier for
   * the receiver and the first argument if there is one.  This
   * unifier will have been compatibly composed with <b>s</b>.
   * Otherwise, return null.  The caller guarantees
   * that <b>s</b> has been applied to both.
   * @param f the formula to be unified with the receiver.
   * @param s a substitution that has been applied to both.  It will
   * be compatibly composed with the result.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is one.
   */
  public abstract Substitution unifyApplied(Formula f, Substitution s);

  public abstract Substitution unifyApplied(Formula f, Substitution s,
                                            DSTGraph cc);

//    /**
//     * Apply <b>s</b> to both the receiver and the first argument, then
//     * try to unify them.  Return a most general, idempotent unifier for
//     * the receiver and the first argument if there is one.  This
//     * unifier will have been compatibly composed with <b>s</b>.
//     * Otherwise, return null.
//     * @param f the formula to be unified with the receiver.
//     * @param s a substitution that is to be applied to both formulas
//     * before unification and to be compatibly composed with the result.
//     * @return a most general, idempotent unifier for the receiver and
//     * the first argument compatibly composed with <b>s</b> if there is one.
//     */
//    abstract Substitution unify(Formula f, Substitution s,
//                                Substitution bvs);

  /**
   * Apply <b>s</b> to both the receiver and the first argument, then
   * try to unify them.  Return a most general, idempotent unifier for
   * the receiver and the first argument if there is one.  This
   * unifier will have been compatibly composed with <b>s</b>.
   * Otherwise, return null.  The caller guarantees
   * that <b>s</b> has been applied to the receiver.
   * @param f the formula to be unified with the receiver.
   * @param s a substitution that has been applied to the receiver.
   * It will be compatibly composed with the result.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is one.
   */
  public abstract Substitution unifyRecApplied(Formula f, Substitution s,
                                               Substitution bvs);

  public abstract Substitution unifyRecApplied(Formula f, Substitution s,
                                               Substitution bvs, DSTGraph cc);

  /**
   * Apply <b>s</b> to both the receiver and the first argument, then
   * try to unify them.  Return a most general, idempotent unifier for
   * the receiver and the first argument if there is one.  This
   * unifier will have been compatibly composed with <b>s</b>.
   * Otherwise, return null.  The caller guarantees
   * that <b>s</b> has been applied to both.
   * @param f the formula to be unified with the receiver.
   * @param s a substitution that has been applied to both.  It will
   * be compatibly composed with the result.
   * @return a most general, idempotent unifier for the receiver and
   * the first argument compatibly composed with <b>s</b> if there is one.
   */
  public abstract Substitution unifyApplied(Formula f, Substitution s,
                                            Substitution bvs);

  public abstract Substitution unifyApplied(Formula f, Substitution s,
                                            Substitution bvs, DSTGraph cc);

  /**
   * A quick check to see if formulas are unifiable.  Returns
   * Substitution.ALPHA, Substitution.DIFFERENCE, or
   * Substitution.CLASH as appropriate.
   * @param f the formula with which the receiver is compared.
   * @return
   * <ul>
   *   <li><b>Substitution.CLASH</b> if there is a clash.
   *   <li><b>Substitution.DIFFERENCE</b> if there is no
   *       clash but the two are not alpha-congruent.
   *   <li><b>Substitution.ALPHA</b> if the two are
   *       alpha-congruent.
   * </ul>
   */
  abstract int failFast(Formula f);

  /**
   * Return <code>true</code> if the receiver and the argument are
   * equal up to renaming of bound variables.
   * @param f the formula to be compared with the receiver.
   * @return <code>true</code> if the receiver and the argument are
   * equal up to renaming of bound variables.
   */
  public boolean alphaCongruent(Formula f) {
    // TODO: this could be made faster by laying it off to the child
    // classes.
    return this.failFast(f) == Substitution.ALPHA;
  }

  /**
   * This returns the the result of applying <b>s</b> to the receiver.
   * The result may share structure with or be identical to the
   * receiver and/or other terms but the receiver will <b>not</b>
   * <b>be</b> <b>modified</b>.
   * @param s the Substitution to be applied to the receiver.
   * @return the result of applying <b>s</b> to the receiver.
   */
  public abstract Formula apply(Substitution s);

  /**
   * Returns the result of replacing every free occurrence of <b>v</b>
   * in the receiver with <b>t</b>.  This does not modify the receiver.
   * @param v the target variable to be replaced.
   * @param t the term with which <b>v</b> will be replaced.
   * @return the result of replacing every free occurrence of <b>v</b>
   * in the receiver with <b>t</b>.
   */
  public abstract Formula replaceUnboundOccurrencesWith(Variable v,
                                                        Term t);

  /**
   * Return true if and only if the receiver contains the term.  Every
   * free variable in <b>t</b> must be free in the place in the
   * receiver where it contains <b>t</b>.
   * @param t the term sought.
   * @return true if and only if the receiver contains the term.
   */
  // TODO: add versions of this that take Substitutions.
  public abstract boolean contains(Term t);

  /**
   * Return true if and only if the receiver contains the formula.
   * Every free variable in <b>f</b> must be free in the place in the
   * receiver where it contains <b>f</b>.
   * @param f the formula sought.
   * @return true if and only if the receiver contains the formula.
   */
  // TODO: add versions of this that take Substitutions.
  public abstract boolean contains(Formula f);

  public FormulaConstructor getConstructor() {
    return this.constructor;
  }

  //  public Object clone();

  public String toString() {
    return "(" + this.getConstructor() + ")";
  }

  protected Formula(FormulaConstructor s) {
    this.constructor = s;
  }

} // Formula
