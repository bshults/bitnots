package bitnots.expressions;

import bitnots.equality.*;
import java.util.*;

/**
 * @author Benjamin Shults
 * @version .2
 */

public class IotaTerm extends ComplexTerm {

  public static String IOTA_TERM_STRING = "the";
  public static TermConstructor IOTA_TERM_CONSTRUCTOR =
    (TermConstructor)
    Symbol.putOrGet(TermConstructor.class, IotaTerm.IOTA_TERM_STRING);

  public static final TermConstructor SYMBOL =
    (TermConstructor)
    Symbol.putOrGet(TermConstructor.class, IotaTerm.IOTA_TERM_STRING);

  /** The variable bound by the term. */
  private Variable bound;

  private Formula property;

  public Formula getBody() {
    return this.property;
  }

  public Variable getBoundVar() {
    return this.bound;
  }

  /**
   * Checks that the receiver and argument are in the same class, then
   * compares the arguments.
   */
  public boolean equals(Object o) {
    if (o instanceof IotaTerm) {
      Substitution sub = Substitution.createBoundVarSubstitution(
              Collections.singleton(this.getBoundVar()),
              Collections.singleton(((IotaTerm) o).getBoundVar()));
      if (sub != null)
        if (this.getBody().equalsUnderSub(((IotaTerm) o).getBody(), sub))
//        if (this.property.equals(((IotaTerm) o).property))
          return true;   // Alpha congruent
        else
          return false;  // Bodies differ
      else
        return false;  // sub was null
    } else
      return false;  // Classes differ
  }

  public int hashCode() {
    int value = 0;
    value = this.property.hashCode();
    return IotaTerm.class.hashCode() + value;
  }

  public Set freeVars() {
    //    return this.property.freeVars(Collections.singleton(this.bound));
    throw new UnsupportedOperationException();
  }

  public Set freeVars(Collection notFree) {
    throw new UnsupportedOperationException();
//      ArrayList pass = new ArrayList(notFree.size() + 1);
//      pass.addAll(notFree);
//      pass.add(this.bound);
//      return this.property.freeVars(pass);
  }

  public int failFast(Term t) {
    throw new UnsupportedOperationException();
  }

  public Term replaceAllConstants(Function old, Function newC) {
    // TODO: fix
    //    return this;
    throw new UnsupportedOperationException();
  }

  public Term replaceUnboundOccurrencesWith(Variable v, Term t) {
    throw new UnsupportedOperationException();
  }

  public Term replaceVariables(Map map) {
    throw new UnsupportedOperationException();
  }

  public Substitution unify(Term t, Substitution s) {
    throw new UnsupportedOperationException();
  }

  public Substitution unify(Term t, Substitution s, DSTGraph cc) {
    throw new UnsupportedOperationException();
  }

  Substitution unifyRecApplied(Term t, Substitution s) {
    throw new UnsupportedOperationException();
  }

  Substitution unifyRecApplied(Term t, Substitution s, DSTGraph cc) {
    throw new UnsupportedOperationException();
  }

  public Substitution unifyApplied(Term t, Substitution s) {
    throw new UnsupportedOperationException();
  }

  public Substitution unify(Term t, Substitution s, Substitution bvs) {
    throw new UnsupportedOperationException();
  }

  public Substitution unifyApplied(Term t, Substitution s, DSTGraph cc) {
    throw new UnsupportedOperationException();
  }

  public Substitution unify(Term t, Substitution s, Substitution bvs,
                            DSTGraph cc) {
    throw new UnsupportedOperationException();
  }

  Substitution unifyRecApplied(Term t, Substitution s,
                               Substitution bvs) {
    throw new UnsupportedOperationException();
  }

  Substitution unifyRecApplied(Term t, Substitution s,
                               Substitution bvs, DSTGraph cc) {
    throw new UnsupportedOperationException();
  }

  Substitution unifyApplied(Term t, Substitution s,
                            Substitution bvs) {
    throw new UnsupportedOperationException();
  }

  Substitution unifyApplied(Term t, Substitution s,
                            Substitution bvs, DSTGraph cc) {
    throw new UnsupportedOperationException();
  }

  public boolean contains(Term t) {
    // TODO: fix
    throw new UnsupportedOperationException();
  }

  public boolean contains(Formula f) {
    // TODO: fix
    throw new UnsupportedOperationException();
  }

  public Term apply(Substitution s) {
    // TODO: fix
    throw new UnsupportedOperationException();
  }

  /** Construct a ClassTerm with the given bound variable and property. */
  public IotaTerm(Variable v, Formula p) {
    // TODO: fix
    super(IotaTerm.IOTA_TERM_CONSTRUCTOR);
    this.bound = v;
    this.property = p;
  }

} // IotaTerm
