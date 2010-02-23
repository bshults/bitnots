package bitnots.expressions;

import bitnots.equality.*;
import java.util.*;

/**
 * A term that represents a class.
 *
 * @author Benjamin Shults
 * @version
 */

public class ClassTerm extends ComplexTerm {

  public static String CLASS_TERM_STRING = "the-class-of-all";
  public static TermConstructor CLASS_TERM_CONSTRUCTOR =
    (TermConstructor)
    Symbol.putOrGet(TermConstructor.class, ClassTerm.CLASS_TERM_STRING);

  public static final TermConstructor SYMBOL =
    (TermConstructor)
    Symbol.putOrGet(TermConstructor.class, ClassTerm.CLASS_TERM_STRING);

  /** The variable bound by the term. */
  private Variable bound;
  /** The formula whose extent describes the class. */
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
    if (o instanceof ClassTerm) {
      Substitution sub = Substitution.createBoundVarSubstitution(
              Collections.singleton(this.getBoundVar()),
              Collections.singleton(((ClassTerm) o).getBoundVar()));
      if (sub != null)
        if (this.getBody().equalsUnderSub(((ClassTerm) o).getBody(), sub))
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
    return ClassTerm.class.hashCode() + value;
  }

  /**
   * This is a shallow copy.
   */
  public ClassTerm clone() {
    return (ClassTerm) super.clone(); // new ClassTerm(this.getConstructor(), this.bound, this.property);
  }

  public Set freeVars() {
    return this.property.freeVars(Collections.singleton(this.bound));
  }

  public Set freeVars(Collection notFree) {
    ArrayList pass = new ArrayList(notFree.size() + 1);
    pass.addAll(notFree);
    pass.add(this.bound);
    return this.property.freeVars(pass);
  }

  public int failFast(Term t) {
    if (t instanceof ClassTerm)
      return Substitution.DIFFERENCE;
    else
      return Substitution.CLASH;
  }

  public Term replaceUnboundOccurrencesWith(Variable v, Term t) {
    return new ClassTerm(this.getBoundVar(),
                         this.getBody().replaceUnboundOccurrencesWith(v, t));
  }

  /**
   * Assuming that this is only called from the congruence closure
   * code... I don't have to implement this since the congruence
   * closure code essentially ignores the structure of class terms.
   */
  public Term replaceAllConstants(Function old, Function newC) {
    return this;
  }

  public boolean contains(Term t) {
    if (this.equals(t))
      return true;
    else
      return this.getBody().contains(t);
  }

  public boolean contains(Formula f) {
    return this.getBody().contains(f);
  }

  public Term replaceVariables(Map map) {
    return new ClassTerm(this.bound,
                         this.property.replaceVariables(map));
  }

  public Substitution unify(Term t, Substitution s) {
    if (t instanceof Variable)
      return t.apply(s).unifyRecApplied(this, s);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.bound, c.bound);
      return this.property.unify(c.property, s, bvs);
    } else
      return null;
  }

  public Substitution unify(Term t, Substitution s, DSTGraph cc) {
    if (t instanceof Variable)
      return t.apply(s).unifyRecApplied(this, s, cc);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.bound, c.bound);
      return this.property.unify(c.property, s, bvs, cc);
    } else
      return null;
  }

  Substitution unifyRecApplied(Term t, Substitution s) {
    if (t instanceof Variable)
      return t.apply(s).unifyApplied(this, s);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.bound, c.bound);
      return this.property.unifyRecApplied(c.property, s, bvs);
    } else
      return null;
  }

  Substitution unifyRecApplied(Term t, Substitution s, DSTGraph cc) {
    if (t instanceof Variable)
      return t.apply(s).unifyApplied(this, s, cc);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.bound, c.bound);
      return this.property.unifyRecApplied(c.property, s, bvs, cc);
    } else
      return null;
  }

  public Substitution unifyApplied(Term t, Substitution s) {
    if (t instanceof Variable)
      return t.unifyApplied(this, s);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.bound, c.bound);
      return this.property.unifyApplied(c.property, s, bvs);
    } else
      return null;
  }

  public Substitution unify(Term t, Substitution s, Substitution bvs) {
    if (t instanceof Variable)
      return t.apply(s).unifyRecApplied(this, s, bvs);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvsNew =
        Substitution.createBoundVarSubstitution(this.bound, c.bound, bvs);
      return this.property.unify(c.property, s, bvsNew);
    } else
      return null;
  }

  public Substitution unifyApplied(Term t, Substitution s, DSTGraph cc) {
    if (t instanceof Variable)
      return t.unifyApplied(this, s, cc);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.bound, c.bound);
      return this.property.unifyApplied(c.property, s, bvs, cc);
    } else
      return null;
  }

  public Substitution unify(Term t, Substitution s, Substitution bvs,
                            DSTGraph cc) {
    if (t instanceof Variable)
      return t.apply(s).unifyRecApplied(this, s, bvs, cc);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvsNew =
        Substitution.createBoundVarSubstitution(this.bound, c.bound, bvs);
      return this.property.unify(c.property, s, bvsNew, cc);
    } else
      return null;
  }

  Substitution unifyRecApplied(Term t, Substitution s, Substitution bvs) {
    if (t instanceof Variable)
      return t.apply(s).unifyApplied(this, s, bvs);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvsNew =
        Substitution.createBoundVarSubstitution(this.bound, c.bound, bvs);
      return this.property.unifyRecApplied(c.property, s, bvsNew);
    } else
      return null;
  }

  Substitution unifyRecApplied(Term t, Substitution s, Substitution bvs,
                               DSTGraph cc) {
    if (t instanceof Variable)
      return t.apply(s).unifyApplied(this, s, bvs, cc);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvsNew =
        Substitution.createBoundVarSubstitution(this.bound, c.bound, bvs);
      return this.property.unifyRecApplied(c.property, s, bvsNew, cc);
    } else
      return null;
  }

  Substitution unifyApplied(Term t, Substitution s, Substitution bvs) {
    if (t instanceof Variable) {
      return t.unifyApplied(this, s, bvs);
    } else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvsNew =
        Substitution.createBoundVarSubstitution(this.bound, c.bound, bvs);
      return this.property.unifyApplied(c.property, s, bvsNew);
    } else
      return null;
  }

  Substitution unifyApplied(Term t, Substitution s, Substitution bvs,
                            DSTGraph cc) {
    if (t instanceof Variable)
      return t.unifyApplied(this, s, bvs, cc);
    else if (t instanceof ClassTerm) {
      ClassTerm c = (ClassTerm) t;
      Substitution bvsNew =
        Substitution.createBoundVarSubstitution(this.bound, c.bound, bvs);
      return this.property.unifyApplied(c.property, s, bvsNew, cc);
    } else
      return null;
  }

  public Term apply(Substitution s) {
    return new ClassTerm(this.getBoundVar(),
                         this.getBody().apply(s));
  }

  public String toString() {
    return "(the-class-of-all (" + this.bound + ") " + this.property + ")";
  }

  /** Construct a ClassTerm with the given bound variable and property. */
  public ClassTerm(Variable v, Formula p) {
    super(ClassTerm.CLASS_TERM_CONSTRUCTOR);
    this.bound = v;
    this.property = p;
  }

  /** Construct a ClassTerm with the given bound variable and property. */
  public ClassTerm(TermConstructor s, Variable v, Formula p) {
    super(ClassTerm.CLASS_TERM_CONSTRUCTOR);
    this.bound = v;
    this.property = p;
  }

} // ClassTerm
