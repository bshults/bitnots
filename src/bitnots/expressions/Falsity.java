package bitnots.expressions;

import bitnots.equality.*;
import java.util.*;
import java.io.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class Falsity extends Formula {

  public static final String FALSITY_STRING = "falsity";

  public static final LogicalConstructor SYMBOL =
    (LogicalConstructor)
    Symbol.putOrGet(LogicalConstructor.class, Falsity.FALSITY_STRING);

  private Object readResolve() throws ObjectStreamException {
    return FALSITY;
  }

  public Set freeVars() {
    return Collections.EMPTY_SET;
  }

  public Set freeVars(Collection notFree) {
    return Collections.EMPTY_SET;
  }

  public boolean equals(Object o) {
    return o == this;
  }

  public boolean equalsUnderSub(Formula f, Substitution s) {
    return f == this;
  }

  public int hashCode() {
    return Falsity.FALSITY_STRING.hashCode();
  }

  public Formula replaceVariables(Map map) {
    return this;
  }

  public Substitution unify(Formula f, Substitution s) {
//    throws NotUnifiableException {
    return this.unifyRecApplied(f, s);
  }

  public Substitution unify(Formula f, Substitution s, Substitution bvs) {
    // TODO: make more efficient?
    // bvs not needed because the receiver is atomic.
    return this.unifyRecApplied(f, s);
  }

  public Substitution unify(Formula f, Substitution s, DSTGraph cc) {
    return this.unifyRecApplied(f, s);
  }

  public Substitution unify(Formula f, Substitution s, Substitution bvs,
                            DSTGraph cc) {
    // bvs not needed because the receiver is atomic.
    return this.unifyRecApplied(f, s);
  }

  public Substitution unifyRecApplied(Formula f, Substitution s) {
    if (f == this)
      return s;
    else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s,
                                      Substitution bvs, DSTGraph cc) {
    return this.unifyRecApplied(f, s);
  }

  public Substitution unifyRecApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f == this)
      return s;
    else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s,
                                      Substitution bvs) {
    return this.unifyRecApplied(f, s);
  }

  public Substitution unifyApplied(Formula f, Substitution s) {
    if (f == this)
      return s;
    else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s,
                                   Substitution bvs) {
    if (f == this)
      return s;
    else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f == this)
      return s;
    else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s,
                                   Substitution bvs, DSTGraph cc) {
    if (f == this)
      return s;
    else
      return null;
  }

  int failFast(Formula f) {
    if (f == this)
      return Substitution.ALPHA;
    else
      return Substitution.CLASH;
  }

  public Formula apply(Substitution s) {
    return this;
  }

  public Formula replaceUnboundOccurrencesWith(Variable v, Term t) {
    return this;
  }

  public boolean contains(Term t) {
    return false;
  }

  public boolean contains(Formula f) {
    return f == this;
  }

  public static final Falsity FALSITY = new Falsity();

  public String toString() {
    return "(falsity)";
  }

  private Falsity() {
    super(Falsity.SYMBOL);
  }
  
} // Falsity

