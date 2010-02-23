package bitnots.expressions;

import bitnots.equality.*;
import java.util.*;
import java.io.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class Truth extends Formula {

  public static final String TRUTH_STRING = "truth";

  public static final LogicalConstructor SYMBOL =
    (LogicalConstructor)
    Symbol.putOrGet(LogicalConstructor.class, Truth.TRUTH_STRING);

  private Object readResolve() throws ObjectStreamException {
    return TRUTH;
  }

  public Set freeVars() {
    return Collections.EMPTY_SET;
  }

  public Set freeVars(Collection notFree) {
    return Collections.EMPTY_SET;
  }

  public boolean equals(Object o) {
    return o instanceof Truth;
  }

  public boolean equalsUnderSub(Formula f, Substitution s) {
    return f instanceof Truth;
  }

  public int hashCode() {
    return Truth.TRUTH_STRING.hashCode();
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
    return this.unify(f, s);
  }

  public Substitution unify(Formula f, Substitution s, DSTGraph cc) {
    return this.unifyRecApplied(f, s);
  }

  public Substitution unify(Formula f, Substitution s, Substitution bvs,
                            DSTGraph cc) {
    // bvs not needed because the receiver is atomic.
    return this.unify(f, s);
  }

  public Substitution unifyRecApplied(Formula f, Substitution s) {
    if (f instanceof Truth)
      return s;
    else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s,
                                      Substitution bvs) {
    if (f instanceof Truth)
      return s;
    else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f instanceof Truth)
      return s;
    else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s,
                                      Substitution bvs, DSTGraph cc) {
    if (f instanceof Truth)
      return s;
    else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s) {
    if (f instanceof Truth)
      return s;
    else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s,
                                   Substitution bvs) {
    if (f instanceof Truth)
      return s;
    else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f instanceof Truth)
      return s;
    else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s,
                                   Substitution bvs, DSTGraph cc) {
    if (f instanceof Truth)
      return s;
    else
      return null;
  }

  int failFast(Formula f) {
    if (f instanceof Truth)
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
    return f instanceof Truth;
  }

  public static final Truth TRUTH = new Truth();

  public String toString() {
    return "(truth)";
  }

  private Truth() {
    super(SYMBOL);
  }
  
}// Truth

