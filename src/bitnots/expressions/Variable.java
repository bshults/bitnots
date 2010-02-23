package bitnots.expressions;

import bitnots.equality.*;
import java.util.*;

/**
 * @author Benjamin Shults
 * @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
 * @version .2
 */

// So far, there seems to be nothing to require bound vars and free
// vars to be disjoint.  Some things could be sped up rather
// significantly if I did that, though.  E.g., contains method and
// replaceUnboundOccurrencesWith method in FOLFormula class.

public class Variable extends Term implements Comparable {

  /**
   * Maps the name of a variable to the number of times that  name has
   * been seen.  This allows multiple FOL formulas to have the same
   * name for their bound variables, and if they are expanded using
   * the Gamma rule, they will have distinct names.
   */
  private static HashMap UNIQUE_HASH = new HashMap();

  private static long UNIQUE = 0;
  public static String BOUND_CLASS = "b";
  public static String SEQUENT_CLASS = "s";
  public static String FREE_CLASS = "v";

  protected Symbol symbol;

  private String internalName;

  /**
   * The varClass shows whether this Variable is attached to a
   * free (v), bound (b), or sequent (s) variable.
   */
  private String varClass;
  
  /**
   * A number that is unique among all variables.  When paired with
   * the varClass, this becomes the variable's internal name.
   */
  private long number;

//   private Variable(String s, String vClass) {
//     this((VariableSymbol) Symbol.putOrGet(VariableSymbol.class, s),
//          vClass);
//   }

  private Variable(Symbol s, String vClass) {
    this.symbol = s;
    this.fixInternalName(vClass);
    this.computeHashCode();
  }

  private void fixInternalName(String vClass) {
    this.varClass = vClass;
    this.number = Variable.UNIQUE++;
    this.internalName = this.varClass + this.number;
  }

  public boolean isSequentVar() {
    return this.getVarClass() == Variable.SEQUENT_CLASS;
  }

  public boolean isTableauVar() {
    return this.getVarClass() == Variable.FREE_CLASS;
  }

  public boolean isBoundVar() {
    return this.getVarClass() == Variable.BOUND_CLASS;
  }

  /**
   * Returns the external name for this Variable.
   * @return the external name for this Variable.
   */
  public String getExternalName() {
    return this.symbol.getName();
  }
    
  /**
   * Returns the internal name for this Variable.
   * @return the internal name for this Variable.
   */
  private String getInternalName() {
    return this.internalName;
  }
    
  /**
   * Returns the variable class of this Variable, which will be either
   * Variable.BOUND_CLASS for a bound variable, Variable.SEQUENT_CLASS
   * for a sequent variable, or Variable.FREE_CLASS for a free
   * variable.
   * @return the variable class of this Variable.
   */
  public String getVarClass() {
    return this.varClass;
  }
    
  /**
   * Returns the unique number for this Variable.
   * @return the unique number for this Variable.
   */
  private long getInternalNumber() {
    return this.number;
  }

  /**
   * Returns the symbol for this Variable.
   * @return the symbol for this Variable.
   */
  public Symbol getSymbol() {
    return this.symbol;
  }

  /**
   * Returns the external name of this Variable, that is, the name
   * that the user will see.
   * @return the external name of this Variable.
   */
  public String toString() {
    return this.getExternalName();
  }

  /**
   */
  public boolean equals(Object o) {
    return (o instanceof Variable) &&
      ((Variable) o).getInternalNumber() == this.getInternalNumber() &&
      ((Variable) o).getVarClass() == this.getVarClass();
  }
  
  /**
   * The hashcode for this object.  Determined on construction, so it
   * does not need to be determined upon every call.  This hinges on
   * the fact that the Variable's internals, the symbol, the internal
   * name and the internal number, are not going to change.  This
   * gives us a huge performace boost.
   */
  private int hashCode;
  
  /**
   * Recomputes the hash code for this object.  This method must be
   * called every time the internals of the object changes.
   */
  private void computeHashCode() {
    this.hashCode = this.symbol.hashCode() +
                this.getInternalName().hashCode() +
                (int) this.getInternalNumber();
  }

  public int hashCode() {
//    UNCOMMENT THIS TO NOT ASSUME IMMUTABLE OBJECTS
//    this.computeHashCode();
    return this.hashCode;
  }

  /**
   * Compares two Variables by class and then by number.  By class, a
   * bound variable comes before a sequent variable, which comes
   * before a free variable.  If the class is the same, their numbers
   * are compared (which are guarenteed to be unique).
   * @param o the other Variable to compare.
   * @return negative integer or a positive integer if this Variable
   * is less than, or greater than the given Variable, respectively.
   */
  public int compareTo(Object o) {
    Variable otherVar = (Variable) o;
    if (this.getVarClass().compareTo(otherVar.getVarClass()) == 0) {
      return (new Long(this.getInternalNumber()))
        .compareTo(new Long(otherVar.getInternalNumber()));
    } else
      return this.getVarClass().compareTo(otherVar.getVarClass());
  }

  /**
   * Creates a new bound variable with the name <b>name</b>.
   * @param name the name of the new bound variable.
   */
  public static Variable createNewBoundVar(String name) {
    return new Variable(new Symbol(name), Variable.BOUND_CLASS);
  }

  /**
   * The name of the new free variable is a variation of the name of
   * <b>v</b>.
   * @param bv the variable that this new variable will replace and on
   * whose name the return value's name will be based.
   * @return a new free variable whose name is a variation on the name
   * of <b>v</b>.
   */
  public static Variable createNewFreeVar(Variable v) {
    return new Variable(
      Variable.createUniqueVarSymbol(v.getExternalName()),
      Variable.FREE_CLASS);
  }

  /**
   * Creates a new sequent variable with the name <b>name</b>.
   * @param name the name of the new sequent variable.
   */
  public static Variable createNewSequentVar(String name) {
    return new Variable(new Symbol(name), Variable.SEQUENT_CLASS);
  }


  /**
   * Returns a new Symbol based on the given name.
   * @param baseName the name that the name of this Symbol will be
   * based on.
   * @return a new Symbol with a unique name.
   */
  public static Symbol createUniqueVarSymbol(String baseName) {
    Integer unique = null;
    if (Variable.UNIQUE_HASH.containsKey(baseName))
      unique = (Integer) Variable.UNIQUE_HASH.get(baseName);
    else
      unique = new Integer(0);
    Variable.UNIQUE_HASH.put(baseName,
                                  new Integer(unique.intValue() + 1));
    return new Symbol(baseName +
                 ((unique.intValue() == 0) ? "" : unique.toString()));
  }

  /**
   * Resets the counter that appends numbers to new free variables,
   * expanded from bound variables (via the Gamma rule).  This method
   * should not be called while still proving a theorem, because new
   * free variables could have externalNames exactly the same as
   * other variables.
   */
  public static void resetSymbolCounter() {
    Variable.UNIQUE_HASH = new HashMap();
  }

  public Set freeVars() {
    return Collections.singleton(this);
  }

  public Set freeVars(Collection notFree) {
    if (notFree.contains(this))
      return Collections.EMPTY_SET;
    else
      return Collections.singleton(this);
  }

  /**
   * Under the circumstances when this method is called, (i.e., in the
   * congruence closure algorithm) it should never reach a variable.
   * Therefore, this throws an IllegalStateException.
   */
  public Term replaceAllConstants(Function old, Function newC) {
    throw new IllegalStateException();
  }

  public Term replaceUnboundOccurrencesWith(Variable v, Term t) {
    if (this.equals(v))
      return t;
    else
      return this;
  }

  // Should this test for occurrence and return occurrence if there is
  // an occurrence clash?  I think not.  That is not fast enough.
  public int failFast(Term t) {
    if (this.equals(t))
      return Substitution.ALPHA;
    else
      return Substitution.DIFFERENCE;
    //    } else if (t.contains(this))
    //      return Substitution.OCCURRENCE;
    //    else
  }

  /**
   * If this is a sequent variable, be sure it is mapped to a free
   * variable and return that free variable.  Otherwise, return the
   * receiver.
   * @param map maps sequent variables to free variables.
   */
  public Term replaceVariables(Map map) {
    if (this.isSequentVar()) {
      Variable newVar = (Variable) map.get(this);
      if (newVar == null) {
        newVar = Variable.createNewFreeVar(this);
        map.put(this, newVar);
      }
      return newVar;
    } else
      return this;
  }

  // no longer a guarantee that <b>s</b> has already been applied to
  // both.
  public Substitution unify(Term t, Substitution s) {
    return this.apply(s).unifyRecApplied(t, s);
  }

  public Substitution unify(Term t, Substitution s,
                            Substitution bvs) {
    return this.apply(s).unifyRecApplied(t, s, bvs);
  }

  public Substitution unify(Term t, Substitution s, DSTGraph cc) {
    return this.apply(s).unifyRecApplied(t, s, cc);
  }

  public Substitution unify(Term t, Substitution s,
                            Substitution bvs, DSTGraph cc) {
    return this.apply(s).unifyRecApplied(t, s, bvs, cc);
  }

  // Caller guarantees that the Substitution has been applied to the
  // receiver.
  Substitution unifyRecApplied(Term t, Substitution s) {
//     if (this.equals(t))
//       return s;
//     else
    return this.unifyApplied(t.apply(s), s);
  }

  // Caller guarantees that the Substitution has been applied to the
  // receiver.
  Substitution unifyRecApplied(Term t, Substitution s,
                               Substitution bvs) {
//    if (!this.isBoundVar())
    return this.unifyApplied(t.apply(s), s, bvs);
//     else if (t instanceof Variable && t.equals(this))
//       return s;
//     else
//       return null;


//         Term newRec = this.apply(bvs);
//         assert newRec != this;
//         Term newT = t.apply(bvs);
//         if (newT == newRec)
//           return s;
//         else
//           return null;
//       } else
//         return null;
//     }
  }

  // Caller guarantees that the Substitution has been applied to the
  // receiver.
  Substitution unifyRecApplied(Term t, Substitution s, DSTGraph cc) {
//     if (this.equals(t))
//       return s;
//     else
    return this.unifyApplied(t.apply(s), s, cc);
  }

  // Caller guarantees that the Substitution has been applied to the
  // receiver.
  Substitution unifyRecApplied(Term t, Substitution s,
                               Substitution bvs, DSTGraph cc) {
//    if (!this.isBoundVar()) {
    return this.unifyApplied(t.apply(s), s, bvs, cc);
//     } else if (t instanceof Variable && t.equals(this))
//       return s;
//     else
//       return null;
  }

  // Caller guarantees that the Substitution has been applied to the
  // receiver.
  public Substitution unifyApplied(Term t, Substitution s) {
    if (this.equals(t))
      return s;
    else if (t.contains(this))
      return null;
    else
      return s.acons(this, t);
  }

  // Caller guarantees that the Substitution has been applied to the
  // receiver and the argument.
  Substitution unifyApplied(Term t, Substitution s,
                            Substitution bvs) {
    if (!this.isBoundVar()) {
      if (this.equals(t))
        return s;
      else if (t.contains(this))
        return null;
      else if (t instanceof Variable) {
        if (((Variable) t).isBoundVar())
          return null;
        else 
          return s.acons(this, t);
      } else
        return s.acons(this, t);
    } else if (t instanceof Variable) {
      if (((Variable) t).isBoundVar()) {
        // at this point, I know that the receiver is a bound variable.
        Term newRec = this.apply(bvs);
        assert newRec != this;
        Term newT = t.apply(bvs);
        if (newT == newRec)
          return s;
        else
          return null;
      } else
        return null;
    } else
      return null;
  }

  // Caller guarantees that the Substitution has been applied to the
  // receiver.
  public Substitution unifyApplied(Term t, Substitution s, DSTGraph cc) {
    if (this.equals(t))
      return s;
    else if (t.contains(this))
      return null;
    else { // if (cc.equivalent(this, t)) {
      Substitution value = cc.unifiable(this, t, s);
      if (value != null) {
        cc.setUsed(true);
        return value;
      } else
        return s.acons(this, t);
    }
  }

  // Caller guarantees that the Substitution has been applied to the
  // receiver and the argument.
  Substitution unifyApplied(Term t, Substitution s,
                            Substitution bvs, DSTGraph cc) {
    if (!this.isBoundVar()) {
      if (this.equals(t))
        return s;
      else if (t.contains(this))
        return null;
      else if (t instanceof Variable) {
        if (((Variable) t).isBoundVar())
          return null;
        else {
          Substitution value = cc.unifiable(this, t, s);
          if (value != null) {
            cc.setUsed(true);
            return value;
          } else
            return s.acons(this, t);
        }
      } else
        return s.acons(this, t);
    } else if (t instanceof Variable) {
      if (((Variable) t).isBoundVar()) {
        // at this point, I know that the receiver is a bound variable.
        Term newRec = this.apply(bvs);
        assert newRec != this;
        Term newT = t.apply(bvs);
        if (newT == newRec)
          return s;
        else
          return null;
      } else
        return null;
    } else
      return null;
  }

  public Term apply(Substitution s) {
    return s.get(this);
  }

  public boolean contains(Term t) {
    return this.equals(t);
  }

  public boolean contains(Formula f) {
    return false;
  }
} // Variable
