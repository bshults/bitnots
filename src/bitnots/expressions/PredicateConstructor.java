package bitnots.expressions;

import java.util.*;

/**
 * PredicateConstructor.java
 *
 *
 * <p>Created: Mon Jan 25 17:21:48 1999
 *
 * @author Benjamin Shults
 * @version
 */

public class PredicateConstructor extends FormulaConstructor {
  
  public final static PredicateConstructor EQUALITY_SYMBOL =
    (PredicateConstructor) Symbol.putOrGet(PredicateConstructor.class, "=");

  /** This is a set of &lt;String, PredicateConstructor&gt; pairs for all
   * known PredicateConstructors.  */
//  public static IdentityHashMap PRED_SYMBOLS = new IdentityHashMap();

  /** Intern the name as a predicate symbol into the default theory */
  public static final PredicateConstructor intern(String name, PredicateConstructor s)
    throws NoArityException {
    if (s.getArity() == -1)
      throw new NoArityException(s);
    else
      return s;
  }

  /** Arity.  Null means not yet determined. */
  private int arity = -1;

  public boolean checkArity(int i) {
    if (i == arity)
      return true;
    else
      throw new IllegalStateException();
  }

  public int getArity() {
    return this.arity;
  }

  protected void setArity(int args) {
    this.arity = args;
  }

  /** The theory into which this is interned. */
  //  protected Theory theory;

  public PredicateConstructor(String first) {
    this(first, -1); // , Theory.DEFAULT);
  }
  
  public PredicateConstructor(String first, int args) {
    super(first);
    this.arity = args;
  }
  
} // PredicateConstructor

