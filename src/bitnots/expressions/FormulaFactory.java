package bitnots.expressions;

import java.lang.reflect.*;
import java.util.*;
import bitnots.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

// TODO: would be better to allow formula classes to register with
// this class somehow, like through a properties file.

public final class FormulaFactory {

  private static final Hashtable TABLE = new Hashtable(20);

  /**
   * Register the string symbol so that when <code>getFormula</code>
   * is called with symbol as an argument, <code>f</code> will be
   * returned.
   *
   * @param symbol the symbol that determines the type of the formula.
   * @param f the <code>Formula</code> to return when this symbol is
   * used to construct a formula.
   * @return the previous Object associated with this symbol if there
   * was one.  Otherwise, <code>null</code>.
   */
  public static Object registerSymbol(FormulaConstructor symbol, Formula f) {
    return TABLE.put(symbol, f);
  }

  /**
   * Register the string symbol so that when <code>getFormula</code>
   * is called with symbol as an argument, the constructor associated
   * with <code>c</code> will be called.
   *
   * @param symbol the symbol that determines the type of the formula.
   * @param c a <code>Constructor</code> value describing the
   * constructor to be called and its argument types.
   * @return the previous Object associated with this symbol if there
   * was one.  Otherwise, <code>null</code>.
   */
  public static Object registerSymbol(FormulaConstructor symbol,
                                      Constructor c) {
    return TABLE.put(symbol, c);
  }

  /**
   * 
   * 
   * @param ps
   * @param args this will be passed to the Constructor.newInstance method
   * so its contents should be whatever the relevant constructor expects.
   * In the case of a Predicate, <code>args</code> should contain a single
   * element which will be a List of the normal arguments.
   * @return
   */
  public static Formula getFormula(FormulaConstructor ps, Object[] args) {
    Object value = TABLE.get(ps);
    if (value == null) {
      return new Predicate((PredicateConstructor) ps, (List) args[0]);
    } else if (value instanceof Formula)
      return (Formula) value;
    else
      try {
        //        System.out.println("creating " + value + " " + args[0]);
        return (Formula) ((Constructor) value).newInstance(args);
      } catch (InstantiationException ie) {
        ie.printStackTrace();
        return null;
      } catch (IllegalAccessException iae) {
        iae.printStackTrace();
        return null;
      } catch (InvocationTargetException ite) {
        ite.printStackTrace();
        return null;
      }
  }

  static {
    try {
      registerSymbol(Truth.SYMBOL, Truth.TRUTH);
      registerSymbol(Falsity.SYMBOL, Falsity.FALSITY);
      registerSymbol(
        And.SYMBOL,
        And.class.getConstructor(new Class[] {List.class}));
    
      registerSymbol(
        Or.SYMBOL,
        Or.class.getConstructor(new Class[] {List.class}));
    
      registerSymbol(
        Not.SYMBOL,
        Not.class.getConstructor(new Class[] {List.class}));
    
      registerSymbol(
        Implies.SYMBOL,
        Implies.class.getConstructor(new Class[] {List.class}));
    
      registerSymbol(
        Iff.SYMBOL,
        Iff.class.getConstructor(new Class[] {List.class}));
    
      registerSymbol(
        Forall.SYMBOL,
        Forall.class.getConstructor(new Class[] {List.class,
                                                 Formula.class}));
    
      registerSymbol(
        Forsome.SYMBOL,
        Forsome.class.getConstructor(new Class[] {List.class,
                                                  Formula.class}));
    } catch (NoSuchMethodException nsme) {
      nsme.printStackTrace();
    }
  }

  private FormulaFactory() {
  }

}// FormulaFactory
