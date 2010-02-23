package bitnots.expressions;

import java.lang.reflect.*;
import java.util.*;
import bitnots.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

// TODO: would be better to allow term classes to register with
// this class somehow, like through a properties file.

public final class TermFactory {

  /**
   * Maps a TermConstructor to either a ComplexTerm or a Constructor.
   */
  private static final Hashtable<TermConstructor, Object> TABLE = 
    new Hashtable(20);

  /**
   * Register the string symbol so that when <code>getTerm</code>
   * is called with symbol as an argument, <code>f</code> will be
   * returned.
   *
   * @param symbol the symbol that determines the type of the term.
   * @param t the <code>Term</code> to return when this symbol is
   * used to construct a term.
   * @return the previous Object associated with this symbol if there
   * was one.  Otherwise, <code>null</code>.
   */
  public static Object registerSymbol(TermConstructor symbol, ComplexTerm t) {
    return TABLE.put(symbol, t);
  }

  /**
   * Register the string symbol so that when <code>getTerm</code>
   * is called with symbol as an argument, the constructor associated
   * with <code>c</code> will be called.
   *
   * @param symbol the symbol that determines the type of the term.
   * @param c a <code>Constructor</code> value describing the
   * constructor to be called and its argument types.
   * @return the previous Object associated with this symbol if there
   * was one.  Otherwise, <code>null</code>.
   */
  public static Object registerSymbol(TermConstructor symbol,
                                      Constructor c) {
    return TABLE.put(symbol, c);
  }

  public static ComplexTerm getTerm(TermConstructor ps, Object[] args) {
    Object value = TABLE.get(ps);
    if (value == null) {
      return Function.createFunction((FunctionConstructor) ps,
                                     (List) args[0]);
    } else if (value instanceof ComplexTerm)
      return (ComplexTerm) value;
    else
      try {
        //        System.out.println("creating " + value + " " + args[0]);
        return (ComplexTerm) ((Constructor) value).newInstance(args);
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
      registerSymbol(
        ClassTerm.SYMBOL,
        ClassTerm.class.getConstructor(new Class[] {Variable.class,
                                                    Formula.class}));
    
      registerSymbol(
        IotaTerm.SYMBOL,
        IotaTerm.class.getConstructor(new Class[] {Variable.class,
                                                   Formula.class}));
    } catch (NoSuchMethodException nsme) {
      nsme.printStackTrace();
    }
  }

  private TermFactory() {
  }

}// TermFactory
