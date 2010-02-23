package bitnots.expressions;

// import bitnots.theories.*;
import java.util.HashMap;

/**
 * @author Benjamin Shults
 * @version .2
 */

public class FunctionConstructor extends TermConstructor {
  
  private int arity;

  public void setArity(int i) {
    this.arity = i;
  }

  public int getArity() {
    return this.arity;
  }

  /**
   * This is a FunctionConstructor that is used for the quick
   * construction of temporary functions whose constructor is
   * irrelevant.  E.g., this is used to see whether substitutions are
   * compatible. */
  public static final FunctionConstructor DUMMY =
    new FunctionConstructor("DUMMY_");

//  private static long UNIQUE = 5;

  private static HashMap UNIQUE_HASH = new HashMap();
  
  /**
   * Resets the counter that gives unique names to functions.
   */
  public static void resetSymbolCounter() {
    FunctionConstructor.UNIQUE_HASH = new HashMap();
  }

  /**
   * Returns a String based on the given name, appened with a unique
   * number.
   * @param base the name that the name of this Symbol will be based
   * on.
   * @return a String with a unique name.
   */
  public static String createUniqueFunctionName(String base)
  {
    Integer unique = null;
    if (FunctionConstructor.UNIQUE_HASH.containsKey(base))
      unique = (Integer) FunctionConstructor.UNIQUE_HASH.get(base);
    else
      unique = new Integer(0);
    FunctionConstructor.UNIQUE_HASH.put(base, new Integer(unique.intValue() + 1));
//    return base + ((unique.intValue() == 0) ? "" : "_" + unique);
    return base + "_" + unique;
  }

  public static FunctionConstructor createUniqueFunctionSymbol() {
    do {
//      String attempt = "FUN_" + UNIQUE++;
      String attempt = FunctionConstructor.createUniqueFunctionName("FUN");
      if (Symbol.get(FunctionConstructor.class, attempt) == null)
        return (FunctionConstructor)
          Symbol.putOrGet(FunctionConstructor.class, attempt);
    } while (true);
  }

  /**
   * bases the name of the return value on the name of the given
   * String.
   */
  public static FunctionConstructor createUniqueFunctionSymbol(String s) {
    do {
//      String attempt = s + "_" + UNIQUE++;
      String attempt = FunctionConstructor.createUniqueFunctionName(s);
      if (Symbol.get(FunctionConstructor.class, attempt) == null)
        return (FunctionConstructor)
          Symbol.putOrGet(FunctionConstructor.class, attempt);
    } while (true);
  }

  /**
   * bases the name of the return value on the name of the given
   * variable.  */
  public static FunctionConstructor createUniqueFunctionSymbol(Variable v) {
    do {
//      String attempt = v.toString() + "_" + UNIQUE++;
      String attempt = FunctionConstructor.createUniqueFunctionName(v.toString());
      if (Symbol.get(FunctionConstructor.class, attempt) == null)
        return (FunctionConstructor)
          Symbol.putOrGet(FunctionConstructor.class, attempt);
    } while (true);
  }

  public FunctionConstructor(String first) {
    this(first, 0);//, Theory.DEFAULT);
  }
  
  public FunctionConstructor(String first, int i) {
    super(first);
    this.arity = i;
  }

} // FunctionConstructor
