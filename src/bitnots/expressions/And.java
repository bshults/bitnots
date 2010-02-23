package bitnots.expressions;

import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class And extends PropositionalFormula {

  private static final long serialVersionUID = 5531242231934209579L;

  public static final String AND_STRING = "and";

  public static final PropositionalConstructor SYMBOL =
    (PropositionalConstructor)
    Symbol.putOrGet(PropositionalConstructor.class, And.AND_STRING);

//    static {
//      Symbol.registerSymbol(AND_STRING, SYMBOL);
//    }

  private String toSExpString() {
    StringBuffer retVal = new StringBuffer("(and");
    Iterator it = this.arguments();
    retVal.append(" " + it.next());
    retVal.append(" " + it.next());
    while (it.hasNext()) {
      retVal.append(" " + it.next());
    }
    retVal.append(")");
    return retVal.toString();
  }
  
  public String toString() {
    return this.toSExpString();
  }

  public And(List args) {
    super(SYMBOL, args);
  }
  
}// And

