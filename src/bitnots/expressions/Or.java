package bitnots.expressions;

import java.util.*;
import java.io.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class Or extends PropositionalFormula {

  public static final String STRING = "or";

  public static final PropositionalConstructor SYMBOL =
    (PropositionalConstructor)
    Symbol.putOrGet(PropositionalConstructor.class, Or.STRING);

  public String toString() {
    StringBuffer retVal = new StringBuffer("(or");
    Iterator it = this.arguments();
    retVal.append(" " + it.next());
    retVal.append(" " + it.next());
    while (it.hasNext()) {
      retVal.append(" " + it.next());
    }
    retVal.append(")");
    return retVal.toString();
  }

  public Or(List args) {
    super(SYMBOL, args);
  }
  
}// Or

