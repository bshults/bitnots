package bitnots.expressions;

import java.util.*;
import java.io.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class Not extends PropositionalFormula {

  public static final String NOT_STRING = "not";

  public static final PropositionalConstructor SYMBOL =
    (PropositionalConstructor)
    Symbol.putOrGet(PropositionalConstructor.class, Not.NOT_STRING);

  public String toString() {
    Iterator it = this.arguments();
    return "(not  " + it.next() + ")";
  }

  public Not(List args) {
    super(SYMBOL, args);
  }
  
}// Not

