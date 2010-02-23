package bitnots.expressions;

import java.util.*;
import java.io.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class Implies extends PropositionalFormula {

  public static final String STRING = "implies";

  public static final PropositionalConstructor SYMBOL =
    (PropositionalConstructor)
    Symbol.putOrGet(PropositionalConstructor.class, Implies.STRING);

  public String toString() {
    Iterator it = this.arguments();
    return "(implies  " + it.next() + " " + it.next() + ")";
  }

  public Implies(List args) {
    super(SYMBOL, args);
  }
  
} // Implies

