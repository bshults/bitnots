package bitnots.expressions;

import java.util.*;
import java.io.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class Iff extends PropositionalFormula {

  public static final String IFF_STRING = "iff";

  public static final PropositionalConstructor SYMBOL =
    (PropositionalConstructor)
    Symbol.putOrGet(PropositionalConstructor.class, Iff.IFF_STRING);

  public String toString() {
    Iterator it = this.arguments();
    return "(iff  " + it.next() + " " + it.next() + ")";
  }

  public Iff(List args) {
    super(SYMBOL, args);
  }
  
}// Iff

