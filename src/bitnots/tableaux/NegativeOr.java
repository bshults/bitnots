package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public final class NegativeOr extends AlphaFormula
  implements NegativeFormula {

  protected Collection createChildren() {
    Iterator it = ((PropositionalFormula) this.getFormula()).arguments();
    ArrayList value = new ArrayList(((PropositionalFormula) this.getFormula()).getNumberOfArguments());
    
    while (it.hasNext()) {
      value.addAll(this.createChildFormulaCollection((Formula) it.next(),
                                                     false));
    }
    return Collections.singleton(value);
  }

  public NegativeOr(Formula f) {
    super(f, HELPER);
  }
  
} // NegativeOr

