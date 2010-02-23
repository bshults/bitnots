package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public final class NegativeImplies extends AlphaFormula
  implements NegativeFormula {

  protected Collection createChildren() {
    Iterator it =
      ((PropositionalFormula) this.getFormula()).arguments();
    ArrayList value = new ArrayList(2);
    value.addAll(this.createChildFormulaCollection((Formula) it.next(),
                                                   true));
    value.addAll(this.createChildFormulaCollection((Formula) it.next(),
                                                   false));
    return Collections.singleton(value);
  }

  public NegativeImplies(Formula f) {
    super(f, HELPER);
  }
  
} // NegativeImplies

