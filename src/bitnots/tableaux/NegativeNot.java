package bitnots.tableaux;

import java.util.*;
import bitnots.expressions.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class NegativeNot extends AlphaFormula
  implements NegativeFormula {

  protected Collection createChildren() {
    Collection pfs = 
      this.createChildFormulaCollection(
        (Formula) ((PropositionalFormula)
                   this.getFormula()).arguments().next(),
        true);
    return Collections.singleton(pfs);
  }

  public NegativeNot(Formula f) {
    super(f, HELPER);
  }
  
}// NegativeNot

