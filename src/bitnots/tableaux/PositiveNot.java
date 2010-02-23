package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public final class PositiveNot extends AlphaFormula
  implements PositiveFormula {

  protected Collection createChildren() {
    Collection pfs = 
      this.createChildFormulaCollection(
        (Formula) ((PropositionalFormula)
                   this.getFormula()).arguments().next(),
        false);
    return Collections.singleton(pfs);
  }

  public PositiveNot(Formula f) {
    super(f, HELPER);
  }
  
}// PositiveNot

