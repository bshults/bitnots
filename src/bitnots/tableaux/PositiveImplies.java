package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public final class PositiveImplies extends BetaFormula
  implements PositiveFormula {

  protected Collection createChildren() {
    return this.createChildren(this.involvedSplits);
  }
  protected Collection createChildren(Set splits) {
    Iterator it =
      ((PropositionalFormula) this.getFormula()).arguments();
    // will contain the two collections of formulas for the two new leaves
    ArrayList value = new ArrayList(2);
    value.add(this.createChildFormulaCollection((Formula) it.next(),
                                                splits, false));
    value.add(this.createChildFormulaCollection((Formula) it.next(),
                                                splits, true));
    return value;
  }

  public PositiveImplies(Formula f) {
    super(f, HELPER);
  }
  
} // PositiveImplies

