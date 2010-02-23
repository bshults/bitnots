package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public final class PositiveOr extends BetaFormula
  implements PositiveFormula {

  protected Collection createChildren() {
    return this.createChildren(this.involvedSplits);
  }
  protected Collection createChildren(Set splits) {
    Iterator it = ((PropositionalFormula) this.getFormula()).arguments();
    ArrayList value = new ArrayList(((PropositionalFormula) this.getFormula()).getNumberOfArguments());

    while (it.hasNext())
      value.add(
        this.createChildFormulaCollection((Formula) it.next(), splits, true));
    return value;
  }

  public PositiveOr(Formula f) {
    super(f, HELPER);
  }
  
} // PositiveOr

