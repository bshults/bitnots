package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public final class NegativeAnd extends BetaFormula
  implements NegativeFormula {

  protected Collection<Collection<TableauFormula>> createChildren() {
    return this.createChildren(this.involvedSplits);
  }

  protected Collection<Collection<TableauFormula>> createChildren(Set<TableauNode.Split> splits) {
    Iterator<Formula> it = ((PropositionalFormula) this.getFormula()).arguments();
    ArrayList<Collection<TableauFormula>> value =
      new ArrayList<Collection<TableauFormula>>(((PropositionalFormula) this.getFormula()).getNumberOfArguments());

    while (it.hasNext()) {
      value.add(this.createChildFormulaCollection(it.next(), splits, false));
    }
    return value;
  }

  public NegativeAnd(Formula f) {
    super(f, HELPER);
  }
  
} // NegativeAnd

