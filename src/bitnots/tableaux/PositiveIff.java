package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2 */

public final class PositiveIff extends BetaFormula
  implements PositiveFormula {

  protected Collection createChildren() {
    return this.createChildren(this.involvedSplits);
  }
  protected Collection createChildren(Set splits) {
    Iterator it =
      ((PropositionalFormula) this.getFormula()).arguments();
    ArrayList value = new ArrayList(2);
    ArrayList first = new ArrayList(2);
    ArrayList second = new ArrayList(2);
    Formula A = (Formula) it.next();
    Formula B = (Formula) it.next();
    
    first.addAll(this.createChildFormulaCollection(A, splits, true));
    first.addAll(this.createChildFormulaCollection(B, splits, true));
    second.addAll(this.createChildFormulaCollection(A, splits, false));
    second.addAll(this.createChildFormulaCollection(B, splits, false));
    value.add(first);
    value.add(second);
    return value;
  }

  public PositiveIff(Formula f) {
    super(f, HELPER);
  }
  
} // PositiveIff

