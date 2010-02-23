package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public final class PositiveAnd extends AlphaFormula
  implements PositiveFormula {

  protected Collection createChildren() {
    Iterator<Formula> it = ((PropositionalFormula) this.getFormula()).arguments();
    ArrayList value = new ArrayList(((PropositionalFormula) this.getFormula()).getNumberOfArguments());
    
    while (it.hasNext())
      value.addAll(this.createChildFormulaCollection((Formula) it.next(),
                                                     true));
    return Collections.singleton(value);
  }

  public PositiveAnd(Formula f) {
    super(f, HELPER);
  }
  
} // PositiveAnd

