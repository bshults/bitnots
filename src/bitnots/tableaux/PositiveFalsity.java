package bitnots.tableaux;

import bitnots.expressions.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public /* abstract */ class PositiveFalsity extends ClosingFormula
  implements PositiveFormula {

  public PositiveFalsity(Formula f) {
    super(f, HELPER);
  }
  
}// PositiveFalsity

