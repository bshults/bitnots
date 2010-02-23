package bitnots.tableaux;

import bitnots.expressions.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public /* abstract */ class NegativeTruth extends ClosingFormula
  implements NegativeFormula {

  public NegativeTruth(Formula f) {
    super(f, HELPER);
  }
  
}// NegativeTruth

