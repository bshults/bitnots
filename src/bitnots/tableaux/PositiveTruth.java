package bitnots.tableaux;

import bitnots.expressions.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public abstract class PositiveTruth extends NilopFormula
  implements PositiveFormula {

  public PositiveTruth(Formula f) {
    super(f, HELPER);
  }
  
}// PositiveTruth

