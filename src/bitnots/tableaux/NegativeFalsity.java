package bitnots.tableaux;

import bitnots.expressions.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public abstract class NegativeFalsity extends NilopFormula
  implements NegativeFormula {

  public NegativeFalsity(Formula f) {
    super(f, HELPER);
  }
  
}// NegativeFalsity

