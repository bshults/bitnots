package bitnots.tableaux;

import bitnots.expressions.*;

/**
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class NegativePredicate extends PredicateFormula
  implements NegativeFormula {

  public NegativePredicate(Formula f) {
    super(f, HELPER);
  }
}// NegativePredicate

