package bitnots.tableaux;

import bitnots.expressions.*;

/**
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class PositivePredicate extends PredicateFormula
  implements PositiveFormula {

  public PositivePredicate(Formula f) {
    super(f, HELPER);
  }
}// PositivePredicate

