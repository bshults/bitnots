package bitnots.tableaux;

import bitnots.expressions.*;

/**
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public abstract class NilopFormula extends TableauFormula {

  // do nothing.
  public final void expand() {
  }

  protected final void attachChildNodes() {
  }

  protected final void registerWithTableau(Tableau pn) {
  }

  NilopFormula(Formula f, FormulaHelper help) {
    super(f, help);
  }

} // NilopFormula
