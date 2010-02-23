package bitnots.tableaux;

import java.util.*;
import bitnots.expressions.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public /* abstract */ class ClosingFormula extends TableauFormula {

  /**
   * There is generally nothing to do to a closing formula since it
   * closes the branch.  This default implementation does just that.
   */
  public final void expand() {}
  
  protected final void attachChildNodes() {}

  /**
   * Should this add a closure to the branch or something?  Probably
   * this should be a nilop but it's worth thinking about.
   */
  protected void registerWithTableau(Tableau p) {}

  public void unregisterWithTableau(Tableau p) {
  }

  protected Collection createChildren() {
    return Collections.EMPTY_SET;
  }

//    ClosingFormula(Formula f) {
//      this(f, false);
//    }
  
  ClosingFormula(Formula f, FormulaHelper help) {
    super(f, help);
  }
  
} // ClosingFormula

