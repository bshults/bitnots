package bitnots.tableaux;

import java.util.*;
import bitnots.expressions.*;

/**
 * Instances represent alpha-formulas.
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public abstract class AlphaFormula extends TableauFormula {

  public final void expand() {
    this.attachChildNodes();
    this.getBirthPlace().getTableau().removeAlpha(this);
  }

  /** This adds the receiver to the list of alphas, betas, etc., of
   * the tableau of pn.
   */
  protected final void registerWithTableau(Tableau pn) {
    pn.addAlpha(this);
  }

  public final void unregisterWithTableau(Tableau pn) {
    pn.removeAlpha(this);
  }

  // TODO: all of the attachChildNodes methods that don't split are getting
  // the same implementation.  Try to extract this method to a super class.
  protected final void attachChildNodes() {
    final TableauNode current = this.getBirthPlace();

    // Since this is an alpha, createChildren will return a singleton
    Collection<TableauFormula> forms = this.createChildren().iterator().next();
    
    // splice new child node into its place.
    TableauNode child = current.spliceUnder(forms);
    this.addUsedAt(child);

    if (child.enforceRegularity())
      this.unregisterWithTableau(current.getTableau());
  }

  AlphaFormula(Formula f, FormulaHelper help) {
    super(f, help);
  }

//    AlphaFormula(Formula f, boolean sign) {
//      super(f, sign);
//    }

} // AlphaFormula

