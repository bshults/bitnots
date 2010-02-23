package bitnots.tableaux;

import java.util.*;
import bitnots.expressions.*;

/**
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public abstract class GammaFormula extends TableauFormula {

  private int copies = 0;

  /**
   * Returns the number of times the gamma-rule has been applied to
   * this formula.
   * @return the number of times the gamma-rule has been applied to
   * this formula. */
  public int getCopies() {
    return this.copies;
  }

  public final void expand() {
    this.copies++;
    this.attachChildNodes();
  }

  /** This adds the receiver to the list of alphas, betas, etc., of
   * the tableau of pn.
   */
  protected final void registerWithTableau(Tableau pn) {
    //    System.out.println("Gamma Registration requested.");
    pn.addGamma(this);
  }

  public final void unregisterWithTableau(Tableau pn) {
    pn.removeGamma(this);
  }

  /**
   * Return a Collection of new free variables that will replace the
   * variables bound in this formula.  The terms in the return value
   * are in the same order as the variable they replace occur in the
   * boundVars list.  Also, the names of the new variables are based
   * on the names of the bound vars.
   * @return a Collection of new free variables that will replace the
   * variables bound in this formula.  The terms in the return value
   * are in the same order as the variable they replace occur in the
   * boundVars list.  Also, the names of the new variables are based
   * on the names of the bound vars. */
  private static Collection createNewFreeVars(Collection boundVars) {
    ArrayList value = new ArrayList(boundVars.size());
    Iterator bvIt = boundVars.iterator();
    do {
      Variable bv = (Variable) bvIt.next();
      value.add(Variable.createNewFreeVar(bv));
    } while (bvIt.hasNext());
    return value;
  }

  protected final void attachChildNodes() {
    final TableauNode current = this.getBirthPlace();

    // Since this is a delta, createChildren will return a singleton
    Collection<TableauFormula> forms = this.createChildren().iterator().next();
    // splice new child node into its place.
    TableauNode child = current.spliceUnder(forms);
    this.addUsedAt(child);
    
    if (child.enforceRegularity())
      this.unregisterWithTableau(current.getTableau());
  }

  protected Collection<Collection<TableauFormula>> createChildren() {
    Formula body =
      (Formula) ((FOLFormula) this.getFormula()).getBody();

    Collection boundVars =
      ((FOLFormula) this.getFormula()).getBoundVars();

    Collection freeVars = GammaFormula.createNewFreeVars(boundVars);

    Substitution s =
      Substitution.createSubstitution(boundVars, freeVars);

    body = body.apply(s);

    return Collections.singleton(this.createChildFormulaCollection(body));
  }

  public GammaFormula(Formula f, FormulaHelper help) {
    super(f, help);
  }

}// GammaFormula

