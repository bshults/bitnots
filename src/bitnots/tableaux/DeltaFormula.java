package bitnots.tableaux;

import java.util.*;
import bitnots.expressions.*;

/**
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public abstract class DeltaFormula extends TableauFormula {

  public final void expand() {
    this.attachChildNodes();
    this.getBirthPlace().getTableau().removeDelta(this);
  }

  /** This adds the receiver to the list of alphas, betas, etc., of
   * the tableau of pn.
   */
  protected final void registerWithTableau(Tableau pn) {
    pn.addDelta(this);
  }

  public final void unregisterWithTableau(Tableau pn) {
    pn.removeDelta(this);
  }

  /**
   * @return a Collection of new terms that will replace the variables
   * bound in this formula.  The terms in the return value are in the
   * same order as the variable they replace occur in the boundVars
   * list. */
  protected Collection createNewSkolemFunctions() {
    Collection boundVars = ((FOLFormula) this.getFormula()).getBoundVars();
    List freeVars = new ArrayList(this.getFormula().freeVars());
    ArrayList value = new ArrayList(boundVars.size());
    Iterator bvIt = boundVars.iterator();
    do {
      Variable bv = (Variable) bvIt.next();
      value.add(Function.createNewSkolemFunction(bv, freeVars));
    } while (bvIt.hasNext());
    return value;
  }

  protected final void attachChildNodes() {
    final TableauNode current = this.getBirthPlace();

 
//     // INDUCTION
//     if (form instanceof NilopFormula) {
//       current.getTableau().addDelta0Literal(form);
//     }

    // Since this is a delta, createChildren will return a singleton
    Collection<TableauFormula> forms = this.createChildren().iterator().next();
    // splice new child node into its place.
    TableauNode child = current.spliceUnder(forms);
    this.addUsedAt(child);

    if (child.enforceRegularity())
      // unregister with tableau
      this.unregisterWithTableau(current.getTableau());
  }

  protected Collection<Collection<TableauFormula>> createChildren() {
    Formula body =
      (Formula) ((FOLFormula) this.getFormula()).getBody();
    // the list of skolem terms introduced by the application of the
    // delta-rule.
    Collection skolemTerms = this.createNewSkolemFunctions();

    Substitution s =
      Substitution.createSubstitution(
        ((FOLFormula) this.getFormula()).getBoundVars(),
        skolemTerms);

    body = body.apply(s);

    return Collections.singleton(this.createChildFormulaCollection(body));
  }

  public DeltaFormula(Formula f, FormulaHelper help) {
    super(f, help);
  }

}// DeltaFormula

