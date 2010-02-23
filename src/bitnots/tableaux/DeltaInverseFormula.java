package bitnots.tableaux;

import bitnots.expressions.*;
import bitnots.tableaux.*;

import java.util.*;

/**
 * The DeltaInverseFormula class allows for the use of the Delta
 * Inverse rule, used in making sequents to be placed in a knowledge
 * base.  This works by re-writing the expand() method to implement the
 * Delta Inverse rule, rather than the regular Delta rule.
 * @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
 * @version 1.0
 */
public class DeltaInverseFormula
{
  /**
   * The DeltaFormula that needs to be expanded using the Delta
   * Inverse rule.
   */
  private DeltaFormula delta;
    
  /**
   * Constructs a new DeltaInverseFormula, taking a DeltaFormula, so
   * it can get necessary information from it.
   */
  public DeltaInverseFormula(DeltaFormula delta) {
    this.delta = delta;
  }
    
  /**
   * Expands the DeltaFormula using the Delta Inverse rule.
   */
  public final void expand() {
    this.attachChildNodes();
    this.delta.getBirthPlace().getTableau().removeDelta(
      this.delta);
  }
    
  /**
   * Instead of doing the normal Delta rule's method for attaching
   * children, it uses the Delta Inverse's method, which is similar
   * to the Gamma rule's method.
   */
  protected final void attachChildNodes() {
    final TableauNode current = this.delta.getBirthPlace();

    // Since this is a delta, createChildren will return a singleton
    Collection<TableauFormula> forms = this.createChildren().iterator().next();
    // splice new child node into its place.
    TableauNode child = current.spliceUnder(forms);
    this.delta.addUsedAt(child);

//    child.enforceRegularity();
  }

  /**
   * Creates the child nodes of this Delta formula.
   * @return a singleton collection of the child nodes.
   */
  protected Collection<Collection<TableauFormula>> createChildren() {
    Formula body =
      (Formula) ((FOLFormula) this.delta.getFormula()).getBody();
        
    Collection boundVars =
      ((FOLFormula) this.delta.getFormula()).getBoundVars();
    Collection freeVars =
      DeltaInverseFormula.createNewSequentVars(boundVars);
        
    Substitution s = Substitution.createSubstitution(boundVars,
                                                            freeVars);
        
    body = body.apply(s);
        
    return Collections.singleton(
      this.delta.createChildFormulaCollectionWithoutExpansion(body));
  }
    
  /**
   * Return a Collection of new sequent variables that will replace
   * the variables bound in this formula.  The terms in the return
   * value are in the same order as the variable they replace occur in
   * the boundVars list.  Also, the names of the new variables are
   * based on the names of the bound vars.
   * @return a Collection of new sequent variables that will replace
   * the variables bound in this formula.  The terms in the return
   * value are in the same order as the variable they replace occur in
   * the boundVars list.  Also, the names of the new variables are
   * based on the names of the bound vars.
   */
  private static Collection createNewSequentVars(Collection boundVars) {
    ArrayList value = new ArrayList(boundVars.size());
    Iterator bvIter = boundVars.iterator();
    do {
      Variable bv = (Variable) bvIter.next();
      value.add(Variable.createNewSequentVar(bv.getExternalName()));
    } while (bvIter.hasNext());
    return value;
  }
}
