package bitnots.prover;

import bitnots.tableaux.*;
import java.util.*;

/**
 * The AllDeltaInversesTask is used to reduce all Delta formulas using
 * the Delta Inverse rule, instead of the normal Delta rule.  This is
 * used in creating sequents that will go into a knowledge base.
 *
 * @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
 * @version 1.0
 */
public class AllDeltaInversesTask extends AbstractTableauTask {
  /**
   * Constructs an AllDeltaInversesTask with the tableau that needs
   * to have its Delta formulas expanded.
   * @param tab the Tableau to expand Delta formulas.
   */
  public AllDeltaInversesTask(Tableau tab) {
    super(tab);
  }

  /**
   * Gets all Delta formulas from the Tableau and then expands them
   * using the Delta Inverse rule.
   */
  public void run() {
    ArrayList deltas = (ArrayList) this.tableau.getDeltas();
    boolean retVal = false;
    while (!deltas.isEmpty() &&
           !Thread.currentThread().isInterrupted()) {
      Iterator it = ((Collection) deltas.clone()).iterator();
      while (it.hasNext()) {
        retVal = true;
        DeltaFormula delta = (DeltaFormula) it.next();
                
        DeltaInverseFormula deltaInverse =
          new DeltaInverseFormula(delta);
                
        deltaInverse.expand();
      }
    }
    // this.setChangedTableau(retVal);
  }
}
