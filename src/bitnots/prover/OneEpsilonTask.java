package bitnots.prover;

import bitnots.tableaux.*;

/**
 * OneEpsilonTask.java
 *
 * Created: Wed Jun 09 15:43:05 2004
 * @author Daniel W. Farmer
 * @version 2.0
 */

public class OneEpsilonTask
    extends AbstractTableauTask {

  public OneEpsilonTask(Tableau tab) {
    super(tab);
  }

  public void run() {
    // expand tree as much as possible with alphas, deltas,
    // gammas, and betas - when that is done, apply an epsilon;
    // then apply all the other formulas again
    if (Prover.multiProve(this.tableau))
      return;
    if (Prover.unifyEquality(this.tableau) != null)
      return;
    this.tableau.getEpsilonToolkit().applyOneEpsilon();
  }
} // OneEpsilonTask
