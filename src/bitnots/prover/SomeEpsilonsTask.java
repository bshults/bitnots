package bitnots.prover;

import bitnots.tableaux.*;

/**
 * @author Benjamin Shults
 * @version
 */

public class SomeEpsilonsTask
    extends AbstractTableauTask {

  public SomeEpsilonsTask(Tableau tab) {
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
    if (this.tableau.getEpsilonToolkit().applySomeEpsilons())
      Prover.unifyEquality(this.tableau);
  }
}

