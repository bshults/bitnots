package bitnots.prover;

import bitnots.tableaux.*;

/**
 * OneEpsilonTask.java
 *
 * Created: Wed Jun 09 15:43:05 2004
 * @author Daniel W. Farmer
 * @version 1.0
 */

public class EpsilonAllTheWayTask extends AbstractTableauTask {

  public EpsilonAllTheWayTask(Tableau tab) {
    super(tab);
  }

  @SuppressWarnings("empty-statement")
  public void run() {
    // expand tree as much as possible with alphas, deltas,
    // gammas, and betas - when that is done, apply an epsilon;
    // then start over

    while (true) {
      while (Prover.multiProve(this.tableau));

      if (Prover.unifyEquality(this.tableau) != null)
        return;

      if (this.tableau.getEpsilonToolkit().applySomeEpsilons())
        continue;
      else
        break;
    }
  }
}
