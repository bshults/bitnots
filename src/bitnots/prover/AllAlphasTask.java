package bitnots.prover;

import bitnots.tableaux.*;

/**
 * AllAlphasTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class AllAlphasTask extends AbstractTableauTask {

  public AllAlphasTask(Tableau tab) {
    super(tab);
  }

  /**
   * Apply alpha-rules until there are no alpha formulas on the tree.
   * This relies on alphas being added to the end.  */
  public void run() {
    Prover.expandAllAlphas(this.tableau);
  }
  
}// AllAlphasTask
