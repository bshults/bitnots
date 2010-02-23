package bitnots.prover;

import bitnots.tableaux.*;

/**
 * AllDeltasTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class AllDeltasTask extends AbstractTableauTask {

  public AllDeltasTask(Tableau tab) {
    super(tab);
  }

  /**
   * Applies delta-ruled until there are no delta-formulas on the
   * tableau.  This relies on deltas being added to the end.  */
  public void run() {
    Prover.expandAllDeltas(this.tableau);
  }
  
}// AllDeltasTask

