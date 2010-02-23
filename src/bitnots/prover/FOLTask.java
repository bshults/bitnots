package bitnots.prover;

import bitnots.tableaux.*;

/**
 * FOLTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class FOLTask extends AbstractTableauTask {

  public FOLTask(Tableau tab) {
    super(tab);
  }

  /**
   * Calls Prover.proveADBorG.
   */
  public void run() {
    Prover.proveADBorG(this.tableau);
  }
  
}// FOLTask

