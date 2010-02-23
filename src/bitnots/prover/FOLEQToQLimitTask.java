package bitnots.prover;

import bitnots.tableaux.*;

/**
 * FOLEQToQLimitTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class FOLEQToQLimitTask extends AbstractTableauTask {

  public FOLEQToQLimitTask(Tableau tab) {
    super(tab);
  }

  /**
   * Calls Prover.proveADBorG.
   */
  public void run() {
    while (Prover.proveEquality(this.tableau));
  }
  
}// FOLEQToQLimitTask

