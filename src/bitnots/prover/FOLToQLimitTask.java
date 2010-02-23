package bitnots.prover;

import bitnots.tableaux.*;

/**
 * FOLToQLimitTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class FOLToQLimitTask extends AbstractTableauTask {

  public FOLToQLimitTask(Tableau tab) {
    super(tab);
  }

  /**
   * Calls Prover.proveADBorG.
   */
  public void run() {
    while(Prover.prove(this.tableau));
  }
  
}// FOLToQLimitTask

