package bitnots.prover;

import bitnots.tableaux.*;

/**
 * OneBetaTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class OneBetaTask extends AbstractTableauTask {

  public OneBetaTask(Tableau tab) {
    super(tab);
  }

  public void run() {
    Prover.expandOneBeta(this.tableau);
  }
  
}// OneBetaTask

