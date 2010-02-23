package bitnots.prover;

import bitnots.tableaux.*;

/**
 * OneGammaTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class OneGammaTask extends AbstractTableauTask {

  public OneGammaTask(Tableau tab) {
    super(tab);
  }

  public void run() {
    Prover.expandOneGamma(this.tableau);
  }
  
}// OneGammaTask

