package bitnots.prover;

import bitnots.tableaux.*;

/**
 * FOLEQUnifyTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class FOLEQUnifyTask extends AbstractTableauTask {

  public FOLEQUnifyTask(Tableau tab) {
    super(tab);
  }

  /**
   * Calls Prover.proveADBorG.
   */
  public void run() {
    Prover.proveEquality(this.tableau);
  }
  
}// FOLEQUnifyTask

