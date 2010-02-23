package bitnots.prover;

import bitnots.tableaux.*;

/**
 * FOLUnifyTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class FOLUnifyTask extends AbstractTableauTask {

  public FOLUnifyTask(Tableau tab) {
    super(tab);
  }

  /**
   * Calls Prover.proveADBorG.
   */
  public void run() {
    Prover.prove(this.tableau);
  }
  
}// FOLUnifyTask

