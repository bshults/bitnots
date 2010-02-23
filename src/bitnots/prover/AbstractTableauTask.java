package bitnots.prover;

import bitnots.tableaux.*;

/**
 * AbstractTableauTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public abstract class AbstractTableauTask implements Runnable {

  protected Tableau tableau;
  // private boolean changedTableau = false;

  /*protected void setChangedTableau(boolean ct) {
    this.changedTableau = ct;
  }*/

  /**
   * Once this is run, changedTableau will be true if this task
   * actually changed the tableau.
  public boolean hasChangedTableau() {
    return this.changedTableau;
  }
   */

  public AbstractTableauTask(Tableau tab) {
    this.tableau = tab;
  }

}// AbstractTableauTask

