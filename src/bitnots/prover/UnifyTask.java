package bitnots.prover;

import bitnots.tableaux.*;

/**
 * UnifyTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class UnifyTask extends AbstractTableauTask {

  public UnifyTask(Tableau tab) {
    super(tab);
  }

  /**
   * Simply call <b>backtrackingUnify</b> on the tableau.
   * ChangedTableau will be true only if the tree is closed.  */
  public void run() {
    this.tableau.backtrackingUnify();
  }
  
}// UnifyTask

