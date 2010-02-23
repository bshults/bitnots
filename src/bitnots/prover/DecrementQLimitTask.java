package bitnots.prover;

import bitnots.tableaux.*;
import javax.swing.*;

/**
 * DecrementQLimitTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class DecrementQLimitTask extends AbstractTableauTask {

  public DecrementQLimitTask(Tableau tab) {
    super(tab);
  }

  /**
   * Simply call <b>backtrackingDecrementQLimit</b> on the tableau.
   * ChangedTableau will be true only if the tree is closed.  */
  public void run() {
    int qLimit = tableau.getQLimit();
    if (qLimit == 0)
      tableau.setQLimit(0);  // this serves to fire an event despite the lack of change
    else
      tableau.setQLimit(tableau.getQLimit() - 1);
  }

}// DecrementQLimitTask

