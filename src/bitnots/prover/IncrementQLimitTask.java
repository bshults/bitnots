package bitnots.prover;

import bitnots.tableaux.*;
import javax.swing.*;

/**
 * IncrementQLimitTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class IncrementQLimitTask extends AbstractTableauTask {

  public IncrementQLimitTask(Tableau tab) {
    super(tab);
  }

  /**
   * Simply call <b>backtrackingIncrementQLimit</b> on the tableau.
   * ChangedTableau will be true only if the tree is closed.  */
  public void run() {
    tableau.setQLimit(tableau.getQLimit() + 1);
  }
  
}// IncrementQLimitTask

