package bitnots.prover;

import bitnots.tableaux.*;
import javax.swing.*;

/**
 * AlertTask.java
 *
 *
 * Created: Thu Jan 30 10:14:05 2003
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class AlertTask extends AbstractTableauTask {

  private JFrame frame;

  public AlertTask(Tableau tab, JFrame frame) {
    super(tab);
    this.frame = frame;
  }

  /**
   * Simply call <b>backtrackingAlert</b> on the tableau.
   * ChangedTableau will be true only if the tree is closed.  */
  public void run() {
    if (tableau.getCloser() != null)
      JOptionPane.showMessageDialog(this.frame,
                                    "Done: " + tableau.getCloser());
    else {
      this.frame.getToolkit().beep();
      JOptionPane.showMessageDialog(this.frame, "No Proof!");
    }
  }
  
}// AlertTask

