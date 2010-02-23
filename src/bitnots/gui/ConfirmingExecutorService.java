package bitnots.gui;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import bitnots.eventthreads.SingleThreadExecutorService;


/**
 * 
 * @author bshults
 *
 */
public class ConfirmingExecutorService extends SingleThreadExecutorService {

  /**
   * 
   * @author bshults
   *
   */
  public class ConfirmingRunnable implements Runnable {
    private volatile Runnable runnable;
    private JDialog dialog;
    private JOptionPane pane;

    /**
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run() {
      // this is on the engine thread
      SwingUtilities.invokeLater(new Runnable() {
        public void run() {
          // close the dialog
          if (ConfirmingRunnable.this.pane.getValue() == JOptionPane.UNINITIALIZED_VALUE) {
            ConfirmingRunnable.this.pane.setValue(new Integer(JOptionPane.YES_OPTION));
            ConfirmingRunnable.this.dialog.dispose();
          }
        }
      });
      // TODO there is some concern that the dialog will be disposed before
      // it is visible.
      this.runnable.run();
    }
    public ConfirmingRunnable(Runnable r, JDialog dialog, JOptionPane pane) {
      this.runnable = r;
      this.dialog = dialog;
      this.pane = pane;
    }
    public void cancel() {
      this.runnable = new Runnable() {
        public void run() {}
      };
    }
  }

  public void executeWithPrompt(Runnable r) {
    if (!this.isBusy())
      this.execute(r);
    else {
      // create a dialog asking if the user wants to wait
      JOptionPane pane =
        new JOptionPane("The prover is busy making changes to the proof right now.  Do you want to wait?", 
                        JOptionPane.QUESTION_MESSAGE, JOptionPane.YES_NO_OPTION);
      JDialog dialog = pane.createDialog(Bitnots.FRAME, "Busy");
      // pass the dialog to the runnable
      ConfirmingRunnable runnable = new ConfirmingRunnable(r, dialog, pane);
      this.execute(runnable);
      // pop up the dialog
      dialog.setVisible(true);
      Object selectedValue = pane.getValue();
      if (selectedValue == null)
        selectedValue = new Integer(JOptionPane.CANCEL_OPTION);
      switch (((Integer) selectedValue).intValue()) {
      case JOptionPane.YES_OPTION:
        // let it go.
        break;
      default:
        // cancel it
        runnable.cancel();
      }
    }
  }
}
