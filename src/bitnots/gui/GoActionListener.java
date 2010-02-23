package bitnots.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import bitnots.util.ImageUtils;


/**
 * 
 * @author bshults
 *
 */
public class GoActionListener extends AbstractAction {

  private ActionListener action;
  
  public GoActionListener(String toolTip, ActionListener l) {
    super("Go",
          ImageUtils.scaleImageIcon(new ImageIcon(GoActionListener.class.getResource("/bitnots/resources/Go.png")), 
                                    20, 20));
    this.action = l;
    this.putValue(Action.SHORT_DESCRIPTION, toolTip);
  }
  
  public void change(String toolTip, ActionListener l) {
    this.action = l;
    this.putValue(Action.SHORT_DESCRIPTION, toolTip);
  }
  
  @Override
  public final void actionPerformed(ActionEvent e) {
    this.action.actionPerformed(e);
  }
}
