package bitnots.gui;

import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

import bitnots.theories.TheoremApplication;

/**
 *
 * @author bshults
 *
 */
public class TheoremAppView extends /* JTable */ JList {

  private ListCellRenderer defaultCellRenderer;

  public TheoremAppView() {
    this.defaultCellRenderer = this.getCellRenderer();
    this.setCellRenderer(new ListCellRenderer() {

      @Override
      public Component getListCellRendererComponent(JList list, Object value, int index,
                                                    boolean isSelected,
                                                    boolean cellHasFocus) {
        JLabel label =
               (JLabel) TheoremAppView.this.defaultCellRenderer.getListCellRendererComponent(list, value, index,
                                                                                             isSelected,
                                                                                             cellHasFocus);
        label.setText(((TheoremApplication) value).getSequent().toString());
        return label;
      }
    });
  }
}

