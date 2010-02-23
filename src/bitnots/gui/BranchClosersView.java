package bitnots.gui;

import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

import bitnots.tableaux.BranchCloser;


/**
 * 
 * @author bshults
 *
 */
public class BranchClosersView extends /* JTable */ JList {
  
  private ListCellRenderer defaultCellRenderer;
  
  public BranchClosersView() {
    this.defaultCellRenderer = this.getCellRenderer();
    this.setCellRenderer(new ListCellRenderer() {
      @Override
      public Component getListCellRendererComponent(JList list, Object value, int index,
                                                    boolean isSelected,
                                                    boolean cellHasFocus) {
        JLabel label = 
          (JLabel) BranchClosersView.this.defaultCellRenderer.getListCellRendererComponent(list, value, index,
                                                                                           isSelected,
                                                                                           cellHasFocus);
        label.setText(((BranchCloser) value).getSubst().toString());
        return label;
      }
    });
  }
}

