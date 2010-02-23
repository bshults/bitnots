package bitnots.gui;

import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

import bitnots.expressions.Formula;
import bitnots.expressions.Substitution;
import bitnots.tableaux.TableauFormula;


/**
 * 
 * @author bshults
 *
 */
public class FormulaTableCellRenderer extends JLabel implements TableCellRenderer {
  
  
  /**
   * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
   */
  @Override
  public Component getTableCellRendererComponent(JTable table, Object formula, boolean isSelected,
                                                 boolean hasFocus, int row, int col) {
    switch (col) {
    case 0:
      TableauFormula f = (TableauFormula) formula;
      if (row < 0)
        ;
      return new JLabel(f.toString());
    default:
      throw new IllegalArgumentException();
    }

  }

}
