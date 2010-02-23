package bitnots.gui;

import javax.swing.JTable;

import bitnots.tableaux.TableauNode;


/**
 * 
 * @author bshults
 *
 */
public class NegativeUnusedTablePanel extends FormulaTablePanel {
  
  public NegativeUnusedTablePanel(TableauNode node) {
    this.setModel(new UnusedNegativeTableModel(node));
    // FIXME consider putting the new ones in a lightgreen background.
  }
}