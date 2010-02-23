package bitnots.gui;

import bitnots.tableaux.TableauNode;

/**
 * 
 * @author bshults
 *
 */
public class PositiveUnusedTablePanel extends FormulaTablePanel {
  
  public PositiveUnusedTablePanel(TableauNode node) {
    this.setModel(new UnusedPositiveTableModel(node));
    // FIXME consider putting the new ones in a lightgreen background.
  }
}