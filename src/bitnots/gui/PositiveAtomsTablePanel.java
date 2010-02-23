package bitnots.gui;

import bitnots.tableaux.TableauNode;

/**
 * 
 * @author bshults
 *
 */
public class PositiveAtomsTablePanel extends FormulaTablePanel {
  
  public PositiveAtomsTablePanel(TableauNode node) {
    this.setModel(new AtomPositiveTableModel(node));
    // FIXME consider putting the new ones in a lightgreen background.
  }
}