package bitnots.gui;

import javax.swing.JTable;

import bitnots.tableaux.TableauNode;


/**
 * 
 * @author bshults
 *
 */
public class NegativeAtomsTablePanel extends FormulaTablePanel {
  
  public NegativeAtomsTablePanel(TableauNode node) {
    this.setModel(new AtomNegativeTableModel(node));
    // FIXME consider putting the new ones in a lightgreen background.
  }
}