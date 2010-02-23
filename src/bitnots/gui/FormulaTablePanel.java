package bitnots.gui;

import javax.swing.JTable;
import javax.swing.ListSelectionModel;

import bitnots.tableaux.TableauFormula;
import bitnots.tableaux.TableauNode;


/**
 * 
 * @author bshults
 *
 */
public class FormulaTablePanel extends JTable {
  
  /**
   * 
   * @param node
   * @param positive
   */
  public FormulaTablePanel() {
    super(null, new String[] {"FOL Form", "req subst"});
    this.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    this.setDefaultRenderer(TableauFormula.class, new FormulaTableCellRenderer());
  }

  /**
   * 
   * @param node
   * @param string
   * @param positive
   * @return
   */
  public static FormulaTablePanel createFormulaTable(TableauNode node,
                                                     String string,
                                                     boolean positive) {
    if (string.equals("atoms")) {
      if (positive) 
        return new PositiveAtomsTablePanel(node);
      else 
        return new NegativeAtomsTablePanel(node);
    } else if (string.equals("unused")){
      if (positive) 
        return new PositiveUnusedTablePanel(node);
      else
        return new NegativeUnusedTablePanel(node);
    }
    throw new IllegalArgumentException();
  }
}
