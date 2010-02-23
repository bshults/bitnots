package bitnots.gui;

import java.util.ArrayList;
import java.util.List;

import bitnots.tableaux.TableauFormula;
import bitnots.tableaux.TableauNode;


/**
 * 
 * @author bshults
 *
 */
public class UnusedNegativeTableModel extends FormulaTableModel {

  /**
   * 
   * @param node
   */
  public UnusedNegativeTableModel(TableauNode node) {
    super(node);
    this.data = new ArrayList<TableauFormula>(node.getUnusedHereNonliteralNegatives());
  }
}
