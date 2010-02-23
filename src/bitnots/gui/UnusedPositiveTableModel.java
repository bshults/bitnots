package bitnots.gui;

import java.util.ArrayList;

import bitnots.tableaux.TableauNode;
import bitnots.tableaux.TableauFormula;


/**
 * 
 * @author bshults
 *
 */
public class UnusedPositiveTableModel extends FormulaTableModel {

  public UnusedPositiveTableModel(TableauNode node) {
    super(node);
    this.data = new ArrayList<TableauFormula>(node.getUnusedHereNonliteralPositives());
  }
}
