package bitnots.gui;

import java.util.ArrayList;

import bitnots.tableaux.TableauFormula;
import bitnots.tableaux.TableauNode;


/**
 * 
 * @author bshults
 *
 */
public class AtomPositiveTableModel extends FormulaTableModel {

  public AtomPositiveTableModel(TableauNode node) {
    super(node);
    this.data = new ArrayList<TableauFormula>(node.getAllPositiveLiterals());
  }

}
