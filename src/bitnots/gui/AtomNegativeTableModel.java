package bitnots.gui;

import java.util.ArrayList;

import bitnots.tableaux.TableauNode;
import bitnots.tableaux.TableauFormula;


/**
 * 
 * @author bshults
 *
 */
public class AtomNegativeTableModel extends FormulaTableModel {

  public AtomNegativeTableModel(TableauNode node) {
    super(node);
    this.data = new ArrayList<TableauFormula>(node.getAllNegativeLiterals());
  }

}
