package bitnots.gui;

import java.util.List;

import javax.swing.table.AbstractTableModel;

import bitnots.tableaux.TableauNode;
import bitnots.tableaux.TableauFormula;


/**
 * 
 * @author bshults
 *
 */
public abstract class FormulaTableModel extends AbstractTableModel {

  protected static final int COLUMN_COUNT = 2;
  protected List<TableauFormula> data;
  
  protected TableauNode branch;
  
  public FormulaTableModel(TableauNode node) {
    this.branch = node;
  }
  
  /**
   * @see javax.swing.table.TableModel#getRowCount()
   */
  @Override
  public int getRowCount() {
    return this.data.size();
  }

  @Override
  public String getColumnName(int col) {
    switch (col) {
    case 0:
      return "FOL Form";
    case 1:
      return "req subst";
    }
    return null;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  @Override
  public int getColumnCount() {
    // TODO Auto-generated method stub
    return FormulaTableModel.COLUMN_COUNT;
  }

  /* (non-Javadoc)
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  @Override
  public Object getValueAt(int rowIndex, int columnIndex) {
    switch (columnIndex) {
    case 0:
      return this.data.get(rowIndex);
    case 1:
      return this.data.get(rowIndex).getSubstitutionDependency();
    }
    throw new IndexOutOfBoundsException();
  }

}
