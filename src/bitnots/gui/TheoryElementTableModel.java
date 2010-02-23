package bitnots.gui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.table.AbstractTableModel;

import bitnots.theories.TheoryElement;


public class TheoryElementTableModel extends AbstractTableModel {

  List<TheoryElement> elements;
  
  public TheoryElementTableModel(List<? extends TheoryElement> elts) {
    this.elements = new ArrayList<TheoryElement>(elts.size());
    this.elements.addAll(elts);
  }
  
  @Override
  public int getColumnCount() {
    return 2;
  }

  @Override
  public String getColumnName(int col) {
    switch (col) {
    case 0:
      return "Name";
    case 1:
      return "Formula";
    }
    return null;
  }

  @Override
  public int getRowCount() {
    return this.elements.size();
  }

  @Override
  public Object getValueAt(int row, int col) {
    switch (col) {
    case 0:
      return this.elements.get(row).getName();
    case 1:
      return this.elements.get(row).getFormula();
    }
    throw new IndexOutOfBoundsException();
  }
}
