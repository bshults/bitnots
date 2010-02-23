package bitnots.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.util.Arrays;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;

import bitnots.gui.MultiSplitLayout.Leaf;
import bitnots.gui.MultiSplitLayout.Node;
import bitnots.gui.MultiSplitLayout.Split;
import bitnots.gui.MultiSplitLayout.Divider;
import bitnots.gui.MultiSplitPane.DividerPainter;
import bitnots.tableaux.TableauNode;

/**
 * 
 * @author bshults
 *
 */
public class BranchViewTablesPane extends JPanel {
  FormulaTablePanel posAtoms;
  FormulaTablePanel negAtoms;
  private FormulaTablePanel posUnused;
  private FormulaTablePanel negUnused;
//  private Split model;
  
  public void resetModels(TableauNode node) {
    this.posAtoms.setModel(new AtomPositiveTableModel(node));
    this.negAtoms.setModel(new AtomNegativeTableModel(node));
    this.posUnused.setModel(new UnusedPositiveTableModel(node));
    this.negUnused.setModel(new UnusedNegativeTableModel(node));
    this.repaint();
  }
  /**
   * 
   * @param node
   */
  public BranchViewTablesPane(TableauNode node) {
    this.setLayout(new BorderLayout());
    final MultiSplitPane tables = new MultiSplitPane();
    this.add(tables);
    
    // set up popup menu
    
    JPopupMenu popup = new JPopupMenu();
    popup.add(new JMenuItem("Apply and Condense"));
    
    tables.addMouseListener(new PopupListener(popup));
    
    // label the top table
    JPanel north = new JPanel(new FlowLayout(FlowLayout.LEFT)) {
      @Override
      public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Color oldColor = g.getColor();
        g.setColor(Color.lightGray);
        g.fillRect(this.getBounds().x, this.getBounds().y, 
                   this.getBounds().width, this.getBounds().height);
      }
    };
    // north.setAlignmentY(LEFT_ALIGNMENT);
    north.setBorder(BorderFactory.createLineBorder(Color.black));
    north.add(new JLabel("Suppose (atoms)"));
    this.add(north, BorderLayout.NORTH);
    
    final Divider nAtomsD = new Divider();
    final Divider pUnusedD = new Divider();
    final Divider nUnusedD = new Divider();
    final Leaf nAtomsL = new Leaf("-atoms");
    final Leaf pAtomsL = new Leaf("+atoms");
    final Leaf nUnusedL = new Leaf("-unused");
    final Leaf pUnusedL = new Leaf("+unused");
    // Divider bottom = new Divider();
    List<Node> children = 
      Arrays.asList(pAtomsL, nAtomsD, nAtomsL, pUnusedD, pUnusedL, nUnusedD, 
                    nUnusedL);
    pAtomsL.setWeight(.3);
    nAtomsL.setWeight(.3);
    pUnusedL.setWeight(.2);
    nUnusedL.setWeight(.2);
    Split modelRoot = new Split();
//    this.model = modelRoot;
    modelRoot.setRowLayout(false);
    modelRoot.setChildren(children);
    
    tables.setDividerSize(20);
    
    tables.setDividerPainter(new DividerPainter() {
      @Override
      public void paint(Graphics g, Divider divider) {
        Color oldColor = g.getColor();
        g.setColor(Color.lightGray);
        g.fillRect(divider.getBounds().x, divider.getBounds().y,
                   divider.getBounds().width, divider.getBounds().height);
        g.setColor(Color.black);
        g.drawRect(divider.getBounds().x, divider.getBounds().y, 
                   divider.getBounds().width, divider.getBounds().height);
        g.setColor(oldColor);
        if (divider == nAtomsD) {
          g.drawString("Show (atoms)", 5, divider.getBounds().y + 15);
        } else if (divider == pUnusedD) {
          g.drawString("Suppose (unused)", 5, divider.getBounds().y + 15);
        } else if (divider == nUnusedD) {
          g.drawString("Show (unused)", 5, divider.getBounds().y + 15);
        }

      }
    });
    
    tables.getMultiSplitLayout().setModel(modelRoot);
    tables.add(new JScrollPane(this.posAtoms = FormulaTablePanel.createFormulaTable(node, "atoms", true)), "+atoms");
    tables.add(new JScrollPane(this.negAtoms = FormulaTablePanel.createFormulaTable(node, "atoms", false)), "-atoms");
    tables.add(new JScrollPane(this.posUnused = FormulaTablePanel.createFormulaTable(node, "unused", true)), "+unused");
    tables.add(new JScrollPane(this.negUnused = FormulaTablePanel.createFormulaTable(node, "unused", false)), "-unused");
  }
  
}
