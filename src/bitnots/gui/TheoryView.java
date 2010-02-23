package bitnots.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import bitnots.tableaux.Tableau;
import bitnots.theories.KBSequent;
import bitnots.theories.Theory;
import bitnots.expressions.Formula;
import bitnots.gui.MultiSplitPane.DividerPainter;
import bitnots.gui.MultiSplitLayout.Divider;
import bitnots.gui.MultiSplitLayout.Node;
import bitnots.gui.MultiSplitLayout.Split;
import bitnots.gui.MultiSplitLayout.Leaf;

/**
 * 
 * @author bshults
 *
 */
public class TheoryView extends JPanel implements PropertyChangeListener {
  
  private Theory theory;
  private MultiSplitPane panes;
  private JPanel axioms;
  private JTable conjectures;

  public Dimension getPreferredSize() {
    return new Dimension(300, 600);
  }
  
  public TheoryView(Theory t) {
    super(new BorderLayout());
    this.theory = t;
    
    // label the top table
    JPanel north = new JPanel(new FlowLayout(FlowLayout.LEFT)) {
      @Override
      public void paintComponent(Graphics g) {
        super.paintComponent(g);
        // Color oldColor = g.getColor();
        g.setColor(Color.lightGray);
        g.fillRect(this.getBounds().x, this.getBounds().y, 
                   this.getBounds().width, this.getBounds().height);
      }
    };
    north.setBorder(BorderFactory.createLineBorder(Color.black));
    north.add(new JLabel("Axioms"));
    this.add(north, BorderLayout.NORTH);

    // set up MultiSplitPane
    
    this.panes = new MultiSplitPane();
    this.add(this.panes);
    
    final Divider conjecturesD = new Divider();
    final Leaf axiomsL = new Leaf("axioms");
    final Leaf conjecturesL = new Leaf("conjectures");

    List<Node> children = 
      Arrays.asList(axiomsL, conjecturesD, conjecturesL);
    axiomsL.setWeight(.3);
    conjecturesL.setWeight(.3);
    Split modelRoot = new Split();
    modelRoot.setRowLayout(false);
    modelRoot.setChildren(children);
    
    this.panes.setDividerSize(20);
    
    this.panes.setDividerPainter(new DividerPainter() {
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
        if (divider == conjecturesD) {
          g.drawString("Conjectures", 5, divider.getBounds().y + 15);
        }
      }
    });
    
    this.panes.getMultiSplitLayout().setModel(modelRoot);
    
    this.initData();
  }

  /**
   * Used to expand the tree when it is refreshed.
   * Code taken from the
   * <a href ="http://javaalmanac.com/egs/javax.swing.tree/ExpandAll.html">
   * Java Almanac</a>.
   * @param jTree The tree to expand.
   */
  public static void expandAll(JTree jTree) {
    TreePath parent = new TreePath(jTree.getModel().getRoot());
    // Traverse children
    TreeNode node = (TreeNode) parent.getLastPathComponent();
    if (node.getChildCount() >= 0) {
      for (Enumeration e = node.children(); e.hasMoreElements(); ) {
        TreeNode n = (TreeNode) e.nextElement();
        TreePath path = parent.pathByAddingChild(n);
        expandAll(jTree, path);
      }
    }
    jTree.expandPath(parent);
  }
  
  private static void expandAll(JTree jTree, TreePath parent) {
    // Traverse children
    TreeNode node = (TreeNode) parent.getLastPathComponent();
    if (node.getChildCount() >= 0) {
      for (Enumeration e = node.children(); e.hasMoreElements(); ) {
        TreeNode n = (TreeNode) e.nextElement();
        TreePath path = parent.pathByAddingChild(n);
        expandAll(jTree, path);
      }
    }
    jTree.expandPath(parent);
  }
  
  private void initData() {
    // set up grid of axioms and grid of conjectures.
    
    TheoryElementTableModel conjecturesList =
      new TheoryElementTableModel(this.theory.getConjectures());
    
    this.axioms = new JPanel(new GridLayout(this.theory.getKB().size(), 1, 10, 5));
    this.axioms.setBackground(Color.white);
    this.axioms.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    this.conjectures = new JTable(conjecturesList);

    // TODO have these expanded automatically
    DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
    renderer.setOpenIcon(null);
    renderer.setLeafIcon(null);

    for (KBSequent seq: this.theory.getKB()) {
      JTree tree = new JTree(new DefaultTreeModel(new KBSequentTree(seq)));
      this.axioms.add(tree);
      tree.setCellRenderer(renderer);
      TheoryView.expandAll(tree);
    }
    /* for (Conjecture conj: this.theory.getConjectures()) {
      conjecturesList.addElement(conj);
    }*/
    
    // add grids to MultiSplitPane.
    
    this.panes.add(new JScrollPane(this.axioms), "axioms");
    this.panes.add(new JScrollPane(this.conjectures), "conjectures");
    this.conjectures.setFillsViewportHeight(true);
    
    // TODO make right-click select a table item so that the popup can
    // apply to that item.
    
    // set up popup menu
    
    final JPopupMenu conjPopup = new JPopupMenu();
    conjPopup.add(new JMenuItem(new AbstractAction("Prove") {
      @Override
      public void actionPerformed(ActionEvent e) {
        Bitnots.FRAME.setTableau(new Tableau((Formula) TheoryView.this.conjectures.
                                             getModel().getValueAt(TheoryView.this.conjectures.getSelectedRow(), 1),
                                             TheoryView.this.theory));
        Bitnots.FRAME.setTheory(TheoryView.this.theory);
      }
    }));
    conjPopup.add(new JMenuItem("Add to KB"));
    conjPopup.add(new JMenuItem("View Details"));
    
    this.conjectures.addMouseListener(new MouseAdapter() {
      public void mousePressed(MouseEvent e) {
        maybeShowPopup(e);
      }
      
      public void mouseReleased(MouseEvent e) {
        maybeShowPopup(e);
      }
      
      private void maybeShowPopup(MouseEvent e) {
        if (e.isPopupTrigger()) {
          TheoryView.this.conjectures.changeSelection(TheoryView.this.conjectures.rowAtPoint(e.getPoint()), 0, false, false);
          conjPopup.show(e.getComponent(),
                         e.getX(), e.getY());
        }
      }
    });
    Bitnots.FRAME.addPropertyChangeListener("theory", this);
  }

  @Override
  public void propertyChange(PropertyChangeEvent e) {
    if (e.getPropertyName().equals("theory")) {
      if (((Bitnots) e.getSource()).getTheory() != this.theory) {
        this.theory = ((Bitnots) e.getSource()).getTheory();
        
      }
    }
  }
}
