package bitnots.prover;

import java.util.*;
import javax.swing.*;
import javax.swing.tree.*;
import bitnots.tableaux.*;

/**
 * RefreshTreeTask.java
 * Expands a JTree using code from the
 * <a href = http://javaalmanac.com/egs/javax.swing.tree/ExpandAll.html>
 * Java Almanac</a>.
 * @author Daniel W. Farmer
 */

public class RefreshTreeTask extends AbstractTableauTask {

  private JTree jTree;

  public RefreshTreeTask(Tableau tab, JTree jTree) {
    super(tab);
    this.jTree = jTree;
  }

  public void run() {
    ((DefaultTreeModel) this.jTree.getModel()).reload();
    TreeNode root = (TreeNode) this.jTree.getModel().getRoot();
    RefreshTreeTask.expandAll(this.jTree, new TreePath(root));
  }

  /**
   * Used to expand the tree when it is refreshed.
   * Code taken from the
   * <a href = http://javaalmanac.com/egs/javax.swing.tree/ExpandAll.html>
   * Java Almanac</a>.
   * @param jTree The tree to expand.
   * @param parent The current path.
   */
  // TODO: this should not be on my task queue!
  private static void expandAll(JTree jTree, TreePath parent) {
    // Traverse children
    TreeNode node = (TreeNode) parent.getLastPathComponent();
    if (node.getChildCount() >= 0) {
      for (Enumeration e = node.children(); e.hasMoreElements(); ) {
        TreeNode n = (TreeNode) e.nextElement();
        TreePath path = parent.pathByAddingChild(n);
        RefreshTreeTask.expandAll(jTree, path);
      }
    }
    jTree.expandPath(parent);
  }
}// RefreshTreeTask

