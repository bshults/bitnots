package bitnots.gui;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.NoSuchElementException;

import javax.swing.tree.TreeNode;

import bitnots.expressions.Formula;
import bitnots.theories.KBSequent;


/**
 * 
 * @author bshults
 *
 */
public class KBSequentTree implements TreeNode {

  
  /**
   * 
   * @author bshults
   *
   */
  public class FormulaTreeNode implements TreeNode {

    private Formula formula;
    private TreeNode parent;
    
    public FormulaTreeNode(Formula form, TreeNode parent) {
      this.formula = form;
      this.parent = parent;
    }

    public String toString() {
      return this.formula.toString();
    }
    
    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#children()
     */
    @Override
    public Enumeration children() {
      return new Enumeration() {
        @Override
        public boolean hasMoreElements() {
          return false;
        }
        @Override
        public Object nextElement() {
          return null;
        }
      };
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getAllowsChildren()
     */
    @Override
    public boolean getAllowsChildren() {
      // TODO Auto-generated method stub
      return false;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getChildAt(int)
     */
    @Override
    public TreeNode getChildAt(int childIndex) {
      // TODO Auto-generated method stub
      return null;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getChildCount()
     */
    @Override
    public int getChildCount() {
      // TODO Auto-generated method stub
      return 0;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getIndex(javax.swing.tree.TreeNode)
     */
    @Override
    public int getIndex(TreeNode node) {
      // TODO Auto-generated method stub
      return -1;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getParent()
     */
    @Override
    public TreeNode getParent() {
      // TODO Auto-generated method stub
      return this.parent;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#isLeaf()
     */
    @Override
    public boolean isLeaf() {
      // TODO Auto-generated method stub
      return true;
    }
  }
  /**
   * 
   * @author bshults
   *
   */
  public class ConcludeTreeNode extends SubTreeNode {

    public ConcludeTreeNode(List<Formula> forms) {
      super(forms);
    }
    
    @Override
    public String toString() {
      return "Conclude";
    }
  }
  
  private abstract class SubTreeNode implements TreeNode {
    List<FormulaTreeNode> formulas;
    
    public SubTreeNode(List<Formula> forms) {
      this.formulas = new ArrayList<FormulaTreeNode>(forms.size());
      for (Formula form: forms) {
        this.formulas.add(new FormulaTreeNode(form, SubTreeNode.this));
      }
    }
    
    @Override
    public abstract String toString();

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#children()
     */
    @Override
    public Enumeration children() {
      return new Enumeration<TreeNode>() {
        int index = 0;
        @Override
        public boolean hasMoreElements() {
          return this.index < SubTreeNode.this.formulas.size();
        }
        @Override
        public TreeNode nextElement() {
          if (this.index >= SubTreeNode.this.formulas.size())
            throw new NoSuchElementException();
          return SubTreeNode.this.formulas.get(this.index++);
        }
      };
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getAllowsChildren()
     */
    @Override
    public boolean getAllowsChildren() {
      return true;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getChildAt(int)
     */
    @Override
    public TreeNode getChildAt(int childIndex) {
      return this.formulas.get(childIndex);
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getChildCount()
     */
    @Override
    public int getChildCount() {
      return this.formulas.size();
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getIndex(javax.swing.tree.TreeNode)
     */
    @Override
    public int getIndex(TreeNode node) {
      // TODO Auto-generated method stub
      return this.formulas.indexOf(node);
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#getParent()
     */
    @Override
    public TreeNode getParent() {
      // TODO Auto-generated method stub
      return KBSequentTree.this;
    }

    /* (non-Javadoc)
     * @see javax.swing.tree.TreeNode#isLeaf()
     */
    @Override
    public boolean isLeaf() {
      return this.formulas.size() == 0;
    }
  }
  
  /**
   * 
   * @author bshults
   *
   */
  public class AssumeTreeNode extends SubTreeNode {

    public AssumeTreeNode(List<Formula> negatives) {
      super(negatives);
    }
    
    @Override
    public String toString() {
      return "Suppose";
    }
  }

  private KBSequent sequent;
  
  private AssumeTreeNode assume;
  private ConcludeTreeNode conclude;
  
  /**
   * The name of the sequent in bold face.
   */
  public String toString() {
    return "<html><b>" + this.sequent.getName() + "</b></html>";
  }
  
  @Override
  public Enumeration<TreeNode> children() {
    return new Enumeration<TreeNode>() {
      int nextCalled = 0;
      @Override
      public boolean hasMoreElements() {
        return this.nextCalled < 2;
      }
      @Override
      public TreeNode nextElement() {
        switch (this.nextCalled) {
        case 0:
          this.nextCalled++;
          return KBSequentTree.this.assume;
        case 1:
          this.nextCalled++;
          return KBSequentTree.this.conclude;
        default:
          throw new NoSuchElementException();
        }
      }
    };
  }

  @Override
  public boolean getAllowsChildren() {
    return true;
  }

  @Override
  public TreeNode getChildAt(int childIndex) {
    switch (childIndex) {
    case 0:
      return this.assume;
    case 1:
      return this.conclude;
    default:
      throw new IndexOutOfBoundsException();
    }
  }

  @Override
  public int getChildCount() {
    return 2;
  }

  @Override
  public int getIndex(TreeNode node) {
    if (node == this.assume)
      return 0;
    else if (node == this.conclude)
      return 1;
    else
      return -1;
  }

  @Override
  public TreeNode getParent() {
    return null;
  }

  @Override
  public boolean isLeaf() {
    // TODO Auto-generated method stub
    return false;
  }
  public KBSequentTree(KBSequent seq) {
    this.sequent = seq;
    this.assume = new AssumeTreeNode(seq.getPositives());
    this.conclude = new ConcludeTreeNode(seq.getNegatives());
  }
}