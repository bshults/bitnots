package bitnots.tableaux;

import bitnots.expressions.*;
import bitnots.theories.*;
import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class BranchCloser {

  /**
   * the Substitution that unifies the formulas involvedSplits.
   */
  private Substitution subst;

  /**
   * the highest node not above any of the involved formulas.
   */
  private TableauNode home;

  /**
   * The collection of theorems used to prove this.
   * @todo currently, this only seems to contain the sequents associated with
   * branch-closing TheoremApplications.
   */
  private Collection sequents;

  /** 
   * A Split is involved if one of the formulas born in one of the children of
   * the split's node is an ancestor of one of the formulas used in this.
   * This is sorted so that the highest one is first.
   */
  private SortedSet<TableauNode.Split> involvedSplits;

  /**
   * The negative formulas involved.
   */
  private Collection<TableauFormula> usedGoals;
  /**
   * The positive formulas involved.
   */
  private Collection<TableauFormula> usedHyps;

  public SortedSet<TableauNode.Split> getInvolvedSplits() {
    return this.involvedSplits;
  }
  
  /**
   * If split's node is null and split is contained in this BranchCloser,
   * then it is removed and this returns true.
   * 
   * @param split
   * @return true if split has a null node and is successfully removed.
   */
  public boolean removeSplit(TableauNode.Split split) {
    if (split.getNode() == null) {
      return this.involvedSplits.remove(split);
    }
    return false;
  }

  public Collection<TableauFormula> getGoals() {
    return Collections.unmodifiableCollection(this.usedGoals);
  }

  public Collection<TableauFormula> getHyps() {
    return Collections.unmodifiableCollection(this.usedHyps);
  }

  public TableauNode getHome() {
    return this.home;
  }

  public Substitution getSubst() {
    return this.subst;
  }

  @Override
  public String toString() {
    return this.getHome().path().toString() + "\nsubs: " +
      this.getSubst().toString();
  }

  /**
   * 
   * 
   * @param bc
   * @param node
   * @return true if node is a child of a parent with multiple children and
   * node contains a new formula that is an ancestor of a formula involved
   * in the receiver.  This is determined by testing whether <code>node</code>'s
   * parent is in the receiver's involvedSplits.
   */
  public boolean involves(TableauNode node) {
    for (TableauNode.Split split: this.getInvolvedSplits()) {
      if (split.getNode() == node.getParent())
        return true;
    }
    return false;
  }

  public BranchCloser(Substitution cache, Collection hyps,
                      Collection goals, TableauNode pn,
                      Collection sequents) {
    this.subst = cache;
    this.usedHyps = hyps;
    this.usedGoals = goals;
    this.home = pn;
    this.sequents = sequents;

    // create splits
    this.involvedSplits = new TreeSet();
    for (TableauFormula goal: this.usedGoals) {
      this.involvedSplits.addAll(goal.getInvolvedSplits());
    }
    for (TableauFormula hyp: this.usedHyps) {
      this.involvedSplits.addAll(hyp.getInvolvedSplits());
    }
  }
  /**
   * Creates a BranchCloser with indicating that the given
   * Substitution closes the branch <b>pn</b> and involves the given
   * positive formulas, <b>hyps</b>, and negative formulas,
   * <b>goals</b>.
   */
  public BranchCloser(Substitution cache, Collection hyps,
                      Collection goals, TableauNode pn) {
    this(cache, hyps, goals, pn, Collections.EMPTY_LIST);
  }
  
  /**
   * Creates a BranchCloser with indicating that the given
   * Substitution closes the branch <b>pn</b> and involves the given
   * positive formulas, <b>hyps</b>, and negative formulas,
   * <b>goals</b>.
   */
  public BranchCloser(Substitution cache, Collection hyps,
                      Collection goals, TableauNode pn, KBSequent sequent) {
    this(cache, hyps, goals, pn, Collections.singleton(sequent));
  }

  public Collection getSequents() {
    // TODO Auto-generated method stub
    return this.sequents;
  }
  
} // BranchCloser

