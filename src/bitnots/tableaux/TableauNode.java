package bitnots.tableaux;

import java.io.*;
import java.util.*;
import java.util.Collections;

import javax.swing.event.EventListenerList;

import bitnots.expressions.*;
import bitnots.util.*;
import bitnots.theories.*;
import bitnots.equality.*;

/**
 * This is a source of SpliceEvents.  One is fired when some node is splice in
 * under this one.
 */
public class TableauNode implements Serializable, Comparable,
                                    Iterable<TableauNode> {

  /**
   * The Tableau of which this is a node.
   */
  private Tableau tableau;

  // TODO: consider going back to the way it was before: TableauNode had a
  // split field that was dealt with in the splice method.
  /**
   * This keeps track of a split in a tableau.
   * 
   * These are added to a TreeSet and intentionally don't override
   * equals or hashCode because I want these to be considered equal when
   * they are ==.
   * 
   * Splits are ordered top to bottom.  I know this is not a total
   * ordering but I don't care because I only use lists of them in which
   * it is a total ordering.
   * 
   * The node of a Split must have multiple children.
   * 
   * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
   * @version .2
   */
  public static class Split implements Comparable, SpliceListener {

    private TableauNode node;

    /**
     * @param o {@link Comparable}
     * @return {@link Comparable}
     */
    public int compareTo(Object o) {
      TableauNode.Split s = (TableauNode.Split) o;
      if (s == this || s.getNode() == this.getNode())
        return 0;
      else if (s.getNode() == null)
        return 1;
      else if (this.getNode() == null)
        return -1;
      else if (this.getNode().isAncestorOf(s.getNode()))
        return -1;
      else
        return 1;
    }

    /**
     * @return the TableauNode associated with this split.
     */
    public TableauNode getNode() {
      return this.node;
    }

    /**
     * Removes itself from listening to its old location and registers
     * interest in the new one.  Also changes the value of its node property.
     * @see bitnots.tableaux.SpliceListener#spliceOccurred(bitnots.tableaux.SpliceEvent)
     */
    @Override
    public void spliceOccurred(final SpliceEvent e) {
      e.getOldLocation().removeSpliceListener(this);
      this.node = e.getNewLocation();
      if (this.node == null) {
        // XXX: I would like to notify BranchClosers and MultiBranchClosers
        // and other interested parties that I am not a valid split
        // any more.
      } else {
        this.node.addSpliceListener(this);
      }
    }

    /**
     * @param node
     */
    public Split(TableauNode node) {
      this.node = node;
      node.addSpliceListener(this);
    }
  }// Split
  private EventListenerList listenerList;

  public void addSpliceListener(SpliceListener l) {
    if (this.listenerList == null)
      this.listenerList = new EventListenerList();
    this.listenerList.add(SpliceListener.class, l);
  }

  public void removeSpliceListener(SpliceListener l) {
    if (this.listenerList != null) {
      this.listenerList.remove(SpliceListener.class, l);
      if (this.listenerList.getListenerCount() == 0)
        this.listenerList = null;
    }
  }

  // Notify all listeners that have registered interest for
  // notification on this event type.  The event instance 
  // is lazily created using the parameters passed into 
  // the fire method.
  private void fireSpliceEvent(SpliceEvent e) {
    if (this.listenerList != null) {
      // Guaranteed to return a non-null array
      Object[] listeners = this.listenerList.getListenerList();
      // Process the listeners last to first, notifying
      // those that are interested in this event
      for (int i = listeners.length - 2; i >= 0; i -= 2) {
        if (listeners[i] == SpliceListener.class) {
          ((SpliceListener) listeners[i + 1]).spliceOccurred(e);
        }
      }
    }
  }

  /**
   * Inserts a new child node under the receiver containing the given formulas.
   * The children of the receiver become the children of the new child node.
   */
  public TableauNode spliceUnder(Collection<TableauFormula> forms) {
    TableauNode value = new TableauNode(this.tableau, forms);
    value.setChildren(this.children);
    this.setChildren(Collections.singletonList(value));
    this.fireSpliceEvent(new SpliceEvent(this, value));
    return value;
  }
  /**
   * The list of children of this node.
   */
  private List<TableauNode> children;
  /**
   * The parent TableauNode of this node.  This will be null if this
   * node is the root of a Tableau.
   */
  private TableauNode parent;
  private List<InitialMatch> initialMatches = new ArrayList<InitialMatch>();
  /**
   * The congruence closure for the positive equalities above and at
   * this node.
   */
  private DSTGraph congruenceClosure;
  /**
   * Negative formulas that were born here.
   */
  // I'm thinking I want these Collections to be something like my
  // ExposedLinkedLists.
  private Collection<TableauFormula> newGoals;
  /**
   * Positive formulas that were born here.
   */
  // I'm thinking I want these Collections to be something like my
  // ExposedLinkedLists.
  private Collection<TableauFormula> newHyps;
  /**
   * Negative equality predicates that were born here.
   */
  private Collection<TableauFormula> newEQGoals =
                                     new ArrayList<TableauFormula>();
  /**
   * Positive equality predicates that were born here.
   */
  private Collection<TableauFormula> newEQHyps = new ArrayList<TableauFormula>();
  /**
   * Collection of BranchClosers for which this is the highest
   * TableauNode at which they work.  Also, I'm thinking that this field
   * should be in a child class that overrides some of the unification
   * methods... See, I want to have different unification strategies
   * in the future.  Several of these fields would go into the child
   * class in that case.
   */
  private List<BranchCloser> branchClosers = new ArrayList<BranchCloser>();
  /**
   * True when passOne of the backtracking unifier has been through
   * this node.  State pattern might be better used here.
   */
  private boolean completelyUnified = false;
  /**
   * Nonnull if and only if this has more than one child.
   */
  // private Split split = null;
  /** False if not yet matched with theory */
  private boolean matchedWithTheory = false;
  /** The applied TheoremApplications which use the formulas here. */
  private List<TheoremApplication> theoremApplications;

  /**
   * Gets the DSTGraph stored at this TableauNode.
   * @return the congruence closure.
   */
  public DSTGraph getCongruenceClosure() {
    return this.congruenceClosure;
  }

  public String getPathString() {
    // TODO Auto-generated method stub
    return this.path().toString();
  }

  public Collection<TableauFormula> getNewEQGoals() {
    return Collections.unmodifiableCollection(this.newEQGoals);
  }

  public Collection<TableauFormula> getNewEQHyps() {
    return Collections.unmodifiableCollection(this.newEQHyps);
  }

  /**
   * Adds a child to the list of children.
   * @param child The new child to be added.
  private void addChild(TableauNode child) {
  this.children.add(child);
  }
   */
  /**
   * Gets the list of TheoremApplications that have been applied to this
   * node.
   * @return TAs that use a TableauFormula from this node
   */
  public List<TheoremApplication> getTheoremApplications() {
    if (this.theoremApplications == null)
      return Collections.emptyList();
    else
      return Collections.unmodifiableList(this.theoremApplications);
  }

  /**
   * Associates this node with an applied TheoremApplication which uses
   * one of this node's TableauFormulas.
   * @param ta the TheoremApplication that needs to be registered
   */
  public void addTheoremApplication(TheoremApplication ta) {
    if (this.theoremApplications == null)
      this.theoremApplications = new ArrayList<TheoremApplication>();
    this.theoremApplications.add(ta);
  }

  public boolean removeTheoremApplication(TheoremApplication ta) {
    if (this.theoremApplications == null) {
      return false;
    } else {
      return this.theoremApplications.remove(ta);
    }
  }

  /**
   * Used to tell this node if it's been matched with the theory.
   * @param b true when this node has been checked against
   * the theory for matches
   */
  public void setBeenMatchedWithTheory(boolean b) {
    this.matchedWithTheory = b;
  }

  /**
   * Returns whether or not this node has been checked against
   * the theory for InitialMatches.
   * @return true if this node has been checked for InitialMatches,
   * false otherwise.
   */
  public boolean hasBeenMatchedWithTheory() {
    return this.matchedWithTheory;
  }

  /**
   * Returns nonnull if this branch has more than one child.
   * @return nonnull if this branch has more than one child.
  public Split getSplit() {
  return this.split;
  }
   */

  /*
  // needs to be called from split constructor.
  void setSplit(Split s) {
  this.split = s;
  }
   */
  /**
   * Add an InitialMatch to the list of
   * InitialMatches involving formulas at this node.
   */
  public void addInitialMatch(InitialMatch im) {
    this.initialMatches.add(im);
  }

  /**
   * @return the list of InitialMatches that involve formulas
   * in this node.
   */
  public List<InitialMatch> getInitialMatches() {
    return this.initialMatches;
  }

  public boolean isFirstSib() {
    return this.parent == null || this == this.parent.children.get(0);
  }

  /**
   * Returns true if this is the last element of it's parent's list of children.
   * Currently this also returns true if it has no parent.
   *
   * @return true if this is the last element of it's parent's list of
   *   children. Currently this also returns true if it has no parent.
   */
  public boolean isLastSib() {
    return this.parent == null || this == this.parent.children.get(this.parent.children.
        size() - 1);
  }

  public List<BranchCloser> getBranchClosers() {
    return Collections.unmodifiableList(this.branchClosers);
  }

  /**
   * True if passOne has been through this node.
   */
  public boolean wasCompletelyUnified() {
    return this.completelyUnified;
  }

  public Collection<TableauFormula> getNewHyps() {
    return Collections.unmodifiableCollection(this.newHyps);
  }

  public Collection<TableauFormula> getNewGoals() {
    return Collections.unmodifiableCollection(this.newGoals);
  }

  /**
   * Returns a new Collection containing the newHyps at this node that
   * are not used.
   * @return a new Collection containing the newHyps at this node that
   * are not used.
   */
  public Collection<TableauFormula> getNewHypsUnusedAnywhere() {
    ArrayList<TableauFormula> value = new ArrayList<TableauFormula>();
    for (TableauFormula pf : this.newHyps) {
      if (!pf.isUsedAnywhere())
        value.add(pf);
    }
    return value;
  }

  public Collection getAllHypsUnusedHere() {
    TableauNode curr = this;
    ArrayList value = new ArrayList();
    do {
      Iterator it = curr.newHyps.iterator();
      while (it.hasNext()) {
        TableauFormula pf = (TableauFormula) it.next();
        if (!pf.isUsedAtAncestorOf(this))
          value.add(pf);
      }
      curr = curr.parent;
    } while (curr != null);
    return value;
  }

  /**
   * Used to get all unused positive formulas, including equality
   * formulas.
   * @param a Collection of all unused positives.
   */
  public Collection getAllUnusedPositives() {
    Collection hyps = new HashSet();
    hyps.addAll(this.getHypsUnusedAnywhere());
    hyps.addAll(this.getNewPositiveEQs());
    return hyps;
  }

  /**
   * Returns a new Collection containing the newHyps at this node that
   * are not used.  The ones that are new to the receiver will be first.
   * @return a new Collection containing the newHyps at this node that
   * are not used.  The ones that are new to the receiver will be first.
   */
  public List<TableauFormula> getAllPositiveLiterals() {
    TableauNode curr = this;
    ArrayList<TableauFormula> value = new ArrayList<TableauFormula>();
    do {
      Iterator<TableauFormula> it = curr.newHyps.iterator();
      while (it.hasNext()) {
        TableauFormula pf = it.next();
        if (pf instanceof NilopFormula || pf instanceof PredicateFormula)
          value.add(pf);
      }
      it = curr.newEQHyps.iterator();
      while (it.hasNext()) {
        TableauFormula pf = it.next();
        value.add(pf);
      }
      curr = curr.parent;
    } while (curr != null);
    return value;
  }

  public Collection getHypsUnusedAnywhere() {
    TableauNode curr = this;
    ArrayList value = new ArrayList();
    do {
      Iterator it = curr.newHyps.iterator();
      while (it.hasNext()) {
        TableauFormula pf = (TableauFormula) it.next();
        if (!pf.isUsedAnywhere())
          value.add(pf);
      }
      curr = curr.parent;
    } while (curr != null);
    return value;
  }

  /**
   * Returns a new Collection containing the newHyps at this node that
   * are not used.
   * @return a new Collection containing the newHyps at this node that
   * are not used.
   */
  public Collection<TableauFormula> getNewPositiveLiterals() {
    ArrayList<TableauFormula> value = new ArrayList<TableauFormula>();
    Iterator<TableauFormula> it = this.newHyps.iterator();
    while (it.hasNext()) {
      TableauFormula pf = it.next();
      if (pf instanceof NilopFormula || pf instanceof PredicateFormula)
        value.add(pf);
    }
    return value;
  }

  /**
   * @return something
   */
  public Collection<TableauFormula> getNewPositiveEQs() {
    return Collections.unmodifiableCollection(this.newEQHyps);
  }

  /**
   * Used to get all unused negative formulas, including equality
   * formulas.
   * @param a Collection of all unused negatives.
   */
  public Collection getAllUnusedNegatives() {
    Collection goals = new HashSet();
    goals.addAll(this.getGoalsUnusedAnywhere());
    goals.addAll(this.getNewNegativeEQs());
    return goals;
  }

  public Collection getUnusedHereNonliteralNegatives() {
    TableauNode curr = this;
    ArrayList value = new ArrayList();
    do {
      Iterator it = curr.newGoals.iterator();
      while (it.hasNext()) {
        TableauFormula pf = (TableauFormula) it.next();
        if (!pf.isUsedAtAncestorOf(this) && !(pf instanceof NilopFormula
                                              || pf instanceof PredicateFormula))
          value.add(pf);
      }
      curr = curr.parent;
    } while (curr != null);
    return value;
  }

  public Collection getUnusedHereNonliteralPositives() {
    TableauNode curr = this;
    ArrayList<TableauFormula> value = new ArrayList<TableauFormula>();
    do {
      Iterator<TableauFormula> it = curr.newHyps.iterator();
      while (it.hasNext()) {
        TableauFormula pf = it.next();
        if (!pf.isUsedAtAncestorOf(this) && !(pf instanceof NilopFormula
                                              || pf instanceof PredicateFormula))
          value.add(pf);
      }
      curr = curr.parent;
    } while (curr != null);
    return value;
  }

  /**
   * @return something
   */
  public Collection<TableauFormula> getNewNegativeEQs() {
    return Collections.unmodifiableCollection(this.newEQGoals);
  }

  /**
   * Returns a new Collection containing the newHyps at this node that
   * are not used.  The ones new to this node will be first.
   * @return a new Collection containing the newHyps at this node that
   * are not used.  The ones new to this node will be first.
   */
  public List<TableauFormula> getAllNegativeLiterals() {
    TableauNode curr = this;
    ArrayList<TableauFormula> value = new ArrayList<TableauFormula>();
    do {
      Iterator<TableauFormula> it = curr.newGoals.iterator();
      while (it.hasNext()) {
        TableauFormula pf = it.next();
        if (pf instanceof NilopFormula || pf instanceof PredicateFormula)
          value.add(pf);
      }
      it = curr.newEQGoals.iterator();
      while (it.hasNext()) {
        TableauFormula pf = it.next();
        value.add(pf);
      }
      curr = curr.parent;
    } while (curr != null);
    return value;
  }

  /**
   * Returns a new Collection containing the newGoals at this node that
   * are not used.
   * @return a new Collection containing the newGoals at this node that
   * are not used.
   */
  public Collection<TableauFormula> getNewNegativeLiterals() {
    ArrayList<TableauFormula> value = new ArrayList<TableauFormula>(this.newGoals.
        size());
    Iterator<TableauFormula> it = this.newGoals.iterator();
    while (it.hasNext()) {
      TableauFormula pf = it.next();
      if (pf instanceof NilopFormula || pf instanceof PredicateFormula)
        value.add(pf);
    }
    return value;
  }

  public Collection getGoalsUnusedAnywhere() {
    TableauNode curr = this;
    ArrayList value = new ArrayList();
    do {
      Iterator it = curr.newGoals.iterator();
      while (it.hasNext()) {
        TableauFormula pf = (TableauFormula) it.next();
        if (!pf.isUsedAnywhere())
          value.add(pf);
      }
      curr = curr.parent;
    } while (curr != null);
    return value;
  }

  public Collection getAllGoalsUnusedHere() {
    TableauNode curr = this;
    ArrayList value = new ArrayList();
    do {
      Iterator it = curr.newGoals.iterator();
      while (it.hasNext()) {
        TableauFormula pf = (TableauFormula) it.next();
        if (!pf.isUsedAtAncestorOf(this))
          value.add(pf);
      }
      curr = curr.parent;
    } while (curr != null);
    return value;
  }

  /**
   * Returns a new Collection containing the newGoals at this node that
   * are not used.
   * @return a new Collection containing the newGoals at this node that
   * are not used.
   */
  public Collection<TableauFormula> getNewGoalsUnusedAnywhere() {
    ArrayList<TableauFormula> value = new ArrayList<TableauFormula>(this.newGoals.
        size());
    Iterator<TableauFormula> it = this.newGoals.iterator();
    while (it.hasNext()) {
      TableauFormula pf = it.next();
      if (!pf.isUsedAnywhere())
        value.add(pf);
    }
    return value;
  }

  /**
   * This implements the preorder relation in which <b>a &lt; b</b>
   * if <b>a</b> is directly above or to the left of <b>b</b>.
   */
  public int compareTo(Object o) {
    TableauNode p = (TableauNode) o;
    if (this == p)
      return 0;
    else if (this.isBefore(p))
      return -1;
    else
      return 1;
  }

  public TableauNode getChildAt(int index) {
    return this.children.get(index);
  }

  public int getChildCount() {
    return this.children == null ? 0 : this.children.size();
  }

  public TableauNode getParent() {
    return this.parent;
  }

  public boolean isLeaf() {
    return (this.getChildCount() == 0);
  }

/*  public int getIndex(TableauNode n) {
    return this.children.indexOf(n);
  }*/

  /**
   * Returns the index of this node in its parent's list of children.
   * If it has no parent, then this returns -1.
   * @return the index of this node in its parent's list of children.
   * If it has no parent, then this returns -1.
   */
  public int getIndex() {
    if (this.parent == null)
      return -1;
    else
      return this.parent.children.indexOf(this);
  }

  /**
   * The obvious.  If I implement a height, then this could become
   * more efficient.
   */
  public boolean isAncestorOf(TableauNode n) {
    if (n == this)
      return true;
    else if (n.parent != null) {
      return this.isAncestorOf(n.parent);
    } else
      return false;
  }

  /**
   * Return true if the receiver is to the left of and neither an
   * ancestor nor a descendant of <code>p</code>.
   */
  public boolean isToTheLeftOf(TableauNode p) {
    // TODO: implement
    throw new UnsupportedOperationException();
//    return false;
  }

  /*
   * This works but runs in time O(h).  That seems better than the
   * alternative.  Wait, there is an alternative in which TableauNodes
   * have paths that share structure.  That way, when one higher up
   * changes, all those below it change as well.  OK, that seems
   * better that what I have now.  TODO: fix it: make path a field and
   * have them share structure with the parent's path.  Oh, but to do
   * this, I would have to reverse them...  Still thinking.  How about
   * this, each is its own ExposedLinkedList but the
   * ExposedLinkedLists share Nodes!  But then I would have to have
   * the search algorithms watch for the tail reference. */
  ExposedLinkedList path() {
    ExposedLinkedList path = new ExposedLinkedList();
    TableauNode curr = this;
    for (TableauNode parent = curr.parent; parent != null; curr = parent, parent =
                                                                          curr.parent) {
      path.push(new Integer(parent.children.indexOf(curr)));
    }
    return path;
  }

  /**
   * The receiver is equal to, to the left of or above <code>p</code>.
   * I.e., before in the preorder.
   */
  public boolean isBefore(TableauNode p) {
    if (p == this)
      return true;
    for (ExposedLinkedList loc1 = this.path(), loc2 = p.path();
         !loc1.isEmpty();) {
      if (loc2.isEmpty())
        // p is an ancestor of the receiver.
        return false;
      int comp = ((Integer) loc1.pop()).compareTo((Integer) loc2.pop());
      if (comp > 0)
        // the receiver is to the right of p
        return false;
      else if (comp < 0)
        // the receiver is to the left of p
        return true;
    }
    // the receiver is an ancestor of p.
    return true;
  }

  // TODO: if I had a constant-time getLevel, this could halt sooner.
  public boolean isDescentantOf(TableauNode p) {
    TableauNode current = this;
    while (true) {
      if (current == p)
        return true;
      else if (current.parent != null)
        current = current.parent;
      else
        return false;
    }
  }

  public void applyToLeaves(Operator op) {
    // TODO what if the operation adds children to the leaf or modifies
    // it in some other way?
    for (TableauNode node : this)
      if (node.isLeaf())
        op.exec(node);
  }

  void addTrivialCloser(BranchCloser b) {
    // TODO: fix
  }

  /**
   * This assumes that <b>afx</b> is a delta_0-formula.
   * This assumes that <b>afx</b> has only one parent.  This also
   * assumes that the top-level quantifier in <b>afx</b> binds only
   * one variable.
   * @return if <b>afx</b> and <b>at</b> match except that there is a
   * term <b>t</b> that occurs in <b>at</b> everywhere the new skolem
   * term occurs in <b>afx</b>, then this returns <b>t</b>.
   * Otherwise, null. */
  // TODO: test
  private static Term disagreementPair(TableauFormula afx, Formula at) {
    FOLFormula parent =
               (FOLFormula) ((TableauFormula) afx.parents.iterator().next()).
        getFormula();
    Formula ax = parent.getBody();
    Variable x = (Variable) parent.getBoundVars().iterator().next();
    return Formula.disagreement(ax, x, at);
  }

  /**
   * This copies formulas containing skolem and replaces skolem with
   * replacement in the copies.  These copies are then added to a new
   * node which is spliced into the tree.
   */
  public void applyBrownsRule(Function skolem, Term replacement) {
    // TODO: implement
  }

  /**
   * Finds all branch closers at every descendant of the receiver and
   * stores them at the highest node at which all formulas involved
   * occur.
   */
  public void backtrackingPassOne() {
    Stack<BranchData> s = new Stack<BranchData>();
    s.push(this.new BranchData());
    do {
      BranchData bd = s.pop();
      TableauNode current = bd.getNode();
      Collection<TableauFormula> newLiteralGoals =
                                 current.getNewNegativeLiterals();
      Collection<TableauFormula> newLiteralHyps =
                                 current.getNewPositiveLiterals();
      if (current.wasCompletelyUnified()) {
        Substitution subst;
        for (TableauFormula goal : newLiteralGoals) {
          subst = goal.getSubstitutionDependency();
          this.unifyGoalWithCollection(goal, bd.nHyps, subst);
        }
        for (TableauFormula hyp : newLiteralHyps) {
          subst = hyp.getSubstitutionDependency();
          this.unifyHypWithCollection(hyp, bd.nGoals, subst);
        }
        if (!this.children.isEmpty()) {
          // since we've been here before, newHyps get added to cHyps,
          // since cHyps is the list of TableauFormulas that have been
          // seen by pass one before.
          Collection<TableauFormula> newH;
          if (!newLiteralHyps.isEmpty()) {
            newH = new ArrayList<TableauFormula>(bd.cHyps.size() + newLiteralHyps.
                size());
            newH.addAll(bd.cHyps);
            newH.addAll(newLiteralHyps);
          } else
            newH = bd.cHyps;
          Collection<TableauFormula> newG;
          if (!newLiteralGoals.isEmpty()) {
            newG = new ArrayList<TableauFormula>(bd.cGoals.size() + newLiteralGoals.
                size());
            newG.addAll(bd.cGoals);
            newG.addAll(newLiteralGoals);
          } else
            newG = bd.cGoals;
          for (TableauNode node : this.children) {
            s.push(node.new BranchData(newH, newG, bd.nHyps, bd.nGoals));
          }
        }
      } else {
        this.completelyUnified = true;
        Substitution subst;
        for (TableauFormula goal : newLiteralGoals) {
          subst = goal.getSubstitutionDependency();
          this.unifyGoalWithCollection(goal, bd.nHyps, subst);
          this.unifyGoalWithCollection(goal, bd.cHyps, subst);
        }
        for (TableauFormula hyp : newLiteralHyps) {
          subst = hyp.getSubstitutionDependency();
          this.unifyHypWithCollection(hyp, newLiteralGoals, subst);
          this.unifyHypWithCollection(hyp, bd.cGoals, subst);
          this.unifyHypWithCollection(hyp, bd.nGoals, subst);
        }
        // recurse:
        if (!this.children.isEmpty()) {
          // since we've not been here before, newHyps get added to
          // nHyps, since nHyps is the list of TableauFormulas that have
          // not been seen by pass one before.
          Collection<TableauFormula> newH;
          if (!newLiteralHyps.isEmpty()) {
            newH = new ArrayList<TableauFormula>(bd.nHyps.size() + newLiteralHyps.
                size());
            newH.addAll(bd.nHyps);
            newH.addAll(newLiteralHyps);
          } else
            newH = bd.nHyps;
          Collection<TableauFormula> newG;
          if (!newLiteralGoals.isEmpty()) {
            newG = new ArrayList<TableauFormula>(bd.nGoals.size() + newLiteralGoals.
                size());
            newG.addAll(bd.nGoals);
            newG.addAll(newLiteralGoals);
          } else
            newG = bd.nGoals;
          for (TableauNode node : this.children) {
            s.push(node.new BranchData(bd.cHyps, bd.cGoals, newH, newG));
          }
        }
      }
    } while (!s.isEmpty());
  }

  /**
   * Handles data for the recursive nature of the pass-one algorithm.
   * @author bshults
   *
   */
  private class BranchData {

    // the unused hyps on the current branch from nodes which were
    // present the last time this was called on this Tableau.
    Collection<TableauFormula> cHyps;
    // the unused goals on the current branch from nodes which were
    // present the last time this was called on this Tableau.
    Collection<TableauFormula> cGoals;
    // the new hyps on this branch since the last call to this method.
    Collection<TableauFormula> nHyps;
    // the new goals on this branch since the last call to this
    // method.
    Collection<TableauFormula> nGoals;

    TableauNode getNode() {
      return TableauNode.this;
    }

    @SuppressWarnings("unchecked")
    BranchData() {
      this(Collections.EMPTY_SET,
           Collections.EMPTY_SET,
           Collections.EMPTY_SET,
           Collections.EMPTY_SET);
    }

    BranchData(Collection<TableauFormula> cHyps,
               Collection<TableauFormula> cGoals,
               Collection<TableauFormula> nHyps,
               Collection<TableauFormula> nGoals) {
      this.cHyps = cHyps;
      this.cGoals = cGoals;
      this.nHyps = nHyps;
      this.nGoals = nGoals;
    }
  }

  /**
   * Handles data for the recursive nature of the pass-one algorithm.
   * @author bshults
   *
   */
  private class BranchDataEQ extends BranchData {

    // the equality hyps on the current branch from nodes which were
    // present the last time this was called on this Tableau.
    Collection<TableauFormula> cEQHyps;
    // the equality on the current branch from nodes which were
    // present the last time this was called on this Tableau.
    Collection<TableauFormula> cEQGoals;
    // the new equality hyps on this branch since the last call to
    // this method.
    Collection<TableauFormula> nEQHyps;
    // the new equality goals on this branch since the last call to
    // this method.
    Collection<TableauFormula> nEQGoals;
    DSTGraph cc;

    @SuppressWarnings("unchecked")
    BranchDataEQ(DSTGraph cc) {
      this(Collections.EMPTY_SET,
           Collections.EMPTY_SET,
           Collections.EMPTY_SET,
           Collections.EMPTY_SET,
           Collections.EMPTY_SET,
           Collections.EMPTY_SET,
           Collections.EMPTY_SET,
           Collections.EMPTY_SET,
           cc);
    }

    BranchDataEQ(Collection<TableauFormula> cHyps,
                 Collection<TableauFormula> cGoals,
                 Collection<TableauFormula> nHyps,
                 Collection<TableauFormula> nGoals,
                 Collection<TableauFormula> cEQHyps,
                 Collection<TableauFormula> cEQGoals,
                 Collection<TableauFormula> nEQHyps,
                 Collection<TableauFormula> nEQGoals,
                 DSTGraph cc) {
      super(cHyps, cGoals, nHyps, nGoals);
      this.cEQHyps = cEQHyps;
      this.cEQGoals = cEQGoals;
      this.nEQHyps = nEQHyps;
      this.nEQGoals = nEQGoals;
      this.cc = cc;
    }
  }

  /**
   * Finds all branch closers at every descendant of the receiver and
   * stores them at the highest node at which all formulas involved
   * occur.  This handles equality.
   * @param cc the congruence closure
   */
  // TODO: unify terms in negative equalities.
  // TODO: create a congruence closure for each new positive equality
  // and apply that equality to congruence closures below it.
  public void backtrackingPassOneEquality(DSTGraph cc) {
    Stack<BranchDataEQ> s = new Stack<BranchDataEQ>();
    s.push(this.new BranchDataEQ(cc));
    do {
      BranchDataEQ bd = s.pop();
      TableauNode current = bd.getNode();
      Collection<TableauFormula> newLiteralGoals =
                                 current.getNewNegativeLiterals();
      Collection<TableauFormula> newLiteralHyps =
                                 current.getNewPositiveLiterals();
      if (current.wasCompletelyUnified()) {
        // This assumes that this is not the first time this node has
        // been exposed to pass one.
        if (current.congruenceClosure != null) {
          if (!bd.nEQHyps.isEmpty()) {
            current.congruenceClosure.addAll(bd.nEQHyps);
          }
          // TODO: here I should probably retry everything!  See bug 354.
          cc = current.congruenceClosure;
        }
        Substitution subst;
        for (TableauFormula goal : newLiteralGoals) {
          subst = goal.getSubstitutionDependency();
          current.unifyGoalWithCollection(goal, bd.nHyps, subst, cc);
        }
        for (TableauFormula hyp : newLiteralHyps) {
          subst = hyp.getSubstitutionDependency();
          current.unifyHypWithCollection(hyp, bd.nGoals, subst, cc);
        }
        // recurse:
        // add
        if (!current.children.isEmpty()) {
          // since we've been here before, newLiteralHyps get added to cHyps,
          // since cHyps is the list of TableauFormulas that have been
          // seen by pass one before.
          Collection<TableauFormula> newH;
          if (!newLiteralHyps.isEmpty()) {
            newH = new ArrayList<TableauFormula>(bd.cHyps.size() + newLiteralHyps.
                size());
            newH.addAll(bd.cHyps);
            newH.addAll(newLiteralHyps);
          } else
            newH = bd.cHyps;
          Collection<TableauFormula> newG;
          if (!newLiteralGoals.isEmpty()) {
            newG = new ArrayList<TableauFormula>(bd.cGoals.size() + newLiteralGoals.
                size());
            newG.addAll(bd.cGoals);
            newG.addAll(newLiteralGoals);
          } else
            newG = bd.cGoals;
          Collection<TableauFormula> newEQH;
          if (!current.newEQHyps.isEmpty()) {
            newEQH =
            new ArrayList<TableauFormula>(bd.cEQHyps.size() + current.newEQHyps.
                size());
            newEQH.addAll(bd.cEQHyps);
            newEQH.addAll(current.newEQHyps);
          } else
            newEQH = bd.cEQHyps;
          Collection<TableauFormula> newEQG;
          if (!current.newEQGoals.isEmpty()) {
            newEQG =
            new ArrayList<TableauFormula>(bd.cEQGoals.size() + current.newEQGoals.
                size());
            newEQG.addAll(bd.cEQGoals);
            newEQG.addAll(current.newEQGoals);
          } else
            newEQG = bd.cEQGoals;
          for (int i = current.children.size() - 1; i >= 0; i--) {
            TableauNode node = current.getChildAt(i);
            s.push(node.new BranchDataEQ(newH, newG, bd.nHyps, bd.nGoals, newEQH,
                                         newEQG, bd.nEQHyps, bd.nEQGoals, cc));
          }
        }
      } else {
        // current.branchClosers.addAll(current.theoremClosers);
        current.completelyUnified = true;
        // This assumes that this is the first time this node has
        // been exposed to pass one.
        if (current.newEQHyps != null && current.newEQHyps.size() > 0) {
          // Add equalities from newEQHyps into cc.
          DSTGraph newCC = (DSTGraph) cc.clone();
          newCC.addAll(current.newEQHyps);
          current.congruenceClosure = newCC;
          cc = newCC;
          // TODO: should this indicate that we need to RETRY to unify
          // everything... I mean, try to unify pairs from up above that
          // failed to unify before?  It seems so.  See bug 354.
        }
        if (current.newEQGoals != null && current.newEQGoals.size() > 0) {
          current.unifyGoalEquations(current.newEQGoals, cc);
        }
        Substitution subst;
        for (TableauFormula goal : newLiteralGoals) {
          subst = goal.getSubstitutionDependency();
          current.unifyGoalWithCollection(goal, bd.nHyps, subst, cc);
          current.unifyGoalWithCollection(goal, bd.cHyps, subst, cc);
        }
        for (TableauFormula hyp : newLiteralHyps) {
          subst = hyp.getSubstitutionDependency();
          current.unifyHypWithCollection(hyp, newLiteralGoals, subst, cc);
          current.unifyHypWithCollection(hyp, bd.cGoals, subst, cc);
          current.unifyHypWithCollection(hyp, bd.nGoals, subst, cc);
        }
        // recurse:
        if (!current.children.isEmpty()) {
          // since we've not been here before, newLiteralHyps get added to
          // nHyps, since nHyps is the list of TableauFormulas that have
          // not been seen by pass one before.
          Collection<TableauFormula> newH;
          if (!newLiteralHyps.isEmpty()) {
            newH = new ArrayList<TableauFormula>(bd.nHyps.size() + newLiteralHyps.
                size());
            newH.addAll(bd.nHyps);
            newH.addAll(newLiteralHyps);
          } else
            newH = bd.nHyps;
          Collection<TableauFormula> newG;
          if (!newLiteralGoals.isEmpty()) {
            newG = new ArrayList<TableauFormula>(bd.nGoals.size() + newLiteralGoals.
                size());
            newG.addAll(bd.nGoals);
            newG.addAll(newLiteralGoals);
          } else
            newG = bd.nGoals;
          Collection<TableauFormula> newEQH;
          if (!current.newEQHyps.isEmpty()) {
            newEQH = new ArrayList<TableauFormula>(bd.nEQHyps.size() + current.newEQHyps.
                size());
            newEQH.addAll(bd.nEQHyps);
            newEQH.addAll(current.newEQHyps);
          } else
            newEQH = bd.nEQHyps;
          Collection<TableauFormula> newEQG;
          if (!current.newEQGoals.isEmpty()) {
            newEQG = new ArrayList<TableauFormula>(bd.nEQGoals.size() + current.newEQGoals.
                size());
            newEQG.addAll(bd.nEQGoals);
            newEQG.addAll(current.newEQGoals);
          } else
            newEQG = bd.nEQGoals;
          for (int i = current.children.size() - 1; i >= 0; i--) {
            TableauNode node = current.getChildAt(i);
            s.push(node.new BranchDataEQ(bd.cHyps, bd.cGoals, newH, newG,
                                         bd.cEQHyps, bd.cEQGoals, newEQH,
                                         newEQG, cc));
          }
        }
      }
    } while (!s.isEmpty());
  }

  /**
   * This returns the list of subtableaus of the splits of
   * <code>bc</code> unless some tableau to the left of the receiver is
   * not killed by <code>bc</code>.  If something is not killed then
   * this returns null.  The return value is sorted in preorder:
   * top-to-bottom, left-to-right.  This basically assumes that it is
   * being called by backtrackingPassTwo.  I.e. that the receiver is
   * the home of <code>bc</code>.
   */
  public SortedSet closeDownIfKillAllToLeft(BranchCloser bc) {
    return null;
  }

  /**
   * This simply fills in the branch closer slot of this node if a
   * unification is successful.
   * @param s a Substitution already taking the substitutionDependency
   * of <code>f</code> into account.
   */
  // TODO: the goal param would be unnecessary if ...
  void unifyGoalWithCollection(TableauFormula f, Collection c,
                               Substitution s) {
    Iterator it = c.iterator();
    while (it.hasNext()) {
      TableauFormula elt = (TableauFormula) it.next();
      Substitution tempSubst =
                   Substitution.compatible(elt.getSubstitutionDependency(), s);
      if (tempSubst != null) {
        tempSubst = f.getFormula().unify(elt.getFormula(), tempSubst);
        if (tempSubst != null)
          this.createBranchCloser(tempSubst,
                                  Collections.singleton(elt),
                                  Collections.singleton(f));
      }
    }
  }

  void unifyGoalWithCollection(TableauFormula f, Collection c,
                               Substitution s, DSTGraph cc) {
    Iterator it = c.iterator();
    while (it.hasNext()) {
      TableauFormula elt = (TableauFormula) it.next();
      Substitution tempSubst =
                   Substitution.compatible(elt.getSubstitutionDependency(), s);
      if (tempSubst != null) {
        tempSubst = f.getFormula().unify(elt.getFormula(), tempSubst, cc);
        if (tempSubst != null)
          this.createBranchCloser(tempSubst,
                                  Collections.singleton(elt),
                                  Collections.singleton(f), cc);
      }
    }
  }

  /**
   * This simply fills in the branch closer slot of this node if a
   * unification between the terms of one of the negative equalities
   * in <code>disEqns</code> is successful.
   */
  // TODO: the goal param would be unnecessary if ...
  void unifyGoalEquations(Collection disEqns, DSTGraph cc) {
    Iterator it = disEqns.iterator();
    while (it.hasNext()) {
      TableauFormula f = (TableauFormula) it.next();
      Predicate disEqn = (Predicate) f.getFormula();
      Iterator terms = disEqn.arguments();
      while (terms.hasNext()) {
        Term t1 = (Term) terms.next();
        Term t2 = (Term) terms.next();
        Substitution s =
                     t1.unifyApplied(t2, f.getSubstitutionDependency(), cc);
        // TODO: something like this:
//      Substitution subst = cc.equivalent(disEqn);
        // TODO: or unify the two terms sending cc along.
//      if (cc.equivalent(disEqn))
        if (s != null) {
          this.createBranchCloser(s, Collections.EMPTY_LIST,
                                  Collections.singleton(f), cc);
        }
      }
    }
  }

  /**
   * This simply fills in the branch closer slot of this node if a
   * unification between the terms of one of the negative equalities
   * in <code>disEqns</code> is successful.
   */
  // TODO: the goal param would be unnecessary if ...
  void unifyGoalEquations(Collection disEqns) {
    Iterator it = disEqns.iterator();
    while (it.hasNext()) {
      TableauFormula f = (TableauFormula) it.next();
      Predicate disEqn = (Predicate) f.getFormula();
      Iterator args = disEqn.arguments();
      Term t1 = (Term) args.next();
      Term t2 = (Term) args.next();
      Substitution subst =
                   t1.unifyApplied(t2, f.getSubstitutionDependency());
      if (subst != null)
        this.createBranchCloser(subst,
                                Collections.EMPTY_LIST,
                                Collections.singleton(f));
    }
  }

  /**
   * This simply fills in the branch closer slot of this node if a
   * unification is successful.
   * @param s A Substitution that already had the
   * substitutionDependency of <code>f</code> taken into account.
   */
  // TODO: the goal param would be unnecessary if ...
  void unifyHypWithCollection(TableauFormula f, Collection c,
                              Substitution s) {
    Iterator it = c.iterator();
    while (it.hasNext()) {
      TableauFormula elt = (TableauFormula) it.next();
      Substitution tempSubst =
                   Substitution.compatible(elt.getSubstitutionDependency(), s);
      if (tempSubst != null) {
        tempSubst = f.getFormula().unify(elt.getFormula(), tempSubst);
        if (tempSubst != null)
          this.createBranchCloser(tempSubst,
                                  Collections.singleton(f),
                                  Collections.singleton(elt));
      }
    }
  }

  void unifyHypWithCollection(TableauFormula f, Collection c,
                              Substitution s, DSTGraph cc) {
    Iterator it = c.iterator();
    while (it.hasNext()) {
      TableauFormula elt = (TableauFormula) it.next();
      Substitution tempSubst =
                   Substitution.compatible(elt.getSubstitutionDependency(), s);
      if (tempSubst != null) {
        tempSubst = f.getFormula().unify(elt.getFormula(), tempSubst, cc);
        if (tempSubst != null)
          this.createBranchCloser(tempSubst,
                                  Collections.singleton(f),
                                  Collections.singleton(elt), cc);
      }
    }
  }

  /**
   * Creates a BranchCloser and adds it to the list of BranchClosers
   * for this node and to the list of trivial-closures if it is
   * trivial.
   */
  void createBranchCloser(Substitution s, Collection usedHyps,
                          Collection usedGoals) {
    BranchCloser bc = new BranchCloser(s, usedHyps, usedGoals, this);
    this.addBranchCloser(bc);
    if (s.isEmpty())
      this.addTrivialCloser(bc);
  }

  /**
   * Creates a BranchCloser and adds it to the list of BranchClosers
   * for this node and to the list of trivial-closures if it is
   * trivial.
   */
  void createBranchCloser(Substitution s, Collection usedHyps,
                          Collection usedGoals, DSTGraph cc) {
    ArrayList usedHs = new ArrayList(usedHyps);
    usedHs.addAll(cc.getTableauFormulas());
    BranchCloser bc = new BranchCloser(s, usedHs, usedGoals, this);
    // TODO: if it becomes possible that equalities from lower in the
    // tree may be used, then we will need to add this BranchCloser to
    // a lower node, in general.
    this.addBranchCloser(bc);
    if (s.isEmpty())
      this.addTrivialCloser(bc);
  }

  /**
   * Creates a BranchCloser and adds it to the list of BranchClosers
   * for this node and to the list of trivial-closures if it is
   * trivial.
  public void createBranchCloser(Substitution s, Collection usedHyps,
                                 Collection usedGoals, Collection sequents) {
    BranchCloser bc = new BranchCloser(s, usedHyps, usedGoals, this, sequents);
    this.addBranchCloser(bc);
    if (s.isEmpty())
      this.addTrivialCloser(bc);
  }
   */

  /**
   * Creates a BranchCloser and adds it to the list of BranchClosers
   * for this node and to the list of trivial-closures if it is
   * trivial.
   */
  public void createBranchCloser(Substitution s, Collection usedHyps,
                                 Collection usedGoals, KBSequent sequent) {
    BranchCloser bc = new BranchCloser(s, usedHyps, usedGoals, this, sequent);
    this.addBranchCloser(bc);
    if (s.isEmpty())
      this.addTrivialCloser(bc);
  }

  public final TableauNode getFirstLeaf() {
    if (this.isLeaf())
      return this;
    else
      return this.getChildAt(0).getFirstLeaf();
  }

  /**
   * if this is a leaf, return the next leaf otherwise the first
   * leaf under this.
   * 
   * @return if this is a leaf, return the next leaf otherwise the first
   * leaf under this.  Null if there is not one.
   */
  public final TableauNode getNextLeaf() {
    TableauNode parent;
    TableauNode current = this;
    if (current.isLeaf()) {
      parent = current.getParent();
      while (true) {
        if (parent == null)
          return null;
        else {
          final int index = parent.children.indexOf(current);
          if (index < parent.children.size() - 1) {
            return parent.getChildAt(index + 1).getFirstLeaf();
          } else {
            current = parent;
            parent = parent.getParent();
          }
        }
      }
    } else {
      return current.getFirstLeaf();
    }
  }

  /**
   * @return the previous leaf or null if there is not one.
   */
  public final TableauNode getPrevLeaf() {
    TableauNode parent;
    if (this.isFirstSib()) {
      parent = this.parent;
      // index of parent within its parent.
      int index;
      while (true) {
        if (parent == null)
          return null;
        else {
          index = parent.getIndex();
          if (index == 0)
            parent = parent.getParent();
          else
            break;
        }
      }
      parent = parent.parent.getChildAt(index - 1);
      return parent.getLastLeaf();
    } else {
      return this.parent.getChildAt(this.getIndex() - 1).getLastLeaf();
    }
  }

  public TableauNode getLastLeaf() {
    if (this.isLeaf())
      return this;
    else
      return this.children.get(this.children.size() - 1).getLastLeaf();
  }

  public void undoneLeaves(Collection<TableauNode> c) {
//     if (this.isClosed())
//       return;
//     else
    if (this.children.isEmpty()) {
      c.add(this);
    } else {
      Iterator<TableauNode> it = this.children.iterator();
      while (it.hasNext()) {
        it.next().undoneLeaves(c);
      }
    }
  }

  protected void addBranchCloser(BranchCloser bc) {
    this.branchClosers.add(bc);
  }

  /**
   * Adds <code>nf</code> to the list of negative formulas born at
   * this node.  If <code>nf</code> is an equality predicate, then it
   * is added to the list of negative equality predicates instead.
   * @param nf the negative formula being added.
   */
  private void addNewGoal(TableauFormula nf) {
    Formula f = nf.getFormula();
    if (f instanceof Predicate && ((Predicate) f).getConstructor().toString().
        equals("="))
      this.newEQGoals.add(nf);
    else
      this.newGoals.add(nf);
  }

  /**
   * Adds <code>nf</code> to the list of positive formulas born at
   * this node.  If <code>nf</code> is an equality predicate, then it
   * is added to the list of positive equality predicates instead.  If
   * <code>nf</code> is an equality predicate with a skolem term at
   * the top level, then it is added to the Tableau's BrownEQ list.
   * @param nf the positive formula being added.
   */
  private void addNewHyp(TableauFormula nf) {
    Formula f = nf.getFormula();
    if (f instanceof Predicate) {
      Predicate p = (Predicate) f;
      if (p.getConstructor().toString().equals("=")) {
        this.newEQHyps.add(nf);
        Iterator<Term> terms = p.arguments();
        Term t1 = terms.next();

        Term t2 = null;
        t2 = terms.next();

        if (t1 instanceof Function && ((Function) t1).isSkolem() >= 0)
          this.getTableau().addBrownEQ(nf);

        // check for class member equalities
        if (t1 instanceof ComplexTerm) {
          ComplexTerm ct1 = (ComplexTerm) t1;
          if (ct1.getConstructor().getName().equals("the-class-of-all"))
            this.getTableau().addClassMemberEQ(p);
        }
        if (t2 instanceof ComplexTerm) {
          ComplexTerm ct2 = (ComplexTerm) t2;
          if (ct2.getConstructor().getName().equals("the-class-of-all"))
            this.getTableau().addClassMemberEQ(p);
        }

      } else
        this.newHyps.add(nf);
    } else
      this.newHyps.add(nf);
  }

  /*public void setParent(TableauNode parent) {
  this.parent = parent;
  }*/
  public List<TableauNode> getChildren() {
    return // this.children == null ? null :
        Collections.unmodifiableList(this.children);
  }

  /**
   * Sets c to be the children of the receiver and sets the receiver
   * to be the parent of each element of c. 
   * 
   * @param c a list of nodes to become the children of the receiver.
   */
  public void setChildren(List<TableauNode> c) {
    if (c != null) {
      this.children = new ArrayList<TableauNode>(c);
      for (TableauNode child : this.children) {
        child.parent = this;
      }
    } else {
      // this needs to be here because if I call this with null, that means
      // I want there to be no children.
      this.children = new ArrayList<TableauNode>(0);
    }
  }

  public Tableau getTableau() {
    return this.tableau;
  }

  /**
   * Searches for and returns a congruence closure (DSTGraph).
   * If no graph is available at this node, it goes up
   * the current branch until it reaches the root node,
   * and finally checks the theory if none has yet been found.
   *
   * @return The closest available congruence closure.
   */
  private DSTGraph getClosestCC() {

    TableauNode node = this;
    DSTGraph currentGraph = null;

    do { // search up the tree
      currentGraph = node.getCongruenceClosure();
      if (currentGraph != null) {
        assert !currentGraph.isEmpty();
        break;
      }
    } while ((node = (TableauNode) node.getParent()) != null);

    // if none has as of yet been found, check the theory
    if (currentGraph == null) {
      currentGraph = this.getTableau().getTheory().
          getCongruenceClosure();
      if (currentGraph.isEmpty())
        return null;
    }

    return currentGraph;
  }

  /**
   * Replaces the receiver's children with the children of its one savedChild.
   * This takes care of any split objects involved.
   * @param savedChild
   */
  public final void replaceMyChildren(TableauNode savedChild) {
    this.setChildren(savedChild.getChildren());
    // call fireSpliceEvent on the receiver with null as the new value because
    // anyone interested in that split has no business with it any more.
    this.fireSpliceEvent(new SpliceEvent(this, null));
    // call fireSpliceEvent on the exception with the receiver as the new value
    // because the receiver is now the root of that split.
    this.fireSpliceEvent(new SpliceEvent(savedChild, this));
  }

  /**
   * This method makes sure that no duplicate formulas appear
   * on the same branch.  To ensure regularity, it needs to be
   * called any time a new formula is added to the tableau.
   * @return true if the node was deleted
   */
  public final boolean enforceRegularity() {
    // FIXME my current implementation of regularity may not be correct.
    // Regularity reformulation of the alpha rule:
    // if a branch phi contains a formula alpha, then phi can be
    // expanded by appending to it every alpha_i which does not already
    // occur on it.
    // Regularity reformulation of the alpha rule:
    // if a branch phi contains a formula beta and neither beta_1 nor
    // beta_2 already occur in it, then phi can be split into new expanded
    // branches with beta_1 and beta_2.
    if (this == this.tableau.getRoot())
      return false; // the root has no ancestor to be a duplicate of

    // TODO must not prune a new branch due to regularity.
    // consider making the duplicate formula refer back to the formula of
    // which it is a duplicate (using the partition data structure?)
    // But the new one needs to know its ancestry.
    // Also, when one is used in a branch closer, a branch closer must
    // be created for each of the duplicates.
    // TODO see if regularity in breadth first tableau is addressed in the
    // literature.

    Collection nonDuplicateHyps = this.getNonDuplicates(this.newHyps);
    Collection nonDuplicateGoals = this.getNonDuplicates(this.newGoals);

    // check to see if node has been emptied, in which case it
    // needs to be deleted from the tableau
    if (nonDuplicateHyps.isEmpty() && nonDuplicateGoals.isEmpty()) {
      this.deleteMeFromTableau();
      return true;
    } else { // otherwise update formulas
      this.newHyps = nonDuplicateHyps;
      this.newGoals = nonDuplicateGoals;
      return false;
    }
  }

  /**
   * This method deletes the receiver from the tableau by
   * setting its parent's children to exclude itself.  This is called by
   * enforceRegularity.  If the receiver's parent is a split, this method
   * might make it no longer a split.  The associated split object will
   * in that case set its node to null.
   */
  private void deleteMeFromTableau() {
    // will become the children of this.parent.
    List<TableauNode> newChildren;
    if (this.parent.children.size() == 1) // the receiver was an only child
      newChildren = this.children;
    else { // the receiver was one of multiple children
      newChildren =
      new ArrayList<TableauNode>(this.parent.children.size() +
                                 this.children.size());
      newChildren.addAll(this.parent.children);
      newChildren.remove(this);
      newChildren.addAll(this.children);
    }
    this.parent.setChildren(newChildren);
    // splits from the removed node must move up.
    this.fireSpliceEvent(new SpliceEvent(this, this.parent));
    // if the parent now has fewer than two children, then its
    // splits must go away.
    if (this.parent.children.size() < 2) {
      this.parent.fireSpliceEvent(new SpliceEvent(this.parent, null));
    }
  }

  /**
   * This method returns a Collection of all the TableauFormulas
   * in the list it is given that don't contain duplicates of
   * Formulas higher up in the Tableau.
   * @param c The TableauFormulas to examine.
   * @return A Collection of non-duplicate TableauFormulas.
   */
  private Collection<TableauFormula> getNonDuplicates(
      Collection<TableauFormula> c) {
    Collection<TableauFormula> retVal = new ArrayList<TableauFormula>(); // TODO rewrite this
    Iterator it = c.iterator();
    while (it.hasNext()) {
      TableauFormula tf = (TableauFormula) it.next();
      if (this.branchAlreadyContainsTF(tf)); // this.tableau.fireRegularityEnforcedEvent(new RegularityEnforcedEvent());
      else
        retVal.add(tf);
    }
    return retVal;
  }

  /**
   * This method checks for a formula (the formula contained
   * in the TableauFormula it is given) all the way up the branch
   * the receiver is in (the assumption being that the
   * receiver contains the TableauFormula it is given - thus
   * the receiver isn't examined in looking for matches). It
   * also looks down the receiver's branch until it hits a split
   * (the assumption is that it is unecessarily costly to check
   * every branch below a split for the formula - this may not be
   * the case, but because it would complicate the algorithm,
   * and because having duplicate formulas in a tree isn't
   * damaging to the proof-finding mechanism, this way of doing
   * things seems acceptable).
   *
   * Note: This method assumes the TableauFormula it is given
   * has a parent (TableauFormulas with a null parent field
   * will result in a NullPointerException).
   *
   * @param tf The TableauFormula whose formula to match.
   * @return true if the receivers branch contains the specified
   * formula, false otherwise.
   */
  private boolean branchAlreadyContainsTF(TableauFormula tf) {

    // check up the branch
    TableauNode currentNode = this.parent;
    do {
      if (currentNode.contains(tf))
        return true;
    } while ((currentNode = currentNode.parent) != null);

    // check down the branch
    Iterator poIt = this.preorderIterator();
    poIt.next(); // skip the root
    while (poIt.hasNext()) {
      TableauNode tn = (TableauNode) poIt.next();
      if (tn.children.size() > 1)
        break;
      if (tn.contains(tf))
        return true;
    }

    // didn't find it
    return false;
  }

  /**
   * The receiver is said to contain the TableauFormula's
   * formula if the receiver has the same formula and if the
   * two formulas' substitutions are either the same or if
   * the receiver's formula's substitution is non-empty and
   * the TableauFormula's formula is empty (because this method
   * is meant for use in checking for regularity, and because
   * allowing a needy formula to be added to a branch where the
   * same formula exists without any substitution dependencies
   * makes no sense).
   *
   * @param tf The TableauFormula whose formula to search for.
   * @return true if the receiver contains the TableauFormula's
   * formula, false otherwise (see description above).
   */
  private boolean contains(TableauFormula tf) {
    Iterator it;
    if (tf.getSign())
      it = this.newHyps.iterator();
    else
      it = this.newGoals.iterator();
    while (it.hasNext()) {
      TableauFormula currentTF = (TableauFormula) it.next();
      if (tf.formula.equals(currentTF.formula)) {
        Substitution s1 = tf.getSubstitutionDependency();
        Substitution s2 = currentTF.getSubstitutionDependency();
        // Note: I'm assuming these are never null...
        // TODO: Maybe make this a little bit more
        // sophisticated...
        if (s1.equals(s2) || s2.isEmpty())
          return true;
      }
    }
    return false;
  }

  public String toLongString(StringBuffer sb) {
    sb.append(this.path().toString() + " ");
    if (!this.newGoals.isEmpty()) {
      if (!this.newHyps.isEmpty()) {
        if (this.newHyps.size() == 1)
          sb.append("Suppose:\n");
        else
          sb.append("Suppose all of the following:\n");
        bitnots.util.Collections.appendToBuffer(newHyps, sb, "\n");
        if (this.newGoals.size() == 1)
          sb.append("and show:\n");
        else
          sb.append("and show one of the following:\n");
        bitnots.util.Collections.appendToBuffer(newGoals, sb, "\n");
      } else {
        if (this.newGoals.size() == 1)
          sb.append("Show:\n");
        else
          sb.append("Show one of the following:\n");
        bitnots.util.Collections.appendToBuffer(newGoals, sb, "\n");
      }
    } else if (!this.newHyps.isEmpty()) {
      if (this.newHyps.size() == 1)
        sb.append("Show that the following is false:\n");
      else
        sb.append("Show that the following are contradictory:\n");
      bitnots.util.Collections.appendToBuffer(newHyps, sb, "\n");
    }
    sb.append("bcs: ");
    Iterator bcsIt = this.branchClosers.iterator();
    while (bcsIt.hasNext()) {
      sb.append(bcsIt.next() + "\n");
    }
    Iterator it = this.getChildren().iterator();
    while (it.hasNext()) {
      ((TableauNode) it.next()).toLongString(sb);
    }
    return sb.toString();
  }

  /**
   * @param flag true if the user want to see the formulas
   * that haven't been used anywhere, false if the user only
   * wants to see the receiver's new goals and hyps.
   */
  public String toHTMLString(boolean flag) {
    StringBuffer sb = new StringBuffer();
    ArrayList goals;
    ArrayList hyps;
    if (flag)
      goals = new ArrayList(this.getGoalsUnusedAnywhere());
    else
      goals = new ArrayList(this.newGoals);
    goals.addAll(this.newEQGoals);
    if (flag)
      hyps = new ArrayList(this.getHypsUnusedAnywhere());
    else
      hyps = new ArrayList(this.newHyps);
    hyps.addAll(this.newEQHyps);

    sb.append("<html>\n");

    if (!goals.isEmpty()) {
      if (!hyps.isEmpty()) {
        if (hyps.size() == 1)
          sb.append("<b>Suppose:</b><br>\n");
        else
          sb.append("<b>Suppose all of the following:</b><br>");
        sb.append("\n<ul>\n");
        bitnots.util.Collections.appendToBuffer(hyps, sb, "<li>", "</li>");
        sb.append("\n</ul>\n<br>");
        if (goals.size() == 1)
          sb.append("<b>and show:</b><br>");
        else
          sb.append("<b>and show one of the following:</b><br>");
        sb.append("\n<ul>\n");
        bitnots.util.Collections.appendToBuffer(goals, sb, "<li>", "</li>");
        sb.append("\n</ul>\n<br>");
      } else {
        if (goals.size() == 1)
          sb.append("<b>Show:</b><br>");
        else
          sb.append("<b>Show one of the following:</b><br>");
        sb.append("\n<ul>\n");
        bitnots.util.Collections.appendToBuffer(goals, sb, "<li>", "</li>");
        sb.append("\n</ul>\n<br>");
      }
    } else if (!hyps.isEmpty()) {
      if (hyps.size() == 1)
        sb.append("<b>Show that the following is false:</b><br>");
      else
        sb.append("<b>Show that the following are contradictory:</b><br>");
      sb.append("\n<ul>\n");
      bitnots.util.Collections.appendToBuffer(hyps, sb, "<li>", "</li>");
      sb.append("\n</ul>\n<br>");
    }
    sb.append("</html>");

    return sb.toString();
  }

  @Override
  public String toString() {
    StringBuffer sb = new StringBuffer(this.path().toString() + " ");
    ArrayList goals = new ArrayList(this.newGoals);
    goals.addAll(this.newEQGoals);
    ArrayList hyps = new ArrayList(this.newHyps);
    hyps.addAll(this.newEQHyps);
    if (!goals.isEmpty()) {
      if (!hyps.isEmpty()) {
        if (hyps.size() == 1)
          sb.append("Suppose: ");
        else
          sb.append("Suppose all of the following: ");
        bitnots.util.Collections.appendToBuffer(hyps, sb, " ");
        if (goals.size() == 1)
          sb.append("and show: ");
        else
          sb.append("and show one of the following: ");
        bitnots.util.Collections.appendToBuffer(goals, sb, " ");
      } else {
        if (goals.size() == 1)
          sb.append("Show: ");
        else
          sb.append("Show one of the following: ");
        bitnots.util.Collections.appendToBuffer(goals, sb, " ");
      }
    } else if (!hyps.isEmpty()) {
      if (hyps.size() == 1)
        sb.append("Show that the following is false: ");
      else
        sb.append("Show that the following are contradictory: ");
      bitnots.util.Collections.appendToBuffer(hyps, sb, " ");
    }
    return sb.toString();
  }

  /**
   * Returns true if any of the formulas in formulas are new in the receiver.
   * Deletes all formulas new here that are not in formulas.
   * @param formulas
   * @return true if any of the formulas in formulas are new in the receiver.
   */
  public boolean deleteAllBut(HashSet<TableauFormula> formulas) {
    Iterator<TableauFormula> it;
    boolean value = false;
    it = this.newEQGoals.iterator();
    while (it.hasNext()) {
      if (formulas.contains(it.next()))
        value = true;
      else
        it.remove();
    }
    it = this.newEQHyps.iterator();
    while (it.hasNext()) {
      if (formulas.contains(it.next()))
        value = true;
      else
        it.remove();
    }
    it = this.newGoals.iterator();
    while (it.hasNext()) {
      if (formulas.contains(it.next()))
        value = true;
      else
        it.remove();
    }
    it = this.newHyps.iterator();
    while (it.hasNext()) {
      if (formulas.contains(it.next()))
        value = true;
      else
        it.remove();
    }
    return value;
  }

  /**
   * Get rid of BranchClosers (and splits, theorem applications?) that were
   * not involved.  Assumes that mbc closes a subtree containing the receiver.
   * 
   * @param node
   * @param mbc
   * @todo handle splits and theoremApps here too.
   */
  public void cleanUp(final MultiBranchCloser mbc) {
    // FIXME this doesn't seem to be working or maybe is not being called when it should
    bitnots.util.Collections.deleteIf(this.branchClosers, new PredicateBlock() {
      @Override
      public boolean test(Object o) {
        return !mbc.getBranchClosers().contains((BranchCloser) o);
      }
    });
  }

  /** Iterates through the subtree rooted at the receiver in pre-order.
   * @see java.lang.Iterable#iterator()
   */
  public Iterator<TableauNode> iterator() {
    return this.preorderIterator();
  }

  /** Iterates through the subtree rooted at the receiver in pre-order.
   * @return an iterator that iterates through the subtree rooted at the
   * receiver in pre-order.
   */
  public Iterator<TableauNode> preorderIterator() {
    return new PreorderIterator();
  }

  /** Iterates through the subtree rooted at the receiver in pre-order.
   */
  public class PreorderIterator implements Iterator<TableauNode> {

    /**
     * The top of this stack is what will be returned by the next call
     * to next().  */
    private ExposedLinkedList<TableauNode> stack =
                                           new ExposedLinkedList<TableauNode>();

    /**
     * Returns true if there is another element in the container, and
     * false otherwise.
     * @return true iff there is another element in the container.  */
    public boolean hasNext() {
      return !this.stack.isEmpty();
    }

    /**
     * Returns the next element in the container, in the pre-order.
     * @return the next element in the container.
     * @exception NoSuchElementException if there are no elements left
     * in the tree that haven't already been visited by this Iterator.
     * */
    public TableauNode next() {
      if (!this.hasNext())
        throw new NoSuchElementException();
      TableauNode retVal = (TableauNode) this.stack.pop();
      for (int i = retVal.children.size() - 1; i >= 0; i--)
        this.stack.push(retVal.getChildAt(i));
      return retVal;
    }

    /**
     * The <code>remove</code> method is not supported.
     * @exception UnsupportedOperationException cuz it's not
     * supported. */
    public void remove() {
      throw new UnsupportedOperationException();
    }

    /**
     * Creates a new <code>PreorderIterator</code> instance.
     */
    public PreorderIterator() {
      this.stack.push(TableauNode.this);
    }

    private PreorderIterator(ExposedLinkedList stack) {
      this.stack = stack;
    }
  }

  // TODO I still have some work to do protecting the invariants of this
  // class.  Implement appropriate set and add methods with appropriate
  // protections.
  public TableauNode(Tableau tableau, Collection<TableauFormula> newHyps,
                     Collection<TableauFormula> newGoals) {
    this.tableau = tableau;
    this.children = new ArrayList<TableauNode>(0);
    this.newGoals = new ArrayList<TableauFormula>(newGoals.size());
    this.newHyps = new ArrayList<TableauFormula>(newHyps.size());
    for (TableauFormula form : newGoals) {
      this.addNewGoal(form);
      form.setBirthPlace(this);
      form.registerWithTableau(tableau);
    }
    for (TableauFormula form : newHyps) {
      this.addNewHyp(form);
      form.setBirthPlace(this);
      form.registerWithTableau(tableau);
    }
  }

  public TableauNode(Tableau tableau, Collection<TableauFormula> formulas) {
    this.tableau = tableau;
    this.children = new ArrayList<TableauNode>(0);
    this.newGoals = new ArrayList<TableauFormula>(formulas.size());
    this.newHyps = new ArrayList<TableauFormula>(formulas.size());
    // iterate through the formula and call insertInto
    for (TableauFormula form : formulas) {
      if (form.getSign()) {
        this.addNewHyp(form);
      } else {
        this.addNewGoal(form);
      }
      form.setBirthPlace(this);
      form.registerWithTableau(tableau);
    }
  }
}
