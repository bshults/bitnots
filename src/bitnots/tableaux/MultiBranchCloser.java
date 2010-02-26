package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 * This is collection of branchClosers that combine to close parts or
 * all of a tableau.  These should be essentially immutable.
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public final class MultiBranchCloser {

  /**
   * List of branchClosers that are merged here.  These need to be stored in
   * pre-order.
   */
  private List<BranchCloser> branchClosers;

  /**
   * The compatible composition of the branchClosers' substitutions.
   */
  private Substitution subst;

  /**
   * These are all of the unclosed children of all of the involved
   * splits.  If you cannot close the first one, then this closer
   * cannot be extended.  These should remain sorted so that the first
   * element is the next to be tried.
   */
  private Stack<NeedToClose> needToCloses;

  /**
   * the top of the stack of NeedToCloses of the parent
   * MultiBranchCloser. */
  // TODO get rid of this field
  private NeedToClose currentNeed;
  
  private Tableau tableau;

  /**
   * No unsatisfied Requirement can have zero possibilities.  
   */
  private Collection<Requirement> requires;

  /**
   */
  private ArrayList<TableauNode.Split> involvedSplits;

  /**
   * This returns true if this MultiBranchCloser closes the entire
   * tableau.
   */
  public boolean isProof() {
    return this.currentNeed == null; // this.needToCloses.isEmpty();
  }

  /**
   * Returns an unmodifiable List of the receiver's involvedSplits.
   * @return an unmodifiable List of the receiver's involvedSplits.
   */
  public List<TableauNode.Split> getInvolvedSplits() {
    return java.util.Collections.unmodifiableList(this.involvedSplits);
  }

  /**
   * Returns the branch closers.
   * @return the branch closers.
   */
  public List<BranchCloser> getBranchClosers() {
    return this.branchClosers;
  }

  /**
   * This prunes branches and nodes that cannot contribute to a proof that
   * uses the receiver.
   * WARNING: this destructively modifies the tableau!
   */
  public void condense() {
    if (this.needToCloses.isEmpty()) {
      // If needToCloses is empty, then condense the entire tree.
      this.condense(this.tableau);
    } else {
      // Otherwise, let firstNeed to the first (in pre-order) element of this.needToCloses.
      NeedToClose firstNeed = this.needToCloses.peek();
      if (firstNeed.node != this.tableau.getRoot()) {
        // If it is not the root, firstNeed must have previous siblings.
        // Let branch be the branch from the root to firstNeed's parent.
        // For each child, toClose, of branch that is to the left of firstNeed
        //   condense the subtree rooted at toClose.
        TableauNode branch = firstNeed.node.getParent();
        for (TableauNode toClose: branch.getChildren())
          if (toClose == firstNeed.node)
            break;
          else
            this.condense(toClose);
        // From here down, the value of branch will change.  It will move
        // up to become the child of split that was on the path from
        // the root to the original value of branch.
        // Move up branch from there: for each split, split, on branch
        TableauNode split = branch.getParent();
        while (split != null) {
          if (split.getChildCount() > 1)
            if (this.involvesSplit(split)) {
              //   if it is involved, condense branches to the left of branch.
              for (TableauNode toClose: split.getChildren())
                if (toClose == branch) 
                  break;
                else
                  this.condense(toClose);
            } else if (split.getChildAt(0) != branch)
              //   if it is not involved,
              //     if split has children to the left of branch
              //       prune all children of split other than branch
              split.setChildren(branch.getChildren()); // TODO worry about splits here?
            //       else leave it alone since it might become involved later.
          branch = split;
          split = branch.getParent();
        }
      } 
      // If firstNeed is the root of the tableau, the do nothing because
      // there are no bcs.
    }
    
    // TODO Remove all formulas that require a substitution that is incompatible
    // with this.subst.  For each Theorem application, if it used a substitution
    // that is not compatible with this.subst, then prune all of its branches
    // other than B.

    // TODO Apply this.subst to the entire tree and add it to the required
    // substitution of every formula.  (Or maybe add the substitution at the
    // tableau level.)
    
  }
  
  /**
   * Condense the entire tableau.
   * PRECONDITION: the receiver has no needs.
   * 
   * @param tableau
   */
  private void condense(Tableau tableau) {
    this.condense(tableau.getRoot());
  }

  /**
   * PRECONDITION: root is completely closed by the receiver.
   * PRECONDITION: The receiver is involved in the receiver.
   */
  private void condense(TableauNode root) {
    // Initialize a collection, formulas, to store involved formulas.
    HashSet<TableauFormula> formulas = new HashSet<TableauFormula>();
    // For each branchClosers, bc
    bc: for (BranchCloser bc: this.branchClosers) {
      // if bc is located on a descendant, branch, of root
      TableauNode branch = bc.getHome();
      // this is needed because root may not be the root of the tableau but
      // only the root of a subtree that needs to be condensed.
      if (branch.isDescentantOf(root)) {
        branch.cleanUp(this);
        //   delete the children of branch bc
        branch.setChildren(null);
        //   add the formulas and ancestors of formulas involved in bc to formulas.
        for (TableauFormula goal: bc.getGoals())
          goal.addAncestors(formulas);
        for (TableauFormula hyp: bc.getHyps())
          hyp.addAncestors(formulas);
        branch.deleteAllBut(formulas);
        // move up branch toward the root looking at parent and branch along the way.
        TableauNode parent = branch.getParent();
        while (parent != null) {
          if (parent.getChildCount() > 1 &&
                  //     if parent is a split
                  // and involved
                  this.involvesSplit(parent) &&
                  // and branch is not the last child of parent
                  branch.getIndex() != parent.getChildCount() - 1) {
            // then let the other bcs catch up.
            continue bc;
          } else if (!parent.deleteAllBut(formulas)) {
            //     if parent does not contain an involved formula
            //       set parent's parent's children to be parent's children
            // this is OK because if parent is not involved, the neither will
            // its siblings be
            parent.getParent().replaceMyChildren(parent);
            parent = parent.getParent();
          } else {
            branch = parent;
            parent = parent.getParent();
            branch.cleanUp(this);
            // since root may not be the root of the tableau but only the
            // root of a subtree which needs to be condensed:
            if (branch == root)
//              break bc; // this doesn't seem right.
              break;
          }
        }
      }
    }
  }

  /**
   * Returns true if split is involved in this.
   * In particular, if node is an involved split.
   * @param node
   * @return
   */
  private boolean involvesSplit(TableauNode node) {
    for (TableauNode.Split split: this.involvedSplits) {
      if (split.getNode() == node)
        return true;
    }
    return false;
  }

  /**
   * Tries to close the tree and returns the successful
   * MultiBranchCloser if successful.  This assumes that
   * backtrackingPassOne has already been called and, thus, all
   * branchClosers are in place.  For <b>each</b> branch closer in the
   * tableau, if it prunes all branches to its left, then this makes a
   * new MultiBranchCloser for that BranchCloser.  If that
   * MultiBranchCloser finishes everything, then it is returned and
   * the tableau is notified that it is done.  If there are branches
   * to the right that are not closed or pruned by the
   * MultiBranchCloser, then this sends unificationComplete to the
   * MultiBranchCloser to see if it can be extended to finish the
   * entire tree.
   * @return a MultiBranchCloser that closes the tree based at root or
   * null if none is found.  */
  public static MultiBranchCloser backtrackingPassTwo(TableauNode root) {
    MultiBranchCloser tCloser = new MultiBranchCloser(root.getTableau());
    return tCloser.unificationComplete();
  }

  public MultiBranchCloser unificationComplete() {
    // The receiver absolutely does not close this tableau and never
    // will, otherwise, unificationComplete would not have been
    // called.  We want to know if one of its extensions will finish
    // the entire tableau.  If all BranchClosers through the first
    // element of needToCloses are examined and none of them can be
    // used to extend unification in such a way as to finish the
    // tableau, then fail.
    Stack<MultiBranchCloser> toBeExtended = new Stack<MultiBranchCloser>();
    toBeExtended.push(this);
    do {
      MultiBranchCloser current = toBeExtended.pop();
      // Extend current in every possible way but by one new BC.
      // That new BC must be below the first needToClose.
      for (TableauNode node: current.currentNeed.node) {
        for (BranchCloser bc: node.getBranchClosers()) {
          // for each branch closer in this node, try to extend current
          MultiBranchCloser finished = current.createCompatibleExtension(bc);
          if (finished != null) {
            if (finished.isProof())
              return finished;
            else {
              // TODO have a version of this that adds finished to some
              // collection to be returned in case the proof is not finished.
              // Push its extensions onto the stack to be extended later.
              toBeExtended.push(finished);
            }
          }
        }
      }
    } while (!toBeExtended.isEmpty());
    // no finishing extension was found.
    return null;
  }
  
  // TODO Write a method that returns a collection of all MBCs that have
  // nonzero possibilities.
  
  /**
   * If the substitutions are compatible and bc works with the
   * receiver in any other necessary way, then this combines the
   * receiver with the argument into the return value. <br>
   *
   * PRECONDITIONS: bc cannot be pruned by everything in the
   * receiver. <br>
   *
   * POSTCONDITIONS: <br>
   *
   * There are three conditions that must be met before a branch
   * closer, bc, is allowed to be added to an existing
   * MultiBranchCloser, mbc.  (We are always working under the
   * assumption that bc is after the other branch closers in mbc in
   * preorder.)  If the return value is non-null, then: <br>
   *
   * C.1. The two substitutions must be compatible. <br>
   *
   * C.2. No Requirement with zero possibilities may be created.  One
   * consequence of this is that if <b>currentNeed.parent</b> is not
   * in <b>bc.involved</b> then <b>newNeeds</b> must not be empty.
   * <br>
   *
   * C.3. <b>bc</b> must be a descendant of the left-most child of
   * every element of <b>newNeeds</b>. </br> */
  @SuppressWarnings("nls")
  private MultiBranchCloser createCompatibleExtension(BranchCloser bc) {
    Substitution subs = Substitution.compatible(this.subst, bc.getSubst());
    if (subs == null)
      return null;
    else {
      // NeedToCloses to be added by this bc if it is compatible.
      ArrayList<NeedToClose> newNeeds = new ArrayList<NeedToClose>();
      // Splits to be added by this bc if it is compatible.
      ArrayList<TableauNode.Split> newSplits = new ArrayList<TableauNode.Split>();
      // Add bc's new splits
      for (TableauNode.Split split: bc.getInvolvedSplits()) {
        // if the split is new to the receiver
        if (!this.involvedSplits.contains(split)) {
          // if the split is still a split
          if (split.getNode() != null) {
            newSplits.add(split);
            Iterator<TableauNode> childrenIt = split.getNode().getChildren().iterator();
            TableauNode child = childrenIt.next();
            if (!child.isAncestorOf(bc.getHome()))
              return null;
            do {
              child = childrenIt.next();
              newNeeds.add(new NeedToClose(child));
            } while (childrenIt.hasNext());
          }
        }
      }
      // Now newNeeds contains the new NeedToCloses that will be added
      // by bc if it is compatible and newSplits contains the new splits
      // introduced by bc.
      // Create the list of Requirements to be included in the new
      // MultiBranchCloser.
      ArrayList<Requirement> newReqs = new ArrayList<Requirement>();
      // If currentNeed has a split and that split is in bc.involved,
      // then we want to do something with it.

      // TODO if currentNeed is the root, then can we even be here?
      // if currentNeed is the root then we don't need to add a requirement.
      if (this.currentNeed.node.getParent() != null && 
              !bc.involves(this.currentNeed.node)) {
        // bc does not involve the current need, so if bc does not add a new
        // NeedToClose, then it is incompatible.
        if (newNeeds.isEmpty())
          return null;
        else
          // otherwise, add a Requirement.
          newReqs.add(new Requirement(this.currentNeed.node, newNeeds.size()));
      }
      for (Requirement req: this.requires) {
        if (!bc.involves(req.node)) {
          Requirement newReq = new Requirement(req);
          // decrement it once for the loss of ntc.
          newReq.decrement();
          // increment it for each new NeedToClose.
          newReq.increment(newNeeds.size());
          if (newReq.possibilities == 0)
            return null;
          else
            newReqs.add(newReq);
        }
      }
      // If we get this far, then we know they are compatible.
      return this.createExtension(bc, subs, newNeeds, newReqs, newSplits);
    }
  }
  
  /**
   * Creates and returns a new MultiBranchCloser that extends the
   * receiver by adding <code>bc</code>.  This assumes that their
   * substitutions are compatible and that <code>composition</code> is
   * the composition of the two substitutions. <br>
   *
   * POSTCONDITIONS: <br>
   *
   * E.1. Naturally, $\mathrm{\bf bc}$ will be added to $\mathrm{\bf
   * mbc2.bcs}$.
   *
   * E.2. The $\mathrm{\bf mbc2.subs}$ will be the composition of
   * $\mathrm{\bf mbc.subs}$ and $\mathrm{\bf bc.subs}$.
   *
   * E.3. The node $\mathrm{\bf ntc}$ will not be in $\mathrm{\bf
   * mbc2.needs}$.
   *
   * E.4. For each element, $\mathrm{\bf i}$, of $\mathrm{\bf
   * bc.involved}$ that is not in $\mathrm{\bf mbc.involved}$,
   * $\mathrm{\bf i}$ is added to $\mathrm{\bf mbc2.involved}$ and all
   * children except the leftmost child of $\mathrm{\bf i}$ are added
   * to $\mathrm{\bf mbc2.needs}$.  (It will be shown in
   * Section~\ref{sec:proofs} that this can easily be done in a way
   * the preserves the order invariant on the stack.)  We will call
   * the set of nodes added to $\mathrm{\bf mbc2.needs}$ by this
   * extension, $\mathrm{\bf newNeeds}$.
   *
   * E.5. For each Requirement, $\mathrm{\bf r}$, of $\mathrm{\bf
   * mbc}$, if $\mathrm{\bf r.node.parent}$ is in $\mathrm{\bf
   * bc.involved}$, then $\mathrm{\bf r}$ will not be in $\mathrm{\bf
   * mbc2}$.  Otherwise, $\mathrm{\bf mbc2}$ will contain a {\em
   * copy\/} of $\mathrm{\bf r}$.  The value of the possibilities
   * field of the copy will be
   *
   * \[\mathrm{\bf r.possibilities} - 1 + \mathrm{\bf
   * newNeeds.size}\enspace.\]
   *
   * If $\mathrm{\bf ntc.parent}$ is not in $\mathrm{\bf bc.involved}$
   * then a new Requirement, whose node will be $\mathrm{\bf ntc}$ and
   * whose possibilities will be $\mathrm{\bf newNeeds.size}$, must
   * also be added to $\mathrm{\bf mbc.requires}$.
   *
   * @param composition the composition of the substitutions
   * associated with the receiver and <code>bc</code>.  This must have
   * been computed previously.
   * @param bc the BranchCloser to be added to the reciever to create
   * the new MultiBranchCloser.
   * @param newNeeds the new NeedToCloses introduced by bc <i>in
   * order</i>.
   * @param valueCPs the list of Requirements for the value.
   * @param newSplits the list of Splits introduced by bc.  */
  private MultiBranchCloser createExtension(BranchCloser bc,
                                            Substitution composition,
                                            List<NeedToClose> newNeeds,
                                            Collection<Requirement> valueCPs,
                                            Collection<TableauNode.Split> newSplits) {
    // Make new branchClosers list.  (4)
    ArrayList<BranchCloser> valueBCs = new ArrayList(this.branchClosers.size() + 1);
    valueBCs.addAll(this.branchClosers);
    valueBCs.add(bc);

    // Make new splitsRemaining list and needToCloses.
    Stack<NeedToClose> valueNTC = (Stack) this.needToCloses.clone();

    for (int i = newNeeds.size() - 1; i >= 0; --i)
      valueNTC.push(newNeeds.get(i));

    // the next currentNeed.
    NeedToClose next = null;
    if (!valueNTC.isEmpty())
      next = valueNTC.pop();

    ArrayList<TableauNode.Split> vInvolved;
    if (newSplits.isEmpty())
      vInvolved = this.involvedSplits;
    else {
      vInvolved = (ArrayList) this.involvedSplits.clone();
      vInvolved.addAll(newSplits);
    }

    return new MultiBranchCloser(this.tableau, valueBCs, composition, valueNTC, next,
                                 valueCPs, vInvolved);
  }

  /**
   * Encapulates a node which MUST eventually be involved with the MBC
   * containing it due to the fact that its first sibling is involved.
   * This node must at one point have been a NTC of the containing MBC or
   * its predecessor.
   * The possibilities is the number of NTCs under that node.  If there are
   * no NTCs under the node of an MBC's requirement, then the MBC cannot be
   * extended to finish the proof.
   * 
   * @author bshults
   *
   */
  private static class Requirement {

    TableauNode node;
    int possibilities = 1;

    void decrement() {
      --this.possibilities;
    }

    void increment(int amount) {
      this.possibilities += amount;
    }

    public Requirement(TableauNode node, int poss) {
      if (poss < 1)
        throw new IllegalArgumentException();
      this.node = node;
      this.possibilities = poss;
    }

    public Requirement(Requirement req) {
      this.node = req.node;
      this.possibilities = req.possibilities;
    }
  }

  private static class NeedToClose implements Comparable {
    TableauNode node = null;

    public int compareTo(Object o) {
      return this.node.compareTo(((NeedToClose) o).node);
    }

    /**
     * Return the split of this node's parent.
     * @return the split of this node's parent.
    private Split getSplit() {
      if (this.node.getParent() == null)
        return null;
      else
        return this.node.getParent().split;
    }
     */

    NeedToClose(TableauNode node) {
      this.node = node;
    }
  }

  public String toString() {
    StringBuffer sb = new StringBuffer();
    sb.append("bcs: " + this.branchClosers.size() + '\n');
    sb.append("subst: " + this.subst + '\n');
    sb.append("need: \n");
    if (!this.isProof()) {
      sb.append(this.currentNeed.node.path() + "\n");
      Iterator needs = this.needToCloses.iterator();
      while (needs.hasNext()) {
        TableauNode n = ((NeedToClose) needs.next()).node;
        sb.append(n.path() + "\n");
      }
    }
    //    sb.append("remaining: " + this.splitsRemaining)
    return sb.toString();
  }

  /**
   * @param bcs the branchClosers
   * @param sub the Substitution
   * @param needs the Stack of NeedToCloses
   * @param need the top of the stack of NeedToCloses of the parent
   * MultiBranchCloser.
   * @param cps Collection of Requirements.
   * @param involvedSplits the involvedSplits.
   */
  private MultiBranchCloser(Tableau tableau, ArrayList<BranchCloser> bcs, Substitution sub,
                            Stack<NeedToClose> needs, NeedToClose need, Collection<Requirement> cps,
                            ArrayList<TableauNode.Split> involvedSplits) {
    this.branchClosers = bcs;
    this.subst = sub;
    this.tableau = tableau;
    this.needToCloses = needs;
    this.currentNeed = need;
    this.requires = cps;
    this.involvedSplits = involvedSplits;
  }

  public MultiBranchCloser(TableauNode root) {
    this(root.getTableau(), new ArrayList<BranchCloser>(), Substitution.createSubstitution(),
         new Stack<NeedToClose>(), new NeedToClose(root),
         new ArrayList<Requirement>(0), new ArrayList<TableauNode.Split>(0));
  }

  public MultiBranchCloser(Tableau tableau) {
    this(tableau, new ArrayList<BranchCloser>(), Substitution.createSubstitution(),
         new Stack<NeedToClose>(), new NeedToClose(tableau.getRoot()),
         new ArrayList<Requirement>(0), new ArrayList<TableauNode.Split>(0));
  }

}// MultiBranchCloser

