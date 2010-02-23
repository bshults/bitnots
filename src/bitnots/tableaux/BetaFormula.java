package bitnots.tableaux;

import java.util.*;

import bitnots.tableaux.TableauNode.Split;
import bitnots.util.*;
import bitnots.expressions.*;

/**
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public abstract class BetaFormula extends TableauFormula {

  public final void expand() {
    this.attachChildNodes();
    this.getBirthPlace().getTableau().removeBeta(this);
  }

  /** This adds the receiver to the list of alphas, betas, etc., of
   * the tableau of pn.
   */
  protected final void registerWithTableau(Tableau pn) {
    pn.addBeta(this);
  }

  public void unregisterWithTableau(Tableau p) {
    p.removeBeta(this);
  }

  // TODO: check this.
  /**
   * For each leaf of this formula's birthplace, two things happen: a new split
   * is added to the leaf AND the leaf is given multiple children each of which
   * involve that new split.
   */
  protected final void attachChildNodes() {

    this.getBirthPlace().applyToLeaves(new Operator() {
        public void exec(Object o) {
          // for each leaf
          TableauNode leaf = (TableauNode) o;

          TableauNode.Split split = new TableauNode.Split(leaf);

          // Take care of splits.
          TreeSet<TableauNode.Split> splits = new TreeSet<TableauNode.Split>();
          splits.addAll(BetaFormula.this.involvedSplits);
          splits.add(split);

          Collection<Collection<TableauFormula>> children =
            BetaFormula.this.createChildren(splits);

          // TODO check this code and put it back in
          /*
          if (leaf.getTableau().makeLemmas())
            children = BetaFormula.this.makeAndInsertLemmas(children, splits);
*/
          // make a list of child nodes
          List<TableauNode> newChildren = BetaFormula.this.createChildNodes(children);

          // TODO get rid of this debugging code.
          // if (newChildren.size() < 2) {
            // throw new Error("This is supposed to be a splitting rule!");
          // }
          
          // set this list to be the leaf's children
          leaf.setChildren(newChildren);

          for (TableauNode newNode: newChildren) {
            // set each new node's parent to be the leaf.
            // set each new node to be used of the receiver.
            // newNode.setParent(leaf);
            BetaFormula.this.addUsedAt(newNode);
            // TODO enforcing regularity on splitting rules is more complicated
            // than my code currently handles.  For the sake of soundness,
            // I need to leave some duplicates until I find a way to fix this.
//            newNode.enforceRegularity();
          }
        }
      });
  }

  /**
   * This does not connect the new nodes to any other nodes.  However,
   * it does insert the child formulas into the new nodes.
   * @return a list containing TableauNodes suitable for use in
   * splitting any branch that contains the receiver.
   */
  protected final List<TableauNode> createChildNodes(Collection<Collection<TableauFormula>> children) {
    ArrayList<TableauNode> value = new ArrayList<TableauNode>(children.size());
    for (Collection<TableauFormula> col: children) {
      value.add(new TableauNode(this.birthPlace.getTableau(), col));
    }
    return value;
  }

  /**
   * Create children formulas with the given list of splits.  The
   * resulting list of splits can be shared between the children.  The
   * collection should contain at least all splits contained in the
   * receiver.
   */
  protected abstract Collection<Collection<TableauFormula>> createChildren(Set<Split> splits);

  public BetaFormula(Formula f, FormulaHelper help) {
    super(f, help);
  }

  /**
  * Returns the Collection of children with equality lemmas added.
  * @param children the collection of children without lemmas.
  * @param splits the set of involved splits.
  * @return the same collection as children, with lemmas added.
  */
  private Collection<Collection<TableauFormula>> makeAndInsertLemmas(Collection<Collection<TableauFormula>> children, 
                                                                     TreeSet<TableauNode.Split> splits) {
    // Until the lemma is found, leaves get entered into here.  When
    // the lemma is found, it is added to all of the leaves here and
    // then those leaves are added to newChildren.
    ArrayList<Collection<TableauFormula>> newChildrenWithoutLemmas =
      new ArrayList<Collection<TableauFormula>>();
    
    // The new collection of children with the lemma added in on every
    // leaf other than the one that the lemma came from.
    ArrayList<Collection<TableauFormula>> newChildren =
      new ArrayList<Collection<TableauFormula>>(children.size());
    
    // The collection that contains the lemma.
    Collection<TableauFormula> theLemma = null;
    
    Iterator<Collection<TableauFormula>> leafIter = children.iterator();
    while (leafIter.hasNext()) {
      Collection<TableauFormula> leaf = leafIter.next();
      
      if (theLemma == null) {
        // We haven't found a lemma yet, so we're looking for
        // equality formulas.  Until one is found, add all leaves
        // to newChildrenWithoutLemmas.
        Iterator<TableauFormula> childIter = leaf.iterator();
        while (childIter.hasNext()) {
          Object obj = childIter.next();
          if ((obj instanceof PredicateFormula) &&
                  ((PredicateFormula) obj).getFormula().getConstructor()
                  .getName().equals("=")) {
            // Just found an equality formula to make a lemma from.  Now
            // make the lemma and add it to any leaves we've seen
            // previously.
            theLemma =
              BetaFormula.this.createChildFormulaCollection(((PredicateFormula) obj).getFormula(),
                                                            splits, !((PredicateFormula) obj).getSign());
            
            Iterator<Collection<TableauFormula>> newChildIter = newChildrenWithoutLemmas.iterator();
            while (newChildIter.hasNext()) {
              Collection<TableauFormula> leafObj = newChildIter.next();
              ArrayList<TableauFormula> newLeaf =
                new ArrayList<TableauFormula>(leafObj.size() + 1);
              newLeaf.addAll(leafObj);
              newLeaf.addAll(theLemma);
              newChildren.add(newLeaf);
            }
            newChildren.add(leaf);
          }
        }
        newChildrenWithoutLemmas.add(leaf);
      } else {
        // Since we've already found a lemma, we can simply add it
        // and add the resulting leaf to newChildren.
        ArrayList<TableauFormula> newLeaf = new ArrayList<TableauFormula>(leaf.size() + 1);
        newLeaf.addAll(leaf);
        newLeaf.addAll(theLemma);
        newChildren.add(newLeaf);
      }
    }
    if (newChildren.size() == 0)
      return newChildrenWithoutLemmas;
    return newChildren;
  }
  
}// BetaFormula

