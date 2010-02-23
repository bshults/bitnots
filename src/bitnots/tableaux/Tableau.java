package bitnots.tableaux;

import bitnots.expressions.*;
import bitnots.util.*;
import bitnots.theories.*;
import java.beans.*;
import java.io.*;
import java.util.*;
import java.awt.event.*;

/**
 * 
 * @author bshults
 *
 */
public class Tableau implements Serializable, Iterable<TableauNode> {

  /**
  * If this is true, lemmas will be made when proving.  Set to false
  * by default.
  */
  private boolean makeLemmas = false;

  private EpsilonToolkit epsilonToolkit;

  /**
  * Returns whether or not lemmas are made when proving.
  * @return true if lemmas are made when proving.
  */
  public boolean makeLemmas()
  {
      return this.makeLemmas;
  }

  /**
  * Sets whether or not lemmas are made when proving.
  * @param newValue the new state of whether to make lemmas or not.
  */
  public void setMakeLemmas(boolean newValue)
  {
      this.makeLemmas = newValue;
  }

  /**
   * The root of the tableau.
   */
  TableauNode root;

  // This method should be gotten rid of as soon as the GUI
  // has a way of visualizing both the condensed and the
  // non-condensed tree.
  public void setRoot(TableauNode r) {
    this.root = r;
  }

  /** The root of the tableau after it has been condensed */
  // private TableauNode condensedRoot = null;

  private Collection<TableauFormula> alphas = new ArrayList<TableauFormula>();
  private Collection<TableauFormula> betas = new ArrayList<TableauFormula>();
  private Collection<TableauFormula> deltas = new ArrayList<TableauFormula>();
  private Collection<TableauFormula> gammas = new ArrayList<TableauFormula>();
  private Collection<TableauFormula> brownEQs = new ArrayList<TableauFormula>();
  private Collection delta0Literals = new ArrayList();

  /** Comprehension schema list - remembers predicate formulas
   * that say that a variable is a member of a larger set.
   */
  private Collection classMemberPredicates = new ArrayList();

  /** Used to remember equalities of the form
   * a = the-class-of-all X P --> this is necessary for
   * the implementation of the comprehension trigger rule.
   */
  private Collection classMemberEQs = new ArrayList();

  /** Maps PredicateFormulas to the Substitutions that
   * will trigger the comprehension schema.
   */
  private Map comprehensionTriggers = new HashMap();

  /** The knowledge base associated with this tableau */
  private Theory theory;

  // the only listener will be a JLabel...
  // private List regularityListeners = new ArrayList(1);

  /**
   * True if the tree is closed.  This is a bound property.
   */
  // TODO: maybe make this a map of TableauNodes to MultiBranchClosers to
  // track which ones have been closed.  This map would need to be passed
  // to certain methods or referred to by them.
  private MultiBranchCloser closer;
  
  // TODO: consider maintaining a list of MBCs that are valid but don't
  // completely finish the proof.

  private transient PropertyChangeSupport pcs =
    new PropertyChangeSupport(this);

  // A bound (and constrained?) property.
  private int qLimit = 1;

  /**
   * Used to add a class member predicate formula to this
   * tableau's list.
   * @param p The formula to add.
   */
  public void addClassMemberPredicate(PredicateFormula p) {
    this.classMemberPredicates.add(p);
  }

  /**
   * Gets this tableau's list of class member predicates
   * (formulas that say that a variable is a member of a
   * larger set).
   * @return The Collection of class member predicates.
   */
  public Collection getClassMemberPredicates() {
    return this.classMemberPredicates;
  }

  /**
   * Removes a class member predicate formula from this
   * tableau's list.
   * @param p The formula to remove.
   */
  public void removeClassMemberPredicate(PredicateFormula p) {
    this.classMemberPredicates.remove(p);
  }

  public void addClassMemberEQ(Predicate p) {
    this.classMemberEQs.add(p);
  }

  public Collection getClassMemberEQs() {
    return this.classMemberEQs;
  }

  public void removeClassMemberEQ(Predicate p) {
    this.classMemberEQs.remove(p);
  }

  public void addComprehensionTrigger(PredicateFormula pf, ClassTerm ct) {
    this.comprehensionTriggers.put(pf, ct);
  }

  public Map getComprehensionTriggers() {
    return this.comprehensionTriggers;
  }

  public void removeComprehensionTrigger(PredicateFormula pf) {
    this.comprehensionTriggers.remove(pf);
  }

  public TableauNode getRoot() {
    return this.root;
  }

  public void condense() {
    if (this.closer != null) {
      this.closer.condense();
    }
  }
  
  /**
   * Gets the root of the condensed tableau.
   * @return The root of the condensed tableau.
  public TableauNode getCondensedRoot() {
    return this.condensedRoot;
  }
   */

  /**
   * This method is used by the Condenser to reset the tree
   * to have a condensed structure.
   * @param tn The condensed root of the tableau.
  public void setCondensedRoot(TableauNode tn) {
    this.condensedRoot = tn;
  }
   */

  /**
   * Gets the knowledge base associated with this tableau
   * @return Theory
   */
  public Theory getTheory() {
    return this.theory;
  }

  public Collection getAlphas() {
    return this.alphas;
  }

  /*
   * This needs to disallow duplicates in an efficient way.  Actually,
   * that's not such a problem since the so-called duplicates you are
   * worried about are not really duplicates as they have different
   * birthplaces.
   */
  void addAlpha(AlphaFormula af) {
    this.alphas.add(af);
  }

  void removeAlpha(AlphaFormula af) {
    this.alphas.remove(af);
  }

  void setAlphas(Collection alphas) {
    this.alphas = alphas;
  }

  /**
   * Adds <code>eq</code> to the list of positive equalities with a
   * skolem argument.
   * @param eq must be a positive equalities with a skolem argument.
   */
  public void addBrownEQ(TableauFormula eq) {
    this.brownEQs.add(eq);
  }

  public Collection getBrowns() {
    return this.brownEQs;
  }

  public Collection getBetas() {
    return this.betas;
  }

  void addBeta(BetaFormula af) {
    this.betas.add(af);
  }

  void setBetas(Collection betas) {
    this.betas = betas;
  }

  void removeBeta(BetaFormula af) {
    this.betas.remove(af);
  }

  // Begin code for INDUCTION:

  public Collection getDelta0Literals() {
    return this.delta0Literals;
  }

  void addDelta0Literal(TableauFormula df) {
    this.delta0Literals.add(df);
  }

  void setDelta0Literals(Collection delta0Literals) {
    this.delta0Literals = delta0Literals;
  }

  void removeDelta0Literal(TableauFormula f) {
    this.delta0Literals.remove(f);
  }

  // End code for INDUCTION.

  public Collection getDeltas() {
    return this.deltas;
  }

  void addDelta(DeltaFormula df) {
    this.deltas.add(df);
  }

  void removeDelta(DeltaFormula df) {
    this.deltas.remove(df);
  }

  void setDeltas(Collection deltas) {
    this.deltas = deltas;
  }

  public Collection getGammas() {
    return this.gammas;
  }

  void addGamma(GammaFormula gf) {
    //    System.out.println("Add Gamma Called: " + gf);
    this.gammas.add(gf);
  }

  void removeGamma(GammaFormula gf) {
    this.gammas.remove(gf);
  }

  void setGammas(Collection gammas) {
    this.gammas = gammas;
  }

  public int getQLimit() {
    return this.qLimit;
  }

  public void setQLimit(final int lim) {
    int old = this.qLimit;
    this.qLimit = lim;
    this.pcs.firePropertyChange("qLimit", old, lim);
  }

  public MultiBranchCloser getCloser() {
    return this.closer;
  }

  private void setCloser(MultiBranchCloser closer) {
    MultiBranchCloser old = this.closer;
    this.closer = closer;
    this.pcs.firePropertyChange("closer", old, closer);
  }

  // Source of bound properties.

  public void addPropertyChangeListener(PropertyChangeListener l) {
    this.pcs.addPropertyChangeListener(l);
  }

  public void addPropertyChangeListener(String prop,
                                        PropertyChangeListener l) {
    this.pcs.addPropertyChangeListener(prop, l);
  }

  public void removePropertyChangeListener(PropertyChangeListener l) {
    this.pcs.removePropertyChangeListener(l);
  }

  public void removePropertyChangeListener(String prop,
                                           PropertyChangeListener l) {
    this.pcs.removePropertyChangeListener(prop, l);
  }

//   public boolean isClosed() {
//     return this.root.isClosed();
//   }

  public String toString() {
    final StringBuffer sb = new StringBuffer();
    this.root.applyToLeaves(new Operator() {
        public void exec(Object o) {
          TableauNode node = (TableauNode) o;
          sb.append(node.path() + "\n");
          Collection goals = node.getGoalsUnusedAnywhere();
          Collection hyps = node.getHypsUnusedAnywhere();
          if (!goals.isEmpty()) {
            if (!hyps.isEmpty()) {
              if (hyps.size() == 1)
                sb.append("Suppose:\n");
              else
                sb.append("Suppose all of the following:\n");
              bitnots.util.Collections.appendToBuffer(hyps, sb, "\n");
              if (goals.size() == 1)
                sb.append("and show:\n");
              else
                sb.append("and show one of the following:\n");
              bitnots.util.Collections.appendToBuffer(goals, sb, "\n");
            } else {
              if (goals.size() == 1)
                sb.append("Show:\n");
              else
                sb.append("Show one of the following:\n");
              bitnots.util.Collections.appendToBuffer(goals, sb, "\n");
            }
          } else if (!hyps.isEmpty()) {
            if (hyps.size() == 1)
              sb.append("Show that the following is false:\n");
            else
              sb.append("Show that the following are contradictory:\n");
            bitnots.util.Collections.appendToBuffer(hyps, sb, "\n");
          }
        }
      });
    //    this.root.toStringHelper(sb, "");
    return sb.toString();
  }

  /*public TableauNode createTableauNode() {
    return new TableauNode(this, new ArrayList(), new ArrayList());
  }*/

  /**
   * This empties the list of brown equalities, applying Brown's rule
   * to each.
   */
  public boolean applyAllBrowns() {
    Collection browns = this.getBrowns();
    if (browns.isEmpty())
      return false;
    else {
      Iterator it = browns.iterator();
      while (it.hasNext()) {
        TableauFormula tf = (TableauFormula) it.next();
        it.remove();
        Predicate eq = (Predicate) tf.getFormula();
        Iterator terms = eq.arguments();
        Term t1 = (Term) terms.next();
        Term t2 = (Term) terms.next();
        if (t1 instanceof Function) {
          int index1 = ((Function) t1).isSkolem();
          if (index1 != -1) {
            if (t2 instanceof Function) {
              int index2 = ((Function) t2).isSkolem();
              if (index2 != -1) {
                // both are skolems
                if (index2 < index1) {
                  // remove t1
                  TableauNode home = tf.getBirthPlace();
                  // TODO: look everywhere below home for occurrences
                  // of t1 and replace them with t2
                  home.applyBrownsRule((Function) t1, t2);
                } else {
                  // remove t2
                  TableauNode home = tf.getBirthPlace();
                  // TODO: look everywhere below home for occurrences
                  // of t2 and replace them with t1
                  home.applyBrownsRule((Function) t2, t1);
                }
              } else {
                // t1 is skolem, t2 is not
                // remove t1
                TableauNode home = tf.getBirthPlace();
                // TODO: look everywhere below home for occurrences
                // of t1 and replace them with t2
                home.applyBrownsRule((Function) t1, t2);
              }
            } else {
              // t1 is skolem, t2 is not
              // remove t1
              TableauNode home = tf.getBirthPlace();
              // TODO: look everywhere below home for occurrences
              // of t1 and replace them with t2
              home.applyBrownsRule((Function) t1, t2);
            }
          } else {
            // t1 is not skolem
          }
        } else {
          // t1 is not skolem
        }
      }
      // there are no more brownEQs
      return true;
    }
  }

  public MultiBranchCloser backtrackingUnify() {
    MultiBranchCloser closer = null;
    if (this.root != null) {
      this.root.backtrackingPassOne();
      closer = MultiBranchCloser.backtrackingPassTwo(this.root);
      if (closer != null) {
        this.setCloser(closer);
      }
    }
    return closer;
  }

  public MultiBranchCloser backtrackingUnifyEquality() {
    MultiBranchCloser closer = null;
    if (this.root != null) {
      this.root.backtrackingPassOneEquality(this.theory.getCongruenceClosure()); // new DSTGraph());
      closer = MultiBranchCloser.backtrackingPassTwo(this.root);
      if (closer != null) {
        this.setCloser(closer);
      }
    }
    return closer;
  }

  // It should not be hard to keep track of these so that this is
  // constant time.  I'm not sure that's important, though.
  public List<TableauNode> undoneLeaves() {
    if (root == null)
      return new ArrayList<TableauNode>(0);
    else {
      ArrayList<TableauNode> l = new ArrayList<TableauNode>(2);
      this.root.undoneLeaves(l);
      return l;
    }
  }

  public Iterator<TableauNode> iterator() {
    return this.preorderIterator();
  }

  private Iterator<TableauNode> preorderIterator() {
    return new PreorderIterator();
  }

  public void setEpsilonToolkit(EpsilonToolkit e) {
    this.epsilonToolkit = e;
  }

  public EpsilonToolkit getEpsilonToolkit() {
    if (this.epsilonToolkit == null)
      EpsilonToolkit.createEpsilonToolkitForTableau(this);
    return this.epsilonToolkit;
  }

  /**
   * The inner class PreorderIterator allows elements in the list to be
   * retrieved in pre-order, using the methods specified by the
   * interface <code>java.util.Iterator</code>
   * @see Iterator */
  public class PreorderIterator implements Iterator<TableauNode>, Cloneable {

    /**
     * The top of this stack is what will be returned by the next call
     * to next().  */
    private ExposedLinkedList<TableauNode> stack = new ExposedLinkedList<TableauNode>();

    public Object clone() {
      PreorderIterator value = new PreorderIterator();
      value.stack = this.stack.clone();
      return value;
    }

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
      Iterator<TableauNode> it = retVal.getChildren().iterator();
      // Put the children of retVal into ell left to right then
      // prepend ell to the stack.
      ExposedLinkedList<TableauNode> ell = new ExposedLinkedList<TableauNode>();
      while (it.hasNext()) {
        ell.addEnd(it.next());
      }
      this.stack.prepend(ell);
      return retVal;
    }

    /**
     * The <code>remove</code> method is not supported.
     * @exception UnsupportedOperationException.  */
    public void remove() {
      throw new UnsupportedOperationException();
    }

    /**
     * Creates a new <code>PreorderIterator</code> instance.
     * */
    public PreorderIterator() {
      if (Tableau.this.root != null)
        this.stack.push(Tableau.this.root);
    }
  }

  /**
   * Adds an ActionListener to the tableau (allows a component
   * to register interest in the only events this fires:
   * RegularityEnforcedEvents...).
   * @param al The ActionListener
  public void addActionListener(ActionListener al) {
    this.regularityListeners.add(al);
  }
   */

  /**
   * Removes an ActionListener.
   * @param al The ActionListener
  public void removeActionListener(ActionListener al) {
    this.regularityListeners.remove(al);
  }
   */

  /**
   * This is called when regularity deletes a formula from a node.
   * This method tells whoever's listening about it.
   * @param e The ActionEvent
  void fireRegularityEnforcedEvent(ActionEvent e) {
    for (int i = 0; i < this.regularityListeners.size(); i++) {
      ((ActionListener) this.regularityListeners.get(i)).actionPerformed(e);
    }
  }
   */

  public Tableau(Formula f) {
    this(f, new Theory());
  }

  /**
   * Creates a tableau with a base formula that is associated
   * with a knowledge base
   * @param f The tableau's root
   * @param t The tableau's knowledge base
    */
  public Tableau(Formula f, Theory t) {
    TableauFormula pf = TableauFormula.createTableauFormula(f, false);
    ArrayList goals = new ArrayList(1);
    goals.add(pf);
    this.root = new TableauNode(this, java.util.Collections.EMPTY_SET, goals);
    this.theory = t;
    // initialize class member equalities field
    this.classMemberEQs.addAll(t.getClassMemberEQs());
  }

  public TableauNode getFirstLeaf() {
    TableauNode value = this.root;
    while (!value.isLeaf()) {
      value = value.getChildren().get(0);
    }
    return value;
  }
  public TableauNode getLastLeaf() {
    TableauNode value = this.root;
    while (!value.isLeaf()) {
      value = value.getChildren().get(value.getChildren().size() - 1);
    }
    return value;
  }
}
