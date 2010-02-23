package bitnots.tableaux;

import javax.swing.tree.*;
import java.util.*;
import java.lang.reflect.*;
import java.io.*;
import bitnots.expressions.*;
//import bitnots.util.*;

public abstract class TableauFormula implements Serializable {

  // private static int code = 0;
  private static final Hashtable TABLE = new Hashtable(20);

  // The purpose of this is so the by changing only one line of code,
  // much of the behavior of the class changes from positive to
  // negative.  This must be set in the constructor.
  /**
   * This is used to determine the positive or negative behavior of
   * the formula.
   */
  protected FormulaHelper help;

  /** The substitution this TableauFormula depends on (if any) */
  private Substitution dependsOn;

  protected Formula formula;
  // needed?
  protected TableauNode birthPlace;
  protected List<TableauFormula> parents = new ArrayList<TableauFormula>(1);
  /**
   * Collection of nodes at and below which this formula is to be
   * considered used.  It must be at and below because of possible
   * splicing.  So this contains nodes at which the children of this
   * formula were born, for example.
   */
  protected Collection<TableauNode> used = new ArrayList<TableauNode>(1);

  protected Set<TableauNode.Split> involvedSplits = new TreeSet<TableauNode.Split>();

  public List<TableauFormula> getParents() {
    return Collections.unmodifiableList(this.parents);
  }

  public Set<TableauNode.Split> getInvolvedSplits() {
    return this.involvedSplits;
  }

//   public void addSplit(Split split) {
//     this.involvedSplits.add(split);
//   }

//   public void setInvolvedSplits(Collection involvedSplits) {
//     this.involvedSplits = involvedSplits;
//   }

  public String toString() {
    return this.getFormula().toString();
  }

  /**
   * @return true if the argument and the receiver have
   * <i>identical</i> formulas.  */
  public boolean equals(Object o) {
    return super.equals(o);
//     if (o instanceof TableauFormula) {
//       TableauFormula t = (TableauFormula) o;
//       if (t.formula == this.formula &&
//           this.birthPlace == t.birthPlace &&
//           this.getSign() == t.getSign())
//         System.out.println(this.toString() + " == " + t);//////////////
//         return true;
//     }
//     return false;
  }

  public int hashCode() {
      return super.hashCode();
//    return this.formula.hashCode();
  }

  /**
   * Returns true if this has ever been used anywhere.
   */
//   public boolean isUsed() {
//     return !this.used.isEmpty();
//   }

  // TODO: fix
  /**
   * This does not work yet.  It should return true if the receiver
   * has been used in an ancestor of <code>node</code>.
   */
  public boolean isUsedAtAncestorOf(TableauNode node) {
    if (this.used.isEmpty())
      return false;
    do {
      if (this.used.contains(node))
        return true;
      node = node.getParent();
    } while (node != null);
    return false;
  }

  public boolean isUsedAnywhere() {
    return !this.used.isEmpty();
  }

  /**
   * Add <b>node</b> to the list of nodes at which the receiver has
   * been used.
   * @param node the node at (and below) which the receiver is to be
   * considered used.  */
  protected void addUsedAt(TableauNode node) {
    this.used.add(node);
  }

  /**
   * Make <b>pn</b> the birthPlace of the receiver.  If the value of
   * birthPlace was previously null, then this also tells the tableau
   * structure associated with <b>pn</b> about the addition of this
   * new formula to the tableau.  This must include its addition to the
   * Tableau's list of alphas, betas, etc., as apropos. This must also
   * include its addition to the node's new hyps or goals.
   *
   * @param pn the new birthPlace of the receiver.
  public final void insertInto(TableauNode tn) {
    this.registerWithTableau(tn.getTableau());
    this.addMeToTableauNode(tn);
    this.setBirthPlace(tn);
  }
   */

  /** CLIENT-VIEW INVARIANT: if c contains a formula, then it contains
   * all of the ancestors of that formula.  
   */
  final void addAncestors(Collection<TableauFormula> c) {
    if (c.add(this))
      for (TableauFormula parent: this.getParents())
        parent.addAncestors(c);
  }
  
  /** This adds the receiver to the list of alphas, betas, etc., of
   * the tableau of pn.
   */
  protected abstract void registerWithTableau(Tableau p);

  public abstract void unregisterWithTableau(Tableau p);

  protected final void setBirthPlace(TableauNode pn) {
    this.birthPlace = pn;
  }

  public final TableauNode getBirthPlace() {
    return this.birthPlace;
  }

  /**
   * Add <b>pf</b> to the list of formulas involvedSplits in the creation of
   * the receiver.
   *
   * @param pf a <code>TableauFormula</code> value
   */
  protected void addParent(TableauFormula pf) {
    this.parents.add(pf);
  }

  /**
   * Set the parents of the receiver to be <b>list</b>.
   *
   * @param list a <code>Collection</code> value
   */
  protected void setParents(List pf) {
    this.parents = pf;
  }

  public boolean getSign() {
    return this.help.getSign();
  }

  public Formula getFormula() {
    return this.formula;
  }

  /**
   * This adds to the current tableau the children of this
   * TableauFormula.  Make new nodes to be added to the tableau and put
   * them in place.  Set these nodes as the birth places of the new
   * formulas.  Mark the receiver used if appropriate.  Also remove
   * the receiver from the list of alphas, betas, etc.
   */
  public abstract void expand();

  /**
   * Creates new TableauNodes containing the child formulas of the
   * receiver, and posts those nodes to some or every branch
   * containing the birthPlace of the receiver.  This sets the parent
   * fields of the new nodes correctly.  This also tells the receiver
   * that it was used on the new TableauNodes created herein.
   */
  protected abstract void attachChildNodes();

  /**
   * This sends the receiver to argument through either addNewGoal or
   * addNewHyp.
  //  protected abstract void addMeToTableauNode(TableauNode pn);
  protected final void addMeToTableauNode(TableauNode pn) {
    this.help.addToTableauNode(pn, this);
  }
   */

//    /**
//     * Return a Collection containing the TableauNodes that are needed to
//     * expand the receiver.  These nodes (or clones of them) are
//     * suitable for being added to each branch containing the receiver.
//     * This will use the createChildren method and populate the nodes
//     * with the correct formulas.  However, these will not be linked to
//     * other tableau nodes.
//     * @return a Collection containing the TableauNodes that are needed to
//     * expand the receiver.
//     */
//    protected abstract List createChildNodes();

  /**
   * Return a Collection containing Collections of TableauFormulas
   * that are needed to expand the receiver.  Each element Collection
   * will contain TableauFormulas that will be added to a single
   * TableauNode child of the birthPlace of the receiver.  Each of
   * these child nodes will be on a separate branch.
   * @return a Collection containing Collections of TableauFormulas that
   * are needed to expand the receiver.  */
  protected abstract Collection<Collection<TableauFormula>> createChildren();

  /**
   * This applies intermediate alpha (but not delta) rules.  Creates
   * a collection of new TableauFormulas based on <code>f</code> that will be
   * children of the receiver.  We don't want to expand delta rules
   * here because we may want to expand them using a delta inverse
   * rule.
   *
   * @param f The return value will be a wrapper around
   * <code>f</code>... sort of.
   * @return a collection of TableauFormulas that are derived from the
   * input Formula.
   */
  protected Collection<TableauFormula> createChildFormulaCollectionWithoutExpansion(Formula f) {
    return this.createChildFormulaCollectionWithoutExpansion(f, this.getSign());
  }

  protected Collection createChildFormulaCollectionWithoutExpansion(Formula f, boolean sign) {
    return this.createChildFormulaCollectionWithoutExpansion(f, this.involvedSplits, sign);
  }

  protected Collection createChildFormulaCollectionWithoutExpansion(
    Formula f, Set involvedSplits, boolean sign) {
    // need the parents I send to have birthPlaces.

    // call getConstructor from TableauFormula.
    TableauFormula retVal =
      TableauFormula.createTableauFormula(
        f, sign, involvedSplits,
        Collections.singletonList(this.getAncestorWithHome()),
        this.dependsOn);
    if (retVal instanceof AlphaFormula) {
      // then go ahead and reduce them.
      Collection intermedSubs = retVal.createChildren();
      return (Collection) intermedSubs.iterator().next();
    }
    return java.util.Collections.singleton(retVal);
  }

  /**
   * This applies intermediate alpha and delta rules.  Creates
   * new TableauFormulas based on <code>f</code> that will be a
   * children of the receiver.
   *
   * @param f The return value will be a wrapper around
   * <code>f</code>... sort of.
   * @return a collection of TableauFormulas that are derived from the
   * input Formula.
   */
  protected Collection<TableauFormula> createChildFormulaCollection(Formula f) {
    return this.createChildFormulaCollection(f, this.getSign());
  }

  protected Collection<TableauFormula> createChildFormulaCollection(Formula f,
                                                                    boolean sign) {
    return this.createChildFormulaCollection(f, this.involvedSplits, sign);
  }

  protected Collection<TableauFormula> createChildFormulaCollection(Formula f,
                                                                    Set involvedSplits,
                                                                    boolean sign) {
    // need the parents I send to have birthPlaces.

    // call getConstructor from TableauFormula.
    TableauFormula retVal =
      TableauFormula.createTableauFormula(
        f, sign, involvedSplits,
        Collections.singletonList(this.getAncestorWithHome()),
        this.dependsOn);
    if (retVal instanceof AlphaFormula || retVal instanceof DeltaFormula) {
      // then go ahead and reduce them.
      Collection<Collection<TableauFormula>> intermedSubs =
        retVal.createChildren();
      return intermedSubs.iterator().next();
    }
    return java.util.Collections.singleton(retVal);
  }



  public TableauFormula getAncestorWithHome() {
    if (this.getBirthPlace() != null)
      return this;
    else {
      TableauFormula current = this;
      while (true) {
        if (current.parents != null && !current.parents.isEmpty()) {
          TableauFormula parent =
            (TableauFormula) current.getParents().iterator().next();
          if (parent.getBirthPlace() != null)
            return parent;
          else
            current = parent;
        } else {
          throw new IllegalStateException();
        }
      }
    }
  }

  // This needs to figure out what kind of TableauFormula to create.
  // But then it needs to have the necessary arguments for that type.
  // Since there is no parent, this is probably creating a new Tableau.
  // The caller is responsible to create the TableauNodes, etc.
  /**
   * Create a new TableauFormula using the constructor that was
   * registered for this formula type.
   * @param f the formula to wrap.
   * @param pos whether this should be a positive formula.
   * @throws IllegalArgumentException if the type of <code>f</code>
   * has not been registered.
   */
  public static Constructor getAppropriateConstructor(Formula f,
                                                      boolean pos) {

    Constructor[] value = (Constructor[]) TABLE.get(f.getClass());

    if (value == null)
      throw new IllegalArgumentException("Unregistered formula type.");
    return pos ? value[0] : value[1];
  }

  // This needs to figure out what kind of TableauFormula to create.
  // But then it needs to have the necessary arguments for that type.
  // Since there is no parent, this is probably creating a new Tableau.
  // The caller is responsible to create the TableauNodes, etc.
  /**
   * Create a new TableauFormula using the constructor that was
   * registered for this formula type.  <strong>Warning!</strong> This
   * does not set the birthPlace.
   * @param f the formula to wrap.
   * @param pos whether this should be a positive formula.
   * @throws IllegalArgumentException if the type of <code>f</code>
   * has not been registered.
   */
  public static TableauFormula createTableauFormula(Formula f, boolean pos) {
    return TableauFormula.createTableauFormula(
      f, pos, new TreeSet(), Collections.EMPTY_LIST,
      Substitution.createSubstitution());
  }

  /**
   * Creates a new TableauFormula and registers its dependency
   * on the specified Substitution.  <strong>Warning!</strong> This
   * does not set the birthPlace.
   * @param f the formula
   * @param pos true for positive formulas, false for negative
   * @param involved the involved splits
   * @param parents this TableauFormula's parents
   * @param s The substitution this TableauFormula depends on
   * @return a new TableauFormula
   */
  public static TableauFormula createTableauFormula(Formula f, boolean pos,
                                             Set<TableauNode.Split> involved,
                                             List parents,
                                             Substitution s) {
    // Get constructors:
    Constructor[] constr = (Constructor[]) TABLE.get(f.getClass());

    if (constr == null)
      throw new IllegalArgumentException("Unregistered formula type:" +
                                         f.getClass() + "\nsign is " + pos);
    try {
      // Call the appropriate constructor:
      TableauFormula value = (TableauFormula)
        (pos ? constr[0] : constr[1]).newInstance(new Object[] {f});
      value.involvedSplits = involved;
//      value.birthPlace = birthPlace;
      value.parents = parents;
      value.dependsOn = s;
      return value;
    } catch (InstantiationException ie) {
      throw new IllegalArgumentException(ie);
    } catch (IllegalAccessException iae) {
      throw new IllegalArgumentException(iae);
    } catch (InvocationTargetException ite) {
      throw new IllegalArgumentException(ite);
    } catch (NullPointerException npe) {
      throw new IllegalArgumentException("Unregistered formula type.", npe);
    }
  }

  /**
   * Copy constructor used by condense while building condensed tree.
   * 
   * @param old TableauFormula to clone.
   * @param nodeMap map of old TableauNodes to new ones
   * @param formulaMap map of old TableauFormulas to new ones
   * @return the copied formula
  public static TableauFormula createTableauFormula(TableauFormula old, 
                                                    HashMap<TableauNode, TableauNode> nodeMap,
                                                    HashMap<TableauFormula, TableauFormula> formulaMap) {
    // TODO: how to deal with nodeMap or formulaMap not containing what I need?
    // Perhaps go ahead and add the needed things to the map?
    TableauFormula value = old.clone();
    value.birthPlace = nodeMap.get(old.birthPlace);
    if (value.birthPlace == null)
      throw new Error("nodeMap not ready");
    value.dependsOn = old.dependsOn;
    value.formula = old.formula;
    value.help = old.help;
    value.involvedSplits = new TreeSet<TableauNode.Split>();
    for (TableauNode.Split split: old.involvedSplits) {
      if (nodeMap.get(split.getNode()) == null)
        // now, splits can have null nodes for other reasons.
        throw new Error("nodeMap not ready");
      value.involvedSplits.add(new TableauNode.Split(nodeMap.get(split.getNode())));
    }
    value.parents = new ArrayList<TableauFormula>(old.parents.size());
    for (TableauFormula par: old.parents) {
      if (formulaMap.get(par) == null)
        throw new Error("formulaMap not ready");
      value.parents.add(formulaMap.get(par));
    }
    value.used = new ArrayList<TableauNode>(old.used.size());
    for (TableauNode used: old.used) {
      if (nodeMap.get(used) == null)
        throw new Error("nodeMap not ready");
      value.used.add(nodeMap.get(used));
    }
    return value;
  }
   */
  /**
   * Gets the substitution this TableauFormula depends on
   * @return this TableauFormula's required substitution
   */
  public Substitution getSubstitutionDependency() {
    return this.dependsOn;
  }
  
  /**
   * Make a very shallow clone.  Simply calls Object.clone.
   * @return the shallow clone.
   */
  @Override
  public TableauFormula clone() {
    try {
      return (TableauFormula) super.clone();
    } catch (CloneNotSupportedException e) {
      throw new Error();
    }
  }

  /**
   * Registers the <code>formulaClass</code> to be handled by the
   * array of constructors given.  When createTableauFormula is called,
   * the appropriate constructor is used.
   *
   * @param formulaClass a subclass of <code>Formula</code>.
   * @param constructors an array of length two.  The first will be
   * called when a positive TableauFormula is being created and the
   * second will be called when a negative TableauFormula is being
   * created.  These constructors must take a single
   * <code>Formula</code> as an argument.
   * @return the array that was previously associated with
   * <code>formulaClass</code> if there was one.  Otherwise,
   * <code>null</code>.
   */
  public static Constructor[] registerConstructors(
    Class formulaClass, Constructor[] constructors) {
    if (Formula.class.isAssignableFrom(formulaClass))
      return (Constructor[]) TABLE.put(formulaClass, constructors);
    else
      throw new IllegalArgumentException();
  }

  /**
   * Registers the <code>formulaClass</code> to be handled by the
   * constructor given.  When createTableauFormula is called, the
   * appropriate constructor is used.
   *
   * @param formulaClass a subclass of <code>Formula</code>.
   * @param sign the sign associated with this constructor.
   * @param constructor The constructor to be called when a
   * TableauFormula is being created.  This constructor must take a
   * single <code>Formula</code> as an argument.
   * @return the constructor that was previously associated with
   * <code>formulaClass</code> if there was one.  Otherwise,
   * <code>null</code>.
   * @throws IllegalArgumentException if <b>formulaClass</b> is not a
   * subclass of Formula.
   */
  public static Constructor registerConstructor(Class formulaClass, 
                                                boolean sign, 
                                                Constructor constructor) {
    if (Formula.class.isAssignableFrom(formulaClass)) {
      final int i = sign ? 0 : 1;
      Constructor[] cons = (Constructor[]) TABLE.get(formulaClass);
      if (cons == null) {
        cons = new Constructor[2];
      }
      Constructor retVal = cons[i];
      cons[i] = constructor;
      TABLE.put(formulaClass, cons);
      return retVal;
    } else
      throw new IllegalArgumentException();
  }

  /**
   *
   */
  protected TableauFormula(Formula f, FormulaHelper help) {
    // TableauFormula.code++;
    this.formula = f;
    this.help = help;
    //    this(f, help);
  }
  
}

