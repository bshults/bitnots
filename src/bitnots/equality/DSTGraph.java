package bitnots.equality;

import bitnots.theories.*;
import bitnots.expressions.*;
import bitnots.tableaux.*;
import bitnots.util.*;
import java.util.*;

/**
 * Downey Sethi Tarjan Congruence Closure algorithm with some
 * "enhancements" by Benjamin Shults to allow limited variable
 * substitution.
 * @author <a href="mailto:b-shults@bethel.edu">Benjamin Shults</a>
 * @version 1.0
 */

public class DSTGraph implements Cloneable {

  /**
   * maps a DRule (vertex) to an ArrayList of CRules (edges)
   */
  private HashMap graph;

  /**
   * maps a signature (a term involving a function applied to elements
   * of ccConstants) to a DRule (vertex).
   */
  private HashMap signatureToVertex;
  
  /**
   * maps a Kconstant to a TreeSet of variables in the language that
   * are in the same equivalence class as the key.
   */
  private HashMap constantToVariables;
  
  /**
   * maps a Kconstant to a term in the language that is in the same
   * equivalence class as the key.
   */
  private HashMap constantToTerm;
  
  /**
   * The variables that are bound by a substitution applied to this
   * structure.
   */
  private HashSet boundVars;

  /**
   * Maps a function symbol from the language to a HashSet of DRules
   * whose left-hand sides are constructed by that symbol.  This
   * really only need apply to symbol with positive arity, I believe.
   */
  private HashMap symbolToDRuleList;

  /**
   * Maps a signature to a list of pairs.  Each pair has another
   * signature that is in in different equivalence class from the
   * input and a substitution that can be applied to make the switch.
   */
  private HashMap constantToECSwitches;
  
  /**
   * maps an element, c, of ccConstants to an ArrayList of DRules (vertices)
   * that contain c as subterms of the lhs.
   */
  private HashMap usageTableD;

  /**
   * Maps an element, c, of ccConstants to the CRule whose lhs is c or to a
   * CRule that is the end of the chain of CRules starting at c.
   */
  private HashMap usageTableC;

  /**
   * The constants that are introduced just for the purpose of
   * congruence closure.
   */
  // Here, I use the fact that it's a list to decide the orientation
  // of CRules.
  private ArrayList ccConstants;

  /**
   * The equalities that still need to be processed.
   */
  // TODO: this may as well be a set.
  private ArrayList unprocessedEqualities;

  /**
   * List of the TableauFormulas that were used to create this
   * congruence closure.
   */
  // TODO: this may as well be a set.
  private List tableauFormulas;

  /**
   * True if this was nontrivially and successfully used in the
   * current attempt.
   */
  private boolean used = false;

  public void setUsed(boolean u) {
    this.used = u;
  }

  /**
   * Return true if this DSTGraph was used in the unification process.
   */
  public boolean wasUsed() {
    return this.used;
  }

  /**
   * Creates a congruence closure for the empty theory.  I.e.,
   * everything is empty.
   */
  public DSTGraph() {
    this(java.util.Collections.EMPTY_SET);
  }

  /**
   * Creates the congruence closure for the theory of the given
   * equalities.
   * @param equalities may be a list of Predicates or a list of
   * TableauFormulas.
   */
  public DSTGraph(Collection equalities) {
    this.graph = new HashMap();
    this.signatureToVertex = new HashMap();
    this.constantToVariables = new HashMap();
    this.constantToTerm = new HashMap();
    this.boundVars = new HashSet();
    this.symbolToDRuleList = new HashMap();
    this.constantToECSwitches = new HashMap();
    this.usageTableD = new HashMap();
    this.usageTableC = new HashMap();
    this.ccConstants = new ArrayList();
    bitnots.util.Collections.deleteIf(equalities, new PredicateBlock() {
        public boolean test(Object o) {
          TableauFormula tf = (TableauFormula) o;
          return !tf.getSubstitutionDependency().isEmpty();
        }
      });
    this.tableauFormulas = new ArrayList(equalities.size());

    Iterator eqsIt = equalities.iterator();
    if (eqsIt.hasNext()) {
      Object item = eqsIt.next();
      if (item instanceof TableauFormula) {
        this.unprocessedEqualities = new ArrayList(equalities.size());
        this.unprocessedEqualities.add(((TableauFormula) item).getFormula());
        this.tableauFormulas.add(item);
        while (eqsIt.hasNext()) {
          this.unprocessedEqualities.add(((TableauFormula) (item = eqsIt.next())).getFormula());
          this.tableauFormulas.add(item);
        }
      } else
        this.unprocessedEqualities = new ArrayList(equalities);
    } else
      this.unprocessedEqualities = new ArrayList(0);
    this.createCongruenceClosure();
  }

  /**
   * The list of positive TableauFormulas wrapping equalities used to
   * create this graph.
   */
  public List getTableauFormulas() {
    return this.tableauFormulas;
  }

  /**
   * This clones the underlying structures.
   */
  public Object clone() {
    // TODO: this would be more efficient if I called a million args
    // constructor sending it the clones of the structures.
    DSTGraph value = new DSTGraph();
    value.unprocessedEqualities.addAll(this.unprocessedEqualities);
    Iterator it = this.constantToVariables.entrySet().iterator();
    while (it.hasNext()) {
      Map.Entry pair = (Map.Entry) it.next();
      TreeSet old = (TreeSet) pair.getValue();
      if (old != null) {
        TreeSet newList = (TreeSet) old.clone();
        value.constantToVariables.put(pair.getKey(), newList);
      }
    }
    value.constantToTerm.putAll(this.constantToTerm);
    it = this.graph.entrySet().iterator();
    while (it.hasNext()) {
      Map.Entry pair = (Map.Entry) it.next();
      ArrayList old = (ArrayList) pair.getValue();
      if (old != null) {
        ArrayList newList = (ArrayList) old.clone();
        value.graph.put(pair.getKey(), newList);
      }
    }
    it = this.constantToECSwitches.entrySet().iterator();
    while (it.hasNext()) {
      Map.Entry pair = (Map.Entry) it.next();
      ArrayList old = (ArrayList) pair.getValue();
      if (old != null) {
        ArrayList newList = (ArrayList) old.clone();
        value.constantToECSwitches.put(pair.getKey(), newList);
      }
    }
    value.signatureToVertex.putAll(this.signatureToVertex);
    it = this.usageTableD.entrySet().iterator();
    while (it.hasNext()) {
      Map.Entry pair = (Map.Entry) it.next();
      ArrayList old = (ArrayList) pair.getValue();
      if (old != null) {
        ArrayList newList = (ArrayList) old.clone();
        value.usageTableD.put(pair.getKey(), newList);
      }
    }
    value.usageTableC.putAll(this.usageTableC);
    value.ccConstants.addAll(this.ccConstants);
    value.tableauFormulas.addAll(this.tableauFormulas);
    it = this.symbolToDRuleList.entrySet().iterator();
    while (it.hasNext()) {
      Map.Entry pair = (Map.Entry) it.next();
      HashSet old = (HashSet) pair.getValue();
      if (old != null) {
        HashSet newList = (HashSet) old.clone();
        value.symbolToDRuleList.put(pair.getKey(), newList);
      }
    }
    value.boundVars.addAll(this.boundVars);
    return value;
  }

  public String toString() {
    StringBuffer sb = new StringBuffer("K = {");
    Iterator it = this.ccConstants.iterator();
    if (it.hasNext()) {
      sb.append(it.next());
    }
    while (it.hasNext()) {
      sb.append(", ");
      sb.append(it.next());
    }
    sb.append("}\nE = {\n");
    it = this.unprocessedEqualities.iterator();
    if (it.hasNext()) {
      sb.append(it.next());
    }
    while (it.hasNext()) {
      sb.append('\n');
      sb.append(it.next());
    }
    sb.append("}\nDRules = {\n");
    it = this.graph.keySet().iterator();
    if (it.hasNext()) {
      sb.append(it.next());
    }
    while (it.hasNext()) {
      sb.append('\n');
      sb.append(it.next());
    }
    sb.append("}\nFind each element of K = {\n");
    it = this.ccConstants.iterator();
    while (it.hasNext()) {
      Function c = (Function) it.next();
      CRule rule = (CRule) this.usageTableC.get(c);
      if (rule != null) {
        sb.append(c);
        sb.append(" -> ");
        sb.append(rule.getRHS());
        break;
      }
    }
    while (it.hasNext()) {
      Function c = (Function) it.next();
      CRule rule = (CRule) this.usageTableC.get(c);
      if (rule != null) {
        sb.append('\n');
        sb.append(c);
        sb.append(" -> ");
        sb.append(rule.getRHS());
      }
    }
    sb.append("}\n");
    return sb.toString();
  }

  /**
   * Creates a new constant symbol and returns that function.  Also
   * adds it to ccConstants.
   */
  protected Function createNextConstant() {
    Function c = Function.createTemporaryConstant();
    this.ccConstants.add(c);
    return c;
  }

  /**
   * Adds <code>rule</code> to the list of vertices where
   * <code>c</code> is used.
   * @param c a Kconstant in the signature on the left-hand side of
   * <code>rule</code>.
   * @param rule a DRule whose left-hand side contains <code>c</code>.
   */
  protected void addToUsageTableD(Function c, DRule rule) {
    ArrayList list = (ArrayList) this.usageTableD.get(c);
    if (list == null)
      this.usageTableD.put(c, list = new ArrayList());
    list.add(rule);
  }

  /**
   * Adds <code>rule</code> to the list of vertices where
   * <code>c</code> is used.  Well, not exactly.  If there is a clash,
   * this detects the clash and deals with it by creating a new CRule
   * and adding it.  If rule does not class, this returns rule.  If
   * rule does clash, this returns the newly created CRule.
   * @param c the Kconstant on the left-hand side of
   * <code>rule</code>.
   * @param rule a CRule whose left-hand side is <code>c</code>.
   */
  // brand new rule, so far
  protected CRule addToUsageTableC(Function c, CRule rule) {
    CRule oldCR = (CRule) this.usageTableC.get(c);
    if (oldCR == null) {
      CRule value = this.find(rule);
      this.usageTableC.put(c, value);
      return rule;
    } else {
      // We need to do a deduction on CRules.  I'm just going to leave
      // the old one in and instead of adding the new one, apply
      // deduction.  But the way I'm going to apply deduction is to
      // skip deduction and jump right to orientation or deletion.

      Function t1 = this.findK((Function) oldCR.getRHS());
      Function t2 = this.findK((Function) rule.getRHS());

      if (t1 != t2) {
        return this.orientation(t1, t2);
      } else
        // TODO: not sure about this
        return null;
    }
  }

  /**
   * Adds <code>rule</code> to the graph and associates with it any
   * CRules that rewrite subterms of it's left-hand side.
   * @param rule  a new DRule.
   */
  protected void addVertexToGraph(DRule rule) {
    if (this.usageTableC.isEmpty())
      this.graph.put(rule, null);
    else {
      Term lhs = rule.getLHS();
      if (lhs instanceof Function && !((Function) lhs).isConstant()) {
        ArrayList edges = new ArrayList();
        Iterator terms = ((Function) lhs).arguments();
        // TODO: should I call find here?
        while (terms.hasNext()) {
          Function c = (Function) terms.next();
          CRule cr = (CRule) this.usageTableC.get(c);
          // This should only be called on collapsed terms.

          if (cr != null) {
            edges.add(cr);
            // shouldn't this have been taken care of when rule was
            // being constructed?
          }
        }
        this.graph.put(rule, edges);
      } else {
        // lhs is not a function
        this.graph.put(rule, null);
      }
    }
  }

  /**
   * For every DRule, drule, that contains <code>rule.getLHS()</code>
   * on it lhs, add <code>rule</code> to the list of edges coming out
   * of drule.
   */
  protected void addEdgesToGraph(CRule rule) {
    ArrayList drules = (ArrayList) this.usageTableD.get(rule.getLHS());
    if (drules != null) {
      final int size = drules.size();
      for (int i = 0; i < size; ++i) {
        DRule drule = (DRule) drules.get(i);
        this.addEdgeToGraph(drule, rule);
      }
    }
  }

  /**
   * Adds <code>cr</code> as an edge out of <code>dr</code>.
   */
  protected void addEdgeToGraph(DRule dr, CRule cr) {
    ArrayList list = (ArrayList) this.graph.get(dr);
    if (list == null)
      this.graph.put(dr, list = new ArrayList());
    list.add(cr);
  }

  /**
   * Add variables in the equivalence class of lhs to the variables in
   * the equivalence class of find(rhs).  This also notifies
   * appropriate signatures that they may be able to switch
   * equivalence classes now.
   */
  // TODO: This also notifies appropriate signatures that they may be
  // able to switch equivalence classes now.
  // TODO: This needs to propogate ECSwitches as well.
  protected void propogateVariables(CRule rule) {
    Function lhs = rule.getLHS();
    TreeSet newVars = (TreeSet) this.constantToVariables.get(lhs);
    // if every member of newVars is bound, then there is nothing to
    // propogate.
    if (newVars == null)
      return;
    else {
      bitnots.util.Collections.deleteIf(newVars, new PredicateBlock() {
          public boolean test(Object o) {
            return DSTGraph.this.boundVars.contains(o);
          }
        });
      if (newVars.isEmpty())
        return;
    }
    // ASSERTION: newVars is a nonempty list of unbound variables.
    // Get the representative of this equivalence class.
    Function rep = this.find(rule).getRHS();
    TreeSet vars = (TreeSet) this.constantToVariables.get(rep);
    if (vars == null) {
      vars = new TreeSet();
      this.constantToVariables.put(rep, vars);
    }
    // TODO: if any of the newVars are actually new, then
    if (vars.addAll(newVars)) {
      Collection dRules = (Collection) this.usageTableD.get(rep);
      if (dRules != null && !dRules.isEmpty()) {
        Iterator it = dRules.iterator();
        while (it.hasNext()) {
          // for each DRule that involves rep.
          DRule involvesRep = (DRule) it.next();
          this.findECSwitches(involvesRep);
        }
      } else
        return;
    } else
      return;
  }

  /**
   * Returns a Substitution if there is already a know way of
   * switching between these classes.  The order of the arguments
   * doesn't matter.
   */
  protected Substitution alreadySwitchable(Function c1, Function c2) {
//     Function context;
//     Function target;
//     Substitution value = null;
//     if (this.ccConstants.indexOf(c1) < this.ccConstants.indexOf(c2)) {
//       context = c2;
//       target = c1;
//     } else {
//       context = c1;
//       target = c2;
//     }
    ArrayList switches = (ArrayList) this.constantToECSwitches.get(c1);
    if (switches != null)
      for (int i = 0; i < switches.size(); ++i) {
        ECSwitch sw = (ECSwitch) switches.get(i);
        if (sw.getRepresentative1() == c2 ||
            sw.getRepresentative2() == c2)
          return sw.getSubstitution();
      }
    return null;
  }

  /**
   * Looks for ways that the given DRule might switch equivalence
   * classes by matching arguments with some other DRules whose lhs
   * have the same constructor as <code>rule</code>.
   * @param rule the DRule that is trying to switch classes.
   */
  protected void findECSwitches(DRule rule) {
    // get the DRules with the same rhs constructor as rule.
    HashSet sameConstructor = (HashSet)
      this.symbolToDRuleList.get(((ComplexTerm) rule.getLHS()).
                                 getConstructor());
    this.findECSwitches(rule, sameConstructor);
  }

  /**
   * Looks for ways that the given DRule might switch equivalence
   * classes by matching arguments with some other element of
   * sameConstructor.  Stores these possibilities in
   * constantToECSwitches.
   * @param sameConstructor is the set of other DRules whose lhs have
   * the same constructor as <code>rule</code>.
   * @param rule the DRule that is trying to switch classes.
   */
  protected void findECSwitches(DRule rule, HashSet sameConstructor) {
    Iterator same = sameConstructor.iterator();
    Function rep = rule.getRHS();
    Function sig = (Function) rule.getLHS();
    Iterator myArgs;
    // there is no match, yet
    while (same.hasNext()) {
      DRule other = (DRule) same.next();
      Substitution valueS = null;
      Function otherRep = other.getRHS();
      Function otherSig = (Function) other.getLHS();
      // if they are in different equivalence classes
      if (otherRep != rep) {
        valueS = this.alreadySwitchable(rep, otherRep);
        if (valueS != null)
          continue;
        valueS = Substitution.createSubstitution();
        myArgs = sig.arguments();
        Iterator otherArgs = otherSig.arguments();
        // go through the arguments in parallel
        while (myArgs.hasNext()) { //  && sub != null) {
          Function arg1 = (Function) myArgs.next();
          Function arg2 = (Function) otherArgs.next();
          // look for a way of matching
          if (arg1 == arg2)
            continue;
          else {
            Substitution already = this.alreadySwitchable(arg1, arg2);
            if (already != null) {
              valueS = Substitution.compatible(already, valueS);
              if (valueS == null)
                break;
              continue;
            }
            TreeSet set1 = (TreeSet) this.constantToVariables.get(arg1);
            if (set1 == null || set1.isEmpty()) {
              TreeSet set2 = (TreeSet) this.constantToVariables.get(arg2);
              // TODO: figure out why I have to see if this is empty.
              if (set2 == null || set2.isEmpty()) {
                // both are empty
                valueS = null;
                break;
              } else {
                // set2 is nonempty.
                Variable v = (Variable) set2.first();
                Term vp = v.apply(valueS);
                if (vp == v) {
                  Term term = (Term) this.constantToTerm.get(arg1);
                  if (!term.contains(v))
                    valueS = valueS.acons(v, term);
                  else {
                    valueS = null;
                    break;
                  }
                } else {
                  valueS = null;
                  break;
                }
              }
            } else {
              // set1 is nonempty.
              Variable v = (Variable) set1.first();
              Term vp = v.apply(valueS);
              if (vp == v) {
                Term term = (Term) this.constantToTerm.get(arg2);
                if (!term.contains(v))
                  valueS = valueS.acons(v, term);
                else {
                  valueS = null;
                  break;
                }
              } else {
                valueS = null;
                break;
              }
            }
          }
        }
        if (valueS != null) {
          // a match was found!
          Collection switches;
          switches = (Collection) this.constantToECSwitches.get(rep);
          if (switches == null) {
            switches = new ArrayList();
            this.constantToECSwitches.put(rep, switches);
          }
          ECSwitch sw = new ECSwitch(rep, otherRep, valueS);
          switches.add(sw);
          switches = (Collection) this.constantToECSwitches.get(otherRep);
          if (switches == null) {
            switches = new ArrayList();
            this.constantToECSwitches.put(otherRep, switches);
          }
          switches.add(sw);
        } else
          continue;
      } else
        continue;
    }
  }

  /**
   * Stored as values in a HashMap.
   */
  private class ECSwitch {
    private Function rep1;
    private Function rep2;
    private Substitution sub;
    /**
     * order of first two args doesn't matter.  The constructor
     * figures out which is which.
     */
    public ECSwitch(Function f1, Function f2, Substitution s) {
      this.rep1 = f1;
      this.rep2 = f2;
      this.sub = s;
    }
    /**
     * The KConstant that is the representative of the class to which
     * the context can switch.
     */
    public Function getRepresentative1() {
      return this.rep1;
    }
    /**
     * The KConstant that maps to this ECSwitch.
     */
    public Function getRepresentative2() {
      return this.rep2;
    }
    public Substitution getSubstitution() {
      return this.sub;
    }
  }

  /**
   * Adds rule to the list of DRules associated with the constructor
   * of its left-hand side.  This should only need to be applied to a
   * rule whose left-hand side is a function of positive arity.
   */
  protected void addToSymbolStructure(DRule rule) {
    Symbol s = ((Function) rule.getLHS()).getConstructor();
    HashSet rules = (HashSet) this.symbolToDRuleList.get(s);
    if (rules == null) {
      rules = new HashSet();
      this.symbolToDRuleList.put(s, rules);
    }
    rules.add(rule);
    this.findECSwitches(rule, rules);
  }

  /**
   * Empties the list of equalities and leaves the graph in a state
   * where some collapses might need to be applied.
   */
  // TODO: rename
  protected void extendSimplifyAndTransform() {
    final int size = this.unprocessedEqualities.size();
    // I'm going in reverse because of the remove.
    for (int i = size - 1; i >= 0; --i) {
      Predicate eqn = (Predicate) this.unprocessedEqualities.get(i);
      Iterator terms = eqn.arguments();
      Function t1 =
        this.extendAndSimplifyTermToConstant((Term) terms.next());
      Function t2 =
        this.extendAndSimplifyTermToConstant((Term) terms.next());
      // add new equality to unprocessedEqualities.
      ArrayList newTerms = new ArrayList(2);
      newTerms.add(t1);
      newTerms.add(t2);
      this.unprocessedEqualities.set(i, FormulaFactory.getFormula(
                   PredicateConstructor.EQUALITY_SYMBOL,
                   new Object[] {newTerms}));
    }
    // transform
    for (int i = size - 1; i >= 0; --i) {
      CRule rule;
      Predicate eqn = (Predicate) this.unprocessedEqualities.get(i);
      this.unprocessedEqualities.remove(i);
      Iterator terms = eqn.arguments();
      Function t1 = (Function) terms.next();
      Function t2 = (Function) terms.next();
      this.orientation(t1, t2);
    }
  }

  // TODO: rename
  /**
   * Interns all subterms of <code>t</code> and returns the DRule that
   * maps the signature of <code>t</code> to a Kconstant.  If (and
   * only if) <code>t</code> IS a Kconstant, then this returns null.
   */
  protected DRule extendAndSimplifyTerm(Term t) {
    if (t instanceof Function) {
      if (!((Function) t).isConstant()) {
        // t is a nonconstant function
        Iterator args = ((Function) t).arguments();
        // list to contain the new subterms.
        ArrayList subTerms =
          new ArrayList(((Function) t).getNumberOfArguments());
        while (args.hasNext()) {
          // get the next subterm
          Term subTerm = (Term) args.next();
          subTerms.add(
            this.findK(this.extendAndSimplifyTermToConstant(subTerm)));
        }
        // at this point, we know that subTerms is all elements of ccConstants.
        Function newT = 
          Function.createFunction((FunctionConstructor)
                                  ((Function) t).getConstructor(),
                                  subTerms);
        DRule oldRule = (DRule) this.signatureToVertex.get(newT);
        if (oldRule == null) {
          Function c = this.createNextConstant();
          DRule rule = new DRule(newT, c);
          this.constantToTerm.put(c, t);
          this.addToSymbolStructure(rule);
          // TODO: in the initialization step, the second arg should
          // be null because there are no CRules.  But, if this is
          // called later (and it should be), then we need to do more
          // work here to get all the edges out of this vertex.  In
          // the initialization phase, edges are added to the graph
          // when CRules are created.  Now, CRules already exist so I
          // need to put them into the graph now.
          this.addVertexToGraph(rule);
          this.signatureToVertex.put(newT, rule);
          final int size = subTerms.size();
          for (int i = 0; i < size; ++i)
            this.addToUsageTableD((Function) subTerms.get(i), rule);
          return rule;
        } else
          return oldRule;
      } else if (this.ccConstants.contains(t))
        // t is a constant
        return null;
    }
    // t is a constant not in ccConstants or a non-function complex term or a
    // variable
    DRule oldRule = (DRule) this.signatureToVertex.get(t);
    if (oldRule == null) {
      Function c = this.createNextConstant();
      oldRule = new DRule(t, c);
      this.constantToTerm.put(c, t);
      this.addVertexToGraph(oldRule);
      this.signatureToVertex.put(t, oldRule);
      if (t instanceof Variable) {
        TreeSet set = new TreeSet();
        set.add(t);
        this.constantToVariables.put(c, set);
      }
    }
    return oldRule;
  }

  /**
   * Same as extendAndSimplifyTerm except that it returns the rhs of
   * the rule and returns the argument if the rule is null.
   */
  protected Function extendAndSimplifyTermToConstant(Term t) {
    DRule tempD;
    return (tempD = this.extendAndSimplifyTerm(t)) == null ?
      (Function) t : tempD.getRHS();
  }

  /**
   * Return the CRule that is at the end of the chain starting at
   * <code>cr</code>.
   */
  // TODO: This needs to propogate ECSwitches as well?
  protected CRule find(CRule cr) {
    Function value = cr.getRHS();
    CRule newCR = (CRule) this.usageTableC.get(value);
    if (newCR == null) {
      return cr;
    } else {
      Function oldValue = cr.getLHS();
      newCR = this.find(newCR);
      this.usageTableC.put(oldValue, newCR);
      return newCR;
    }
  }

  /**
   * Returns the element of ccConstants that represents the equivalence class
   * containing <code>c</code>.
   */
  protected Function findK(Function c) {
    CRule cr = (CRule) this.usageTableC.get(c);
    if (cr != null) {
      Function value = cr.getRHS();
      CRule newCR = (CRule) this.usageTableC.get(value);
      if (newCR == null)
        return value;
      else
        return this.find(newCR).getRHS();
    } else
      return c;
  }

  /**
   * Applies collapse and deduction to the given DRule if possible.
   * Returns true if a collaps was possible on this DRule.
   * @param removed a list of DRules that have been removed from the
   * graph by deduction.  If this method applied deduction, the
   * removed DRule is added to this list.
   */
  protected boolean collapseAndDeduction(DRule d, ArrayList removed) {
    // collapse d with each edge coming out of it from the graph.
    // This will involve rewriting the lhs of d, removing d from the
    // graph and adding a new vertex to the graph that is the result
    // of the rewriting.  This will also involve changing the
    // usageTable?  Or is the usageTable not needed any more?

    // list of CRules that will apply to subterms of the lhs of d.
    ArrayList listOfCRules = (ArrayList) this.graph.get(d);
    final int size;
    if (listOfCRules != null && (size = listOfCRules.size()) > 0) {
      DRule value;
      Term lhs = d.getLHS();
      for (int i = 0; i < size; ++i) {
        CRule cr = (CRule) listOfCRules.get(i);
        // This may be the only place I really need to call find.
        Function newSubterm = this.find(cr).getRHS();
        lhs = lhs.replaceAllConstants(cr.getLHS(), newSubterm);
      }
      this.graph.remove(d);
      this.addVertexToGraph(value = new DRule(lhs, d.getRHS()));
      this.addToSymbolStructure(value);
      // the call to deduction takes care of the signatureToVertex
      // table.
      DRule oldVertex = this.deduction(value);
      if (oldVertex != null)
        removed.add(oldVertex);
      return true;
    } else
      return false;
  }

  /**
   * If possible, apply the deduction rule involving the given DRule,
   * <code>v</code>.  Gets rid of the rule with the "larger" lhs.
   * @return the DRule that was removed from R, if any.
   */
  protected DRule deduction(DRule v) {
    // map signature to DRule
    Term signature = v.getLHS();
    DRule match = (DRule) this.signatureToVertex.get(signature);
    if (match != null) {
      DRule value;
      Function t1 = (Function) v.getRHS();
      Function t2 = (Function) match.getRHS();
      // TODO: I'm not sure I'm doing the right condition here.
      if (this.orientation(t1, t2).getLHS() == t1)
        this.graph.remove(value = v);
      else {
        this.graph.remove(value = match);
        this.signatureToVertex.put(signature, v);
      }
      return value;
    }
    this.signatureToVertex.put(signature, v);
    return null;
  }

  /**
   * Assumes that the deletion from unprocessedEqualities happens elsewhere.  Assumes t1
   * and t2 are in ccConstants.  Adds the new CRule to the usageTable and the
   * graph remedying any clashes.  This also deals with propogating
   * variables.
   * @return the newly created CRule even if it wasn't the one added
   * to the graph.
   */
  protected CRule orientation(Function t1, Function t2) {
    CRule cr;
    if (this.ccConstants.indexOf(t1) < this.ccConstants.indexOf(t2)) {
      cr = new CRule(t1, t2);
      this.propogateVariables(cr);
      if (this.addToUsageTableC(t1, cr) == cr)
        this.addEdgesToGraph(cr);
    } else {
      cr = new CRule(t2, t1);
      this.propogateVariables(cr);
      if (this.addToUsageTableC(t2, cr) == cr)
        this.addEdgesToGraph(cr);
    }
    return cr;
  }

  /**
   * If <code>s</code> is the identity substitution, this returns the
   * receiver.  Otherwise, it adds the pairs in the substitution to a
   * copy of the graph and returns the modified copy.
   */
  protected DSTGraph apply(Substitution s) {
    if (s.isEmpty())
      return this;
    else {
      DSTGraph value = (DSTGraph) this.clone();
      Iterator it = s.iterator();
      while (it.hasNext()) {
        Map.Entry pair = (Map.Entry) it.next();
        Variable v = (Variable) pair.getKey();
        this.boundVars.add(v);
        Term t = (Term) pair.getValue();
        // add new equality to unprocessedEqualities.
        ArrayList newTerms = new ArrayList(2);
        newTerms.add(v);
        newTerms.add(t);
        value.unprocessedEqualities.add(FormulaFactory.getFormula(
                      PredicateConstructor.EQUALITY_SYMBOL,
                      new Object[] {newTerms}));
      }
      value.createCongruenceClosure();
      return value;
    }
  }
  
  /**
   * Returns true if there are no equalities in the graph.  The graph
   * may have lots of rules in it for all the terms that have been
   * interned, but no equalities have been applied to the graph.
   */
  public boolean isEmpty() {
    return this.usageTableC.isEmpty();
  }

  /**
   * Return true if the two terms are equivalent in the receiver's
   * graph.
   */
  public boolean equivalent(Term t1, Term t2) {
    if (this.isEmpty())
      return t1.equals(t2);
    Function c1 = this.findK(this.extendAndSimplifyTermToConstant(t1));
    Function c2 = this.findK(this.extendAndSimplifyTermToConstant(t2));
    return c1 == c2;
  }

  /**
   * Return true if the two terms are equivalent in the receiver's
   * graph.
   */
  public boolean equivalent(Predicate eqn) {
    Iterator args = eqn.arguments();
    Term t1 = (Term) args.next();
    Term t2 = (Term) args.next();
    return this.equivalent(t1, t2);
  }

  /**
   * Returns a substitution under which the disequality is provable in
   * the given theory.  This assumes that the caller has already
   * applied the substitution to the predicate.
   */
  public Substitution unifiable(Predicate eqn, Substitution s) {
    Iterator args = eqn.arguments();
    Term t1 = (Term) args.next();
    Term t2 = (Term) args.next();
    return this.unifiable(t1, t2, s);
  }

  /**
   * Returns a substitution under which the terms are provably equal
   * in the given theory.  This assumes that the caller has already
   * applied the substitution to the predicate.
   */
  public Substitution unifiable(Term t1, Term t2, Substitution s) {
    if (this.isEmpty())
      return null;
    // TODO: first, apply the Substitution to the graph.
    DSTGraph worker = this.apply(s);
    return worker.privateUnifiable(t1, t2, s);
  }

  /**
   * This is called on the congruence closure after the Substitution
   * has been applied to it.
   */
  public Substitution privateUnifiable(Term t1, Term t2, Substitution s) {
    // TODO: what does it mean if one of t1, t2 is a variable, a
    // constant, a non-function complex term, or a function?

    // If one is a variable, then there must be an occurrence problem,
    // right?  So equality can help if the variable is equal to some
    // other term.

    // If one is a constant or a non-function complex term, then I
    // think the other one must be a non-constant function.

    // If one is a non-constant function, then compare its signature
    // to signatures in the graph looking for a pairing of Kconstants
    // representing variables.

    // If congruence closure fails, then I need these rules later.
    DRule r1 = this.extendAndSimplifyTerm(t1);
    DRule r2 = this.extendAndSimplifyTerm(t2);
    assert r1 != null;
    assert r2 != null;
    Function c1 = this.findK(r1.getRHS());
    Function c2 = this.findK(r2.getRHS());
    if (c1 == c2)
      return s;
    else {
      // Check top level
      // Is there a variable in the same class as t1?
      TreeSet l = (TreeSet) this.constantToVariables.get(c1);
      if (l != null) {
        Iterator varsIt = l.iterator();
        while (varsIt.hasNext()) {
          Variable v = (Variable) varsIt.next();
          // if v == t1, let the unifier take care of it.
          if (!v.equals(t1) && !s.containsKey(v))
            if (!t2.contains(v))
              return s.acons(v, t2);
        }
      }
      // Is there a variable in the same class as t2?
      l = (TreeSet) this.constantToVariables.get(c2);
      if (l != null) {
        Iterator varsIt = l.iterator();
        while (varsIt.hasNext()) {
          Variable v = (Variable) varsIt.next();
          // if v == t2, let the unifier take care of it.
          if (!v.equals(t2) && !s.containsKey(v))
            if (!t1.contains(v))
              return s.acons(v, t1);
        }
      }
      // The case where one of them is a variable has already been
      // checked above.  So, now we know that if one of them is a
      // variable, then we should give up.

      if (t1 instanceof Variable || t2 instanceof Variable)
        return null;

      // Now we know that t1 and t2 are complex terms.

      // get signatures for t1 and t2.
      Term sig1 = r1.getLHS();
      Term sig2 = r2.getLHS();

      // See if either of these sigs is in a switchable class, if so,
      // see if the switch helps.

      Function rep1 = this.findK(r1.getRHS());
      Function rep2 = this.findK(r2.getRHS());

      Substitution temp = this.alreadySwitchable(rep1, rep2);
      if (temp != null) {
        return Substitution.compatible(s, temp);
      }
      temp = this.tryToSwitch((Function) sig1, rep2, s);
      if (temp == null)
        return this.tryToSwitch((Function) sig2, rep1, s);
      else
        return temp;
    }
  }

  /**
   * Returns the composition of <code>s</code> with a substitution
   * that allows sig to switch to otherRep's equivalence class.
   * Otherwise, null.
   */
  private Substitution tryToSwitch(Function sig, Function otherRep,
                                   Substitution s) {
    // Go through the args of sig.
    List sigArgs = ((Function) sig).getArguments();
    // j is the index of arg in arguments of sig.
    for (int j = 0; j < sigArgs.size(); ++j) {
      // arg is a KConstant whose switch may let rep switch.
      Function arg = (Function) sigArgs.get(j);
      // if arg is in a switchable class
      ArrayList switches = (ArrayList) this.constantToECSwitches.get(arg);
      if (switches != null) {
        for (int i = 0; i < switches.size(); ++i) {
          // then see if a switch helps by constructing a signature
          // involving the switched class.
          ECSwitch sw = (ECSwitch) switches.get(i);
          Function switched = sw.getRepresentative1();
          if (switched == arg)
            switched = sw.getRepresentative2();
          ArrayList newArgs = new ArrayList(sigArgs);
          newArgs.set(j, switched);
          Function newSig =
            (Function)
            TermFactory.getTerm(((ComplexTerm) sig).getConstructor(),
                                new Object[] {newArgs});
          DRule hope = (DRule) this.signatureToVertex.get(newSig);
          if (hope != null) {
            Function hopeClass = hope.getRHS();
            // TODO: I need to apply the Substitution to the graph
            // before testing the following equality.
            Substitution temp =
              Substitution.compatible(s, sw.getSubstitution());
            if (temp != null) {
              DSTGraph worker = this.apply(sw.getSubstitution());
              if (worker.findK(hopeClass) == worker.findK(otherRep))
                return temp;
            }
          }
        }
        // TODO: I might consider trying different combinations of
        // these.
      }
    }
    return null;
  }

  /**
   * Adds the given equality to the list of equalities and then
   * incorporates it into the graph.
   */
  public void addNewEquation(Predicate f) {
    this.unprocessedEqualities.add(f);
    this.createCongruenceClosure();
  }

  public boolean add(KBSequent seq) {
    if (seq.isEquality()) {
      Predicate eqn = (Predicate) seq.getNegatives().get(0);
      this.unprocessedEqualities.add(eqn);
      this.createCongruenceClosure();
      return true;
    } else
      return false;
  }

  /**
   * Adds the given equalities to the list of equalities and then
   * incorporates them into the graph.
   * @param eqs may contain either Predicates or TableauFormulas but
   * not both.
   */
  public void addAll(Collection eqs) {
    Iterator eqsIt = eqs.iterator();
    if (eqsIt.hasNext()) {
      Object item = eqsIt.next();
      if (item instanceof TableauFormula) {
        this.unprocessedEqualities.add(((TableauFormula) item).getFormula());
        this.tableauFormulas.add(item);
        while (eqsIt.hasNext()) {
          this.unprocessedEqualities.add(((TableauFormula) eqsIt.next()).getFormula());
          this.tableauFormulas.add(item);
        }
      } else
        this.unprocessedEqualities.addAll(eqs);
    }
    this.createCongruenceClosure();
  }

  /**
   * Take the formulas from this.unprocessedEqualities and bring them into the graph.
   */
  private void createCongruenceClosure() {
    this.extendSimplifyAndTransform();
    Set keySet = this.graph.keySet();
    // As long as collapses are happenning, continue
    while (true) {
      boolean collapseOccurred = false;
      if (!keySet.isEmpty()) {
        Iterator origKeys = keySet.iterator();
        final int size = keySet.size();
        // clone the keySet;
        ArrayList oldKeys = new ArrayList(size);
        while (origKeys.hasNext()) {
          oldKeys.add(origKeys.next());
        }
        origKeys = oldKeys.iterator();
        ArrayList removed = new ArrayList();
        while (origKeys.hasNext()) {
          DRule vertex = (DRule) origKeys.next();
          if (!removed.contains(vertex)) {
            // since this cannot create any new CRules, it's ok to go
            // through this list just once.
            if (this.collapseAndDeduction(vertex, removed))
              collapseOccurred = true;
          }
        }
      }
      if (!collapseOccurred)
        // no collapse was possible
        break;
      // At this point, unprocessedEqualities, contains only equations of the form c1=c2
      // where c1, c2 are in ccConstants.
      if (!this.unprocessedEqualities.isEmpty()) {
        int index = this.unprocessedEqualities.size() - 1;
        do {
          // TODO: Simplify may be needed here.
          Predicate eqn = (Predicate) this.unprocessedEqualities.get(index);
          // at this point, eqn should be an equation between elements
          // of ccConstants.
          Iterator terms = eqn.arguments();
          Function t1 = this.findK((Function) terms.next());
          Function t2 = this.findK((Function) terms.next());
          this.unprocessedEqualities.remove(index--);
          if (t1 != t2)
            this.orientation(t1, t2);
        } while (index >= 0);
      }
    
    }
  }

  /**
   * A D-Rule is a rewrite rule that takes a term in T(SUK)\ccConstants to some
   * element of ccConstants.  It also labels a vertex in a DSTGraph.
   */
  private static class DRule {

    /**
     * Term that is rewriten by this rule.  This must not be an
     * element of ccConstants.
     */
    private Term lhs;
    /**
     * Term that is produced by this rule.  This must be an element of
     * ccConstants.
     */
    private Function rhs;

    /**
     * Creates a DRule from the given left- and right-hand sides.
     */
    public DRule(Term lhs, Function rhs) {
      this.lhs = lhs;
      this.rhs = rhs;
    } // DRule constructor
  
    public String toString() {
      return this.lhs + " -> " + this.rhs;
    }

    /**
     * Only checks reference equality.
     */
    public final boolean equals(Object o) {
      return this == o;
    }

    /**
     * Returns the term that is rewriten by this rule.  This must not
     * be an element of ccConstants.
     */
    public Term getLHS() {
      return this.lhs;
    }

    /**
     * Returns the term that is produced by this rule.  This must be
     * an element of ccConstants.
     */
    public Function getRHS() {
      return this.rhs;
    }

    public int hashCode() {
      // TODO: think about improving this.
      return this.lhs.hashCode() + this.rhs.hashCode();
    }

    // I think I want DRules to be equal only if they are the same
    // reference.
//     public boolean equals(Object o) {
//       if (o instanceof DRule) {
//         DRule arg = (DRule) o;
//         return this.rhs.equals(arg.rhs) && this.lhs.equals(arg.lhs);
//       }
//       return false;
//     }

  } // DRule

  /**
  * A C-Rule is a rewrite rule that takes a term in ccConstants to some element
  * of ccConstants.  It also labels an edge in a DSTGraph.
  */

  private static class CRule {

    private Function lhs;
    private Function rhs;

    public CRule(Function lhs, Function rhs) {
      this.lhs = lhs;
      this.rhs = rhs;
    } // CRule constructor
  
    /**
     * Returns the term that is produced by this rule.  This must be
     * an element of ccConstants.
     */
    public Function getRHS() {
      return this.rhs;
    }

    /**
     * Returns the term that is rewriten by this rule.  This must be
     * an element of ccConstants.
     */
    public Function getLHS() {
      return this.lhs;
    }

    /**
     * Only checks reference equality.
     */
    public final boolean equals(Object o) {
      return this == o;
    }

    public int hashCode() {
      // TODO: think about improving this.
      return this.lhs.hashCode() + this.rhs.hashCode();
    }

//     public boolean equals(Object o) {
//       if (o instanceof CRule) {
//         CRule arg = (CRule) o;
//         return this.rhs.equals(arg.rhs) && this.lhs.equals(arg.lhs);
//       }
//       return false;
//     }

  } // CRule

} // DSTGraph

