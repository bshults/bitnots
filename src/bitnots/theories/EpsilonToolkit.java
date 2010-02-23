package bitnots.theories;

import bitnots.equality.*;
import bitnots.expressions.*;
import bitnots.tableaux.*;
import bitnots.util.Operator;
import java.util.*;

/**
 * This class is responsible for the application of
 * the Shults epsilon rule. Its methods search the
 * tableau for a branch which is a likely candidate
 * to the rule. From there the knowledge base is
 * examined and matched to the unused formulas in
 * the selected branch. The best match is then
 * applied: if all the formulas from a sequent are
 * used, then the branch is closed; if only one
 * sequent formula is left over, it is added to the
 * tableau; and if more than one sequent formula
 * is left over, the necessary leaves are split and
 * the left over formulas are added to each new branch.
 *
 * @author Daniel W. Farmer
 * @version 1.1
 */
// TODO consider trying harder to match older tableau formulas.
// TODO consider trying harder to match tableau formulas that have
// not been matched much by theorems.
public class EpsilonToolkit {

  /**
   * A map of KBSequents to maps of sequent formulas to InitialMatches.
   * @todo can I get rid of this?
   */
  private HashMap<KBSequent, Map<Formula, List<InitialMatch>>> sequentMap =
                                                               new HashMap<KBSequent, Map<Formula, List<InitialMatch>>>();
  private IdentityHashMap<TheoremApplication, String> applied =
                                                      new IdentityHashMap<TheoremApplication, String>();
  private Tableau tableau;
  /**
   * The best-to-date TheoremApplication used in the
   * createTheoremApplications of scanning a branch.
   */
  private TheoremApplication bestTA;

  public static EpsilonToolkit createEpsilonToolkitForTableau(Tableau t) {
    return new EpsilonToolkit(t);
  }

  private EpsilonToolkit(Tableau t) {
    this.tableau = t;
    t.setEpsilonToolkit(this);
  }

  /**
   * Select the most needy leaf and apply the best TheoremApplication to it.
   * @param leafToTAs a map of leaf nodes to the list of TheoremApplications
   * that apply on that branch.  Such a thing is created by createAllTheoremApps().
   * @return true if a theorem is applied.
   */
  private boolean applyBestTheoremApp(
      Map<TableauNode, List<TheoremApplication>> leafToTAs) {
    // create an ordered set of branches, from most to least needy
    List<TableauNode> branches = this.tableau.undoneLeaves();
    Collections.sort(branches, new BranchComparator());

    for (TableauNode leaf : branches) {
      // see whether a good theorem applies here.
      // prefer TAs that are higher (split-wise)
      List<TheoremApplication> tas = leafToTAs.get(leaf);
      if (tas != null) {
        Collections.sort(tas, new TheoremAppComparator());
        for (TheoremApplication ta : tas) {
          if (!this.hasBeenApplied(ta)) {
            if (this.doTA(ta))
              return true;
          }
        }
      }
    }
    return false;
  }

  private boolean hasBeenApplied(TheoremApplication ta) {
    return this.applied.get(ta) != null;
  }

  /**
   * Create all InitialMatches and TheoremApplications on the given branch.
   * @param leaf the branch on which InitialMatches and TheoremApplications
   * will be created.
   * @return the list of all TheoremApplications that apply to the branch.
   */
  private List<TheoremApplication> createTheoremAppsOnBranch(TableauNode leaf) {
    Stack<TableauNode> branchNodes = new Stack<TableauNode>();
    TableauNode current = leaf;
    do {
      branchNodes.push(current);
      current = current.getParent();
    } while (current != null);
    List<TheoremApplication> allTAs = new ArrayList<TheoremApplication>();
    List<TheoremApplication> newTAs = new ArrayList<TheoremApplication>();
    do {
      current = branchNodes.pop();
      Collection<TheoremApplication> addedNewTAs = null;
      if (current.hasBeenMatchedWithTheory()) {
        // combine the IMs here with the new TAs from above.
        addedNewTAs = EpsilonToolkit.createTheoremApplications(current, newTAs);
      } else {
        // create new InitialMatches.
        this.findInitialMatches(current, current.getTableau().getTheory());
        // combine those with ALL the TAs from above.
        addedNewTAs = EpsilonToolkit.createTheoremApplications(current, allTAs);
      }
      if (addedNewTAs != null && !addedNewTAs.isEmpty()) {
        allTAs.addAll(addedNewTAs);
        newTAs.addAll(addedNewTAs);
      }
    } while (!branchNodes.isEmpty());
    return allTAs;
  }

  private static class TheoremAppComparator implements Comparator {

    public int compare(Object o1, Object o2) {
      TheoremApplication ta1 = (TheoremApplication) o1;
      TheoremApplication ta2 = (TheoremApplication) o2;
      if (ta1.rank > ta2.rank)
        // I want the largest to come first
        return -1;
      else if (ta1.rank == ta2.rank)
        return 0;
      else
        return 1;
    }
  }

  /**
   * This method does the work of trying to apply a sequent from the knowledge
   * base to (unused?) formulas in the tableau.
   * @param tableau The Tableau to be looked through
   * @return true if an epsilon was applied
   */
  @SuppressWarnings("empty-statement")
  public boolean applyOneEpsilon() {
    Map<TableauNode, List<TheoremApplication>> leafToTAs =
                                               createAllTheoremApps(this.tableau.
        getRoot());
    if (!leafToTAs.isEmpty())
      return this.applyBestTheoremApp(leafToTAs);
    return false;
  }

  /**
   * Apply an epsilon rule to ln(#of branches).
   */
  public boolean applySomeEpsilons() {
    boolean retVal = false;
    List<TableauNode> branches = this.tableau.undoneLeaves();
    Collections.sort(branches, new BranchComparator());

    int i = 0, a = (int) Math.ceil(Math.log(branches.size()));
    for (TableauNode leaf : branches) {
      // Create IMs and TAs on this branch.
      List<TheoremApplication> tas = this.createTheoremAppsOnBranch(leaf);
      // Apply the best one.
      if (tas != null) {
        Collections.sort(tas, new TheoremAppComparator());
        for (TheoremApplication ta : tas) {
          if (!this.hasBeenApplied(ta)) {
            if (this.doTA(ta)) {
              // only increment i if a theorem is applied
              retVal = true;
              i++;
              break;
            }
          }
        }
      }
      if (i >= a)
        break;
    }
    return retVal;
  }

  /**
   * Create InitialMatches on all nodes in the tree rooted at <code>tn</code>
   * where they don't exist and create
   * TheoremApplications for all new InitialMatches.
   * @todo need to store new TAs at the highest node containing all IMs.
   * @todo have this return a map of leaves to TA lists.
   */
  public Map<TableauNode, List<TheoremApplication>> createAllTheoremApps(
      TableauNode tn) {
    Stack<BranchData> s = new Stack<BranchData>();
    s.push(new BranchData(tn));
    Map<TableauNode, List<TheoremApplication>> retVal =
                                               new IdentityHashMap<TableauNode, List<TheoremApplication>>();
    do {
      BranchData bd = s.pop();
      Collection<TheoremApplication> addedNewTAs = null;
      TableauNode current = bd.tn;
      if (current.hasBeenMatchedWithTheory()) {
        // combine the IMs here with the new TAs from above.
        addedNewTAs = EpsilonToolkit.createTheoremApplications(current,
                                                               bd.newTAs);
      } else {
        // create new InitialMatches.
        this.findInitialMatches(current, current.getTableau().getTheory());
        // combine those with ALL the TAs from above.
        addedNewTAs = EpsilonToolkit.createTheoremApplications(current,
                                                               bd.allTAs);
      }
      if (current.getChildCount() != 0) {
        Collection<TheoremApplication> newAllTAs = null;
        Collection<TheoremApplication> newNewTAs = null;
        if (addedNewTAs != null && !addedNewTAs.isEmpty()) {
          newAllTAs = new ArrayList<TheoremApplication>(bd.allTAs);
          newNewTAs = new ArrayList<TheoremApplication>(bd.newTAs);
          newAllTAs.addAll(addedNewTAs);
          newNewTAs.addAll(addedNewTAs);
        }
        for (TableauNode node : current.getChildren()) {
          // add TAs from here to the list of all TAs.
          s.push(new BranchData(node, newNewTAs == null ? bd.newTAs : newNewTAs,
                                newAllTAs == null ? bd.allTAs : newAllTAs));
        }
      } else {
        // current is a leaf
        List<TheoremApplication> value = null;
        if (!bd.allTAs.isEmpty() || addedNewTAs != null) {
          value = new ArrayList<TheoremApplication>(bd.allTAs);
          if (addedNewTAs != null)
            value.addAll(addedNewTAs);
          retVal.put(current, value);
        }
      }
    } while (!s.isEmpty());
    return retVal;
  }

  /**
   * Create new TheoremApplications between the InitialMatches at
   * <code>current</code> and the TheoremApplications in <code>taList</code> and
   * add them to <code>current's</code> theoremApplications list and return them.
   * @param current the TableauNode whose InitialMatches are to be combined
   * with the TheoremApplications on <code>taList</code>.
   * @param taList the list of TheoremApplications that need to be combined
   * with the InitialMatches at <code>current</code>.
   * @return the list of new TAs created by this createTheoremApplications or
   * null if none are created.
   */
  private static Collection<TheoremApplication> createTheoremApplications(
      TableauNode current,
      Collection<TheoremApplication> taList) {
    if (!current.getInitialMatches().isEmpty()) {
      Collection<TheoremApplication> retVal =
                                     new ArrayList<TheoremApplication>();
      for (InitialMatch match : current.getInitialMatches()) {
        // first create a TA for match
        retVal.add(new TheoremApplication(match));
        // then combine match with existing TAs
        for (TheoremApplication ta : taList) {
          Substitution combo = Substitution.compatible(match.getSubstitution(),
                                                       ta.getSubstitution());
          if (combo != null) {
            TheoremApplication newTA = ta.combine(match, combo);
            retVal.add(newTA);
          }
        }
      }
      return retVal;
    }
    return null;
  }

  /**
   * Goes through all sequent and node formulas looking for
   * matches and creates InitialMatches for each match
   * which are stored at the TableauNode at which they occurred.
   * This is done so that the larger TheoremApplications can
   * keep references to them and thus make sure the same theorem
   * isn't applied twice in the same way. After a call to this
   * method, private field sequentMap is up to date for
   * the specified sequent.
   *
   * @param tn The node representing the branch being looked at
   * @param sequents The list of sequents from the theory
   */
  private void findInitialMatches(TableauNode tn, Theory theory) {

    // Go through these trying to match with any and every
    // sequent.
    tn.setBeenMatchedWithTheory(true);

    Collection<KBSequent> sequents = theory.getKB();
    for (TableauFormula branchFormula : tn.getNewHypsUnusedAnywhere()) {
      for (KBSequent currentSeq : sequents) {
        this.match((List) currentSeq.getPositives(), currentSeq,
                   branchFormula);
      }
    }
    for (TableauFormula branchFormula : tn.getNewGoalsUnusedAnywhere()) {
      for (KBSequent currentSeq : sequents) {
        this.match((List) currentSeq.getNegatives(), currentSeq,
                   branchFormula);
      }
    }
  }

  /**
   * This method looks through the sequent formulas it is
   * given, and attempts to match them with the branch
   * formula it is given. In the event of a successful
   * match, all relevant lists and maps are updated.
   *
   * @param sequentFormulas the list of sequent formulas to match
   * @param currentSeq the sequent the sequent formulas belong to
   * @param tableauFormula the branch formula to match
   */
  private void match(List<Formula> sequentFormulas, KBSequent currentSeq,
                     TableauFormula tableauFormula) {
    // get the closest congruence closure
    DSTGraph dstG = tableauFormula.getBirthPlace().getCongruenceClosure();

    for (Formula sequentFormula : sequentFormulas) {
      // try to unify the sequent formula with the branch formula
      Substitution sub;

      if (dstG != null) {
        sub = tableauFormula.getFormula().
            unifyApplied(sequentFormula, tableauFormula.
            getSubstitutionDependency(), dstG);
      } else {
        sub = tableauFormula.getFormula().
            unifyApplied(sequentFormula, tableauFormula.
            getSubstitutionDependency());
      }

      if (sub != null) { // the unification was successful
        // Get the set of KBSequents sequentFormula appears in
        Iterator setIt = ((Set) tableauFormula.getBirthPlace().getTableau().
            getTheory().getFormulaMap().get(sequentFormula)).iterator();
        // TODO: doesn't this create more IMs than it should?
        // for each sequent that the sequent formula appears in,
        // create a new InitialMatch object
        while (setIt.hasNext()) {
          InitialMatch im =
                       new InitialMatch(sub, tableauFormula, sequentFormula, (KBSequent) setIt.
              next());
          tableauFormula.getBirthPlace().addInitialMatch(im);

          // take care of map-related stuff
          HashMap sfsToIMs = (HashMap) this.sequentMap.get(currentSeq);
          if (sfsToIMs == null) {
            sfsToIMs = new HashMap();
            this.sequentMap.put(currentSeq, sfsToIMs);
          }
          List imList = (List) sfsToIMs.get(sequentFormula);
          if (imList == null) {
            imList = new ArrayList();
            sfsToIMs.put(sequentFormula, imList);
          }
          imList.add(im);
          sfsToIMs.put(im.getSequentFormula(), imList);
        }
      }
    }
  }

  /**
   * This recursive method extends a TheoremApplication as much as
   * possible by trying to combine its substitution with the
   * substitutions from InitialMatches from the same sequent.
   *
   * @param ta the TheoremApplication to extend
   * @param entries a List of pairs mapping sequent formulas to lists
   * of initial matches.
  public final void extend(TheoremApplication ta,
  List<Map.Entry<Formula, List<InitialMatch>>> entries) {

  Substitution taSub = ta.getSubstitution();
  Substitution currentSub = taSub;

  int j = 0;
  for (Map.Entry<Formula, List<InitialMatch>> pair : entries) {
  //    while (index < entries.size()) {

  //      Map.Entry<Formula, List<InitialMatch>> pair = entries.get(index);
  Formula sf = pair.getKey();
  j++;
  assert !ta.contains(sf);

  // list of initial matches using sequentFormula.
  for (InitialMatch im : pair.getValue()) {
  Substitution ims = im.getSubstitution();

  // check to see if the InitialMatch is compatible
  // with the current TheoremApplication
  currentSub = Substitution.compatible(ims, currentSub);

  if (currentSub != null) {  // compatible!
  TheoremApplication newTA = ta.combine(im, currentSub);
  List branchTAs;
  if ((branchTAs = newTA.getLowestFormula().getBirthPlace().
  getTheoremApplications()) != null
  && branchTAs.contains(newTA)) {
  return; // this theorem has already been applied
  } else if (this.bestTA == null) {
  this.bestTA = newTA;
  } else if (this.bestTA.compareTo(newTA) < 0) {
  this.bestTA = newTA;
  }
  this.extend(newTA, entries.subList(j, entries.size()));
  } else  // not compatible!
  currentSub = taSub;  // reset the currentSub
  }
  }
  }
   */
  /**
   * Applies a <code>TheoremApplication</code>
   * @param ta The TheoremApplication to apply
   * return true if it was applied.
   */
  private boolean doTA(final TheoremApplication ta) {

    // get the lowest involved node
    final TableauNode node = ta.getLowestFormula().getBirthPlace();

    // tell the TA that it has been applied.
    this.applied.put(ta, "");
    node.addTheoremApplication(ta);

    // get the sequent's unused formulas
    final List<Formula> unusedPositives =
                        ta.getUnusedPositiveSeqFormulas();
    final List<Formula> unusedNegatives =
                        ta.getUnusedNegativeSeqFormulas();

    int needs = unusedPositives.size() + unusedNegatives.size();

    // get the substitution
    final Substitution sub = ta.getSubstitution();
    // maps sequent variables to tableau variables.
    // XXX: this varMap doesn't seem to be what it should be!  Shouldn't it
    // map sequent variables to tableau variables?
    final HashMap<Variable, Variable> varMap = new HashMap();
    ta.setVarMap(varMap);
    final Substitution tabSub = sub.removeSequentVars(varMap);
    ta.tabSubst = tabSub;
    final List matchedBranchFormulas = ta.getBranchFormulas();
    final TreeSet splits = (TreeSet) ta.getSplits();

    // apply the epsilon rule according to
    // this TheoremApplication's needs
    switch (needs) {
      case 0:
        // branch is closed - post a branch closer
        node.createBranchCloser(tabSub,
                                ta.getPositiveFormulas(),
                                ta.getNegativeFormulas(),
                                ta.getSequent());
        break;
      case 1:
        // get the unused formula and create a new TableauFormula
        Formula unusedSequentFormula;
        TableauFormula newTF;
        if (unusedPositives.size() != 0) {
          // TODO: make this a method because it is duplicated.

          // the formula is positive so create a
          // negative TableauFormula
          unusedSequentFormula = unusedPositives.get(0);
          Formula f =
                  unusedSequentFormula.apply(sub).replaceVariables(varMap);
          newTF = TableauFormula.createTableauFormula(f, false, splits,
                                                      matchedBranchFormulas,
                                                      tabSub);
        } else {
          // the formula is negative so create a
          // positive TableauFormula
          unusedSequentFormula = unusedNegatives.get(0);
          Formula f =
                  unusedSequentFormula.apply(sub).replaceVariables(varMap);
          newTF = TableauFormula.createTableauFormula(f, true, splits,
                                                      matchedBranchFormulas,
                                                      tabSub);
        }
        // splice new child node into its place.
        TableauNode child = node.spliceUnder(Collections.singletonList(newTF));
        // TODO consider adding some formulas to the usedAt field
        if (child.enforceRegularity()) {
          // if the above returns true, we need to clean up the TheoremApplication, etc.
          newTF.unregisterWithTableau(node.getTableau());
          node.removeTheoremApplication(ta);
          return false;
        }
        break;
      default:
        // multiple branches will be added.

        // for every branch that passes through the lowest node
        // involved with this theorem application, give the leaf a new
        // child for each need.

        node.applyToLeaves(new Operator() {

          public void exec(Object o) {
            // for each leaf
            TableauNode leaf = (TableauNode) o;

            // update splits
            TreeSet<TableauNode.Split> mySplits = (TreeSet) splits.clone();
            mySplits.add(new TableauNode.Split(leaf));

            // create the child formulas and put them into nodes
            ArrayList<TableauNode> childNodes = new ArrayList<TableauNode>();

            TableauFormula theLemma = null;
            Formula lemmaFood = EpsilonToolkit.this.findLemmaCandidate(
                unusedPositives);
            if (lemmaFood == null) {
              lemmaFood =
              EpsilonToolkit.this.findLemmaCandidate(unusedNegatives);
              if (lemmaFood != null) {
                theLemma = TableauFormula.createTableauFormula(
                    lemmaFood.apply(sub).replaceVariables(varMap),
                    true, mySplits, matchedBranchFormulas, tabSub);
              }
            } else {
              theLemma = TableauFormula.createTableauFormula(
                  lemmaFood.apply(sub).replaceVariables(varMap),
                  false, mySplits, matchedBranchFormulas, tabSub);
            }

            EpsilonToolkit.this.makeNodes(childNodes, unusedPositives,
                                          false, node, /*leaf,*/ sub, tabSub,
                                          mySplits, matchedBranchFormulas,
                                          varMap, lemmaFood, theLemma);
            EpsilonToolkit.this.makeNodes(childNodes, unusedNegatives,
                                          true, node, /*leaf,*/ sub, tabSub,
                                          mySplits, matchedBranchFormulas,
                                          varMap, lemmaFood, theLemma);
            // associate the leaf with its new child nodes
            leaf.setChildren(childNodes);
          }
        });
      /*
      node.applyToLeaves(new Operator() {
      public void exec(Object o) {
      // TODO enforcing regularity on splitting rules is more complicated
      // than my code currently handles.  For the sake of soundness,
      // I need to leave some duplicates until I find a way to fix this.
      // enforce regularity for each leaf
      //          ((TableauNode) o).enforceRegularity();
      }
      });
       */
    }
    return true;
  }

  /**
   * This method creates nodes and gives them the appropriate
   * formulas in an epsilon rule application that requires branch
   * splitting.
   * @param childNodes The list of nodes into which the new node will be inserted.
   * @param unused Unused sequent formulas to be turned into formulas
   * in the tableau
   * @param pos The sign of the formulas to be created
   * @param lowNode The lowest node from the epsilon rule application
   * @param leaf The node representing the current branch
   * @param sub The substitution that enabled the match from this
   * epsilon rule application
   * @param tabSub This is <code>sub</code> with its sequent variables
   * translated to tableau variables.
   * @param splits The set of splits involved in the creation of the
   * new formulas
   * @param parents matched formulas from the node
   * @param varMap This is the map of sequent variables to tableau
   * variables.
   * @param lemmaFood The formula that was used to create theLemma.
   * @param theLemma The lemma that needs to be inserted into every
   * branch, except the one it came from.
   */
  private void makeNodes(List<TableauNode> childNodes,
                         List<Formula> unused, boolean pos,
                         TableauNode lowNode,// TableauNode leaf,
                         Substitution sub, Substitution tabSub,
                         Set<TableauNode.Split> splits,
                         List<TableauFormula> parents,
                         HashMap<Variable, Variable> varMap,
                         Formula lemmaFood, TableauFormula theLemma) {
    // we create a new node for each formula in unused an insert that node
    // into childNodes.
    for (Formula seqForm : unused) {
      Formula nf = seqForm.apply(sub).replaceVariables(varMap);
      TableauFormula newTF =
                     TableauFormula.createTableauFormula(nf, pos, splits,
                                                         parents, tabSub);
      ArrayList<TableauFormula> forms = new ArrayList<TableauFormula>(2);
      forms.add(newTF);
      // newTF.insertInto(newNode);
      if (lemmaFood != null && lemmaFood != seqForm)
        //theLemma.insertInto(newNode);
        forms.add(theLemma);
      TableauNode newNode = new TableauNode(lowNode.getTableau(), forms);
      childNodes.add(newNode);
    }
  }

  /**
   * Finds a good lemma candidate from the given Collection, which
   * will be the first equality predicate formula it finds.
   * @param unused the Collection of unusued formulas that may contain
   * an equality predicate to be turned into a lemma.
   */
  private Formula findLemmaCandidate(Collection<Formula> unused) {
    for (Formula form : unused) {
      if (form instanceof Predicate
          && form.getConstructor().getName().equals("="))
        return form;
    }
    return null;
  }

  /**
   * Handles data for the recursive nature of the pass-one algorithm.
   * @author bshults
   *
   */
  private static class BranchData {

    TableauNode tn;
    Collection<TheoremApplication> newTAs;
    Collection<TheoremApplication> allTAs;

    @SuppressWarnings("unchecked")
    BranchData(TableauNode tn) {
      this(tn, Collections.EMPTY_SET,
           Collections.EMPTY_SET);
    }

    BranchData(TableauNode tn,
               Collection<TheoremApplication> newTAs,
               Collection<TheoremApplication> allTAs) {
      this.tn = tn;
      this.newTAs = newTAs;
      this.allTAs = allTAs;
    }
  }
}
