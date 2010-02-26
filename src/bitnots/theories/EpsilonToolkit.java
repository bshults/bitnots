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
            if (this.applyTheoremApplication(ta))
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

  private void markApplied(TheoremApplication ta) {
    this.applied.put(ta, "");
  }

  /**
   * Subracts from <code>ta</code>'s rank a value depending on how many
   * TheoremApplications similar to <code>ta</code> have already been applied.
   * @param ta the TheoremApplication to be compared with those that have
   * already been applied.
   * @return the adjusted ranking.
   */
  private double getSimilarityRanking(TheoremApplication ta) {
    double retVal = ta.rank;
    for (TheoremApplication appliedTA : this.applied.keySet()) {
      if (ta.getSequent() == appliedTA.getSequent())
        if (ta.getSequentFormulas().equals(appliedTA.getSequentFormulas())) {
          retVal -= 5;
        }
    }
    return retVal;
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
    do {
      current = branchNodes.pop();
      Collection<TheoremApplication> addedNewTAs = null;
      if (current.hasBeenMatchedWithTheory()) {
        // combine the IMs here with the new TAs from above.
        addedNewTAs = EpsilonToolkit.createTheoremApplications(current, allTAs);
      } else {
        // create new InitialMatches.
        this.findInitialMatches(current, current.getTableau().getTheory());
        // combine those with ALL the TAs from above.
        addedNewTAs = EpsilonToolkit.createTheoremApplications(current, allTAs);
      }
      if (addedNewTAs != null) {
        allTAs.addAll(addedNewTAs);
      }
    } while (!branchNodes.isEmpty());
    return allTAs;
  }

  /**
   * This is a member class because it needs to see whether simlar theorems
   * have been applied.
   */
  private class TheoremAppComparator implements Comparator {

    // XXX: this needs to consider whether similar ones have been applied.
    public int compare(Object o1, Object o2) {
      TheoremApplication ta1 = (TheoremApplication) o1;
      TheoremApplication ta2 = (TheoremApplication) o2;
      // check for similar TheoremApplications that have already been applied
      // and subtract for each similar application.
      double ta1Rank = EpsilonToolkit.this.getSimilarityRanking(ta1);
      double ta2Rank = EpsilonToolkit.this.getSimilarityRanking(ta2);
      if (ta1Rank > ta2Rank)
        // I want the largest to come first
        return -1;
      else if (ta1Rank == ta2Rank)
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
  @SuppressWarnings("empty-statement")
  public boolean applyOneEpsilon() {
  Map<TableauNode, List<TheoremApplication>> leafToTAs =
  createAllTheoremApps(this.tableau.
  getRoot());
  if (!leafToTAs.isEmpty())
  return this.applyBestTheoremApp(leafToTAs);
  return false;
  }
   */
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
            if (this.applyTheoremApplication(ta)) {
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
      TableauNode current, Collection<TheoremApplication> taList) {
    if (!current.getInitialMatches().isEmpty()) {
      Collection<TheoremApplication> retVal =
                                     new ArrayList<TheoremApplication>();
      for (InitialMatch match : current.getInitialMatches()) {
        // combine match with TAs from previous matches from this same node.
        retVal.addAll(combineMatchWithTAs(match, retVal));
        // combine match with TAs from nodes up above
        retVal.addAll(combineMatchWithTAs(match, taList));
        // create a TA for match
        retVal.add(new TheoremApplication(match));
      }
      return retVal;
    }
    return null;
  }

  /**
   * Attempts to combine <code>match</code> with each TheoremApplication in
   * taList.
   * @param match
   * @param taList
   * @return the combinations.
   */
  private static Collection<TheoremApplication> combineMatchWithTAs(InitialMatch match,
                                                                    Collection<TheoremApplication> taList) {
    ArrayList<TheoremApplication> retVal = new ArrayList<TheoremApplication>();
    for (TheoremApplication ta : taList) {
      TheoremApplication newTA = ta.combine(match);
      if (newTA != null)
        retVal.add(newTA);
    }
    return retVal;
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
        InitialMatch im =
                     new InitialMatch(sub, tableauFormula, sequentFormula,
                                      currentSeq);
        // add it to the node
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

  private boolean doTA(TheoremApplication ta, final TableauNode node) {
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
   * Applies a <code>TheoremApplication</code> to the highest node where it
   * applies and registers the TheoremApplication as applied.
   * @param ta The TheoremApplication to apply return true if it was applied.
   * @return true if the TheoremApplication is applied.
   */
  private boolean applyTheoremApplication(final TheoremApplication ta) {
    TableauNode node = ta.getLowestFormula().getBirthPlace();
    if (this.doTA(ta, node)) {
      // tell the TA that it has been applied.
      this.markApplied(ta);
      node.addTheoremApplication(ta);
      return true;
    }
    return false;
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
