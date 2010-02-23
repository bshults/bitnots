package bitnots.theories;

import bitnots.tableaux.*;
import bitnots.expressions.*;
import java.util.*;

/**
 * This class is used to remember the way in which
 * a sequent can be applied to a tableau.
 * It contains references to the tableau
 * formulas that matched sequent formulas.
 *
 * @author <a href="mailto:b-shults@bethel.edu">Benjamin Shults</a>
 * @author Daniel W. Farmer
 * @version 1.2
 */
public class TheoremApplication implements Comparable {

  /**
   * The lowest formula of the branch this
   * TheoremApplication can be performed on.
   */
  private TableauFormula lowestFormula;

  /**
   * The map that was used to translate sequent variables to tableau
   * variables when this theorem was applied.
   */
  private HashMap varMap = null;

  /** The sequent that is applied on this branch. */
  private KBSequent sequent;

  /** The substitution which allows this sequent to be applied.  This
   * substitution will contain sequent variables.
   */
  Substitution subst;

  /** The substitution which allows this sequent to be applied.  This
   * substitution will not contain sequent variables and those pairs
   * that were replacing sequent variables won't be in here.  I.e.,
   * this is the substitution that the tableau needs to know about.
   */
  Substitution tabSubst;

  /** The InitialMatches used in the creation of this TA */
  private List<InitialMatch> initialMatches;

  /** The splits involved in this TheoremApplication */
  private TreeSet splits;

  /** Used to rank TAs in relation to each other.
   * Each TA's rank is initialized based on a
   * variety of factors.
   */
  double rank;

  /**
   * Creates a new TheoremApplication which associates a
   * sequent with a branch of the tableau. It is
   * a possible application of the epsilon rule.
   * @param im the InitialMatch from which this
   * TheoremApplication is being extended
   */
  public TheoremApplication(InitialMatch im) {

    this.sequent = im.getSequent();
    this.subst = im.getSubstitution();
    this.initialMatches = Collections.singletonList(im);
    this.splits = new TreeSet();
    this.splits.addAll(im.getBranchFormula().getInvolvedSplits());
    this.rank = this.calculateRank();
    this.lowestFormula = im.getBranchFormula();
  }

  /**
   * Creates a new TheoremApplication which associates a
   * sequent with a specific branch of the tableau. It is
   * a possible application of the epsilon rule.
   *
   * @param lowest The TableauFormula which specifies the branch
   * the theorem can be applied to.
   * @param kbs The theorem which can be applied.
   * @param s The Substitution necessary for the application
   * of the epsilon rule.
   * @param ims List of InitialMatches used in the construction
   * of this TheoremApplication
   */
  private TheoremApplication(TableauFormula lowest, KBSequent kbs,
                             Substitution s, ArrayList ims,
                             TreeSet splitz) {
    this.lowestFormula = lowest;
    this.sequent = kbs;
    this.subst = s;
    this.initialMatches = ims;
    this.splits = splitz;
    this.rank = this.calculateRank();
  }

  /**
   * The idea behind this method is that the prover should
   * be able to pick which sequent it uses intelligently.
   * The following are beneficial for a TheoremApplication:<br/><ul>
   * <li>Fewer needs</li>
   * <li>More InitialMatches</li>
   * <li>Smaller Substitution</li>
   * <li>Fewer involved splits</li>
   * <li>Little-used sequent</li>
   * </ul>
   *
   * @return The rank of this TheoremApplication.
   */
  private double calculateRank() {

    double rank = 0;

    // fewer needs is better
    rank += ( -40) * (this.getSequent().getPositives().size() +
                      this.getSequent().getNegatives().size() -
                      this.initialMatches.size());

    // more IMs is better
    rank += 4 * this.initialMatches.size();

    // smaller substitutions are better
    rank += ( -3) * this.subst.size();

    // smaller number of splits is better
    rank += ( -5) * this.splits.size();

    // a less-used sequent is better
    // TODO: replace this with HashMap reference
    // rank += ( -1) * this.sequent.getTimesApplied();

    return rank;
  }

  /**
   * Records the map that was used to translate sequent variables to
   * tableau variables.
   * @throws an IllegalStateException if the call to this method
   * would change the TheoremApplication (TAs are immutable).
   */
  public void setVarMap(HashMap m) {
    if (this.varMap == null)
      this.varMap = m;
    else
      throw new IllegalStateException("TheoremApplications are immutable");
  }

  /**
   * Gets this application's lowest node, which contains
   * the lowest formula in the tree which can be matched.
   * @return The lowest matchable node
   */
  public TableauFormula getLowestFormula() {
    return this.lowestFormula;
  }

  /**
   * Gets the sequent that is applied.
   * @return The KBSequent that is matched here.
   */
  public KBSequent getSequent() {
    return this.sequent;
  }

  /**
   * Gets the substitution.
   * @return The substitution necessary for the theorem to be applied
   */
  public Substitution getSubstitution() {
    return this.subst;
  }

  /**
   * Gets the list of InitialMatches used in the
   * creation of this TheoremApplication.
   * @return the List of InitialMatches
   */
  public List getInitialMatches() {
    return Collections.unmodifiableList(this.initialMatches);
  }

  public Set getSplits() {
    return this.splits;
  }

  /**
   * Creates a new TheoremApplication object with a new
   * Map which includes information about the new match
   * that was found.
   * Note: This assumes the substitution in the InitialMatch
   * is compatible with this TA's substitution.
   *
   * @param im the match to combine with this TheoremApplication
   * @param newSub the combination of the substitution of the receiver
   * and the substitution of <code>im</code>.
   * @return the result of combining <code>im</code> with the receiver.
   */
  public TheoremApplication combine(InitialMatch im,
                                    Substitution newSub) {

    // it's illegal to try create a TA from different sequents
    assert this.getSequent() == im.getSequent();

    // create the new InitialMatch list
    ArrayList newInitialMatchList =
        new ArrayList(this.initialMatches.size() + 1);
    newInitialMatchList.addAll(this.initialMatches);
    newInitialMatchList.add(im);

    // create the new set of splits
    TreeSet newSplits = new TreeSet();
    newSplits.addAll(this.splits);
    newSplits.addAll(im.getBranchFormula().getInvolvedSplits());

    // compare the height of the TA's lowest node
    // and the node of the initial match
    TableauFormula newLowestFormula;
    if (this.lowestFormula.getBirthPlace().isAncestorOf(
        im.getBranchFormula().getBirthPlace())) {
      newLowestFormula = im.getBranchFormula();
    } else {
      newLowestFormula = this.lowestFormula;
    }

    return new TheoremApplication(newLowestFormula, this.sequent,
                                  newSub, newInitialMatchList,
                                  newSplits);
  }

  /**
   * This method returns whether or not a user-specified
   * sequent formula exists within the InitialMatch list of this
   * TheoremApplication.
   * @param f The sequent formula to look for.
   * @return true if this sequent formula is contained in this
   * TheoremApplication, false otherwise.
   */
  public boolean contains(Formula f) {
    return this.getSequentFormulas().contains(f);
  }

  public int compareTo(Object o) {
    TheoremApplication ta = (TheoremApplication) o;

    if (this.rank > ta.rank)
      return 1;
    else if (this.rank == ta.rank)
      return 0;
    else
      return -1;
  }

  public boolean equals(Object o) {
    // two TheoremApplications are considered equal if
    // they were built from the same InitialMatches

    if (!(o instanceof TheoremApplication)) {
      return false;
    }

    TheoremApplication ta = (TheoremApplication) o;
    if (ta.initialMatches.size() != this.initialMatches.size()) {
      return false;
    }
    for (int i = 0; i < ta.initialMatches.size(); i++) {
      if (!this.initialMatches.contains(ta.initialMatches.get(i))) {
        return false;
      }
    }
    return true;
  }

  /**
   * Gets the branch formulas that have matched in this
   * TheoremApplication.
   * @return the list of matched branch formulas.
   */
  public List<TableauFormula> getBranchFormulas() {
    ArrayList<TableauFormula> retVal = new ArrayList<TableauFormula>(this.initialMatches.size());
    for (InitialMatch match: this.initialMatches) {
      retVal.add(match.getBranchFormula());
    }
    return retVal;
  }

  /**
   * Gets the sequent formulas that have been matched
   * in this TheoremApplication.
   * @return the list of matched sequent formulas
   */
  protected List getSequentFormulas() {
    ArrayList retVal = new ArrayList(this.initialMatches.size());
    for (int i = 0; i < this.initialMatches.size(); i++) {
      InitialMatch im = (InitialMatch)this.initialMatches.get(i);
      retVal.add(im.getSequentFormula());
    }
    return retVal;
  }

  /**
   * Gets the positive tableau formulas which are
   * matched in this TheoremApplication.
   * @return The positive formulas.
   */
  public List getPositiveFormulas() {
    ArrayList pfs = new ArrayList(this.initialMatches.size());
    TableauFormula tf;
    for (int i = 0; i < this.initialMatches.size(); i++) {
      tf = (TableauFormula) ((InitialMatch)this.initialMatches.
                             get(i)).getBranchFormula();
      if (tf.getSign()) {
        pfs.add(tf);
      }
    }
    return pfs;
  }

  /**
   * Gets the negative tableau formulas which are
   * matched in this TheoremApplication.
   * @return The negative formulas.
   */
  public List getNegativeFormulas() {
    ArrayList nfs = new ArrayList(this.initialMatches.size());
    TableauFormula tf;
    for (int i = 0; i < this.initialMatches.size(); i++) {
      tf = (TableauFormula) ((InitialMatch)this.initialMatches.
                             get(i)).getBranchFormula();
      if (!tf.getSign()) {
        nfs.add(tf);
      }
    }
    return nfs;
  }

  /**
   * This method scans through the sequent's positive
   * formulas to find the ones that haven't been used
   * by comparing those formulas with the match hash.
   *
   * @return The unused positive formulas in the sequent
   */
  public List getUnusedPositiveSeqFormulas() {

    List matchedSeqForms = this.getSequentFormulas();
    ArrayList unusedPositives = new ArrayList();
    List sps = this.sequent.getPositives();
    Formula sf;
    for (int cntr = 0; cntr < sps.size(); cntr++) {
      sf = (Formula) sps.get(cntr);
      if (!matchedSeqForms.contains(sf)) {
        unusedPositives.add(sf);
      }
    }
    return unusedPositives;
  }

  /**
   * This method scans through the sequent's negative
   * formulas to find the ones that haven't been used
   * by comparing those formulas with the match hash.
   *
   * @return The unused negative formulas in the sequent
   */
  public List getUnusedNegativeSeqFormulas() {

    List matchedSeqForms = this.getSequentFormulas();
    ArrayList unusedNegatives = new ArrayList();
    List sns = this.sequent.getNegatives();
    Formula sf;
    for (int cntr = 0; cntr < sns.size(); cntr++) {
      sf = (Formula) sns.get(cntr);
      if (!matchedSeqForms.contains(sf)) {
        unusedNegatives.add(sf);
      }
    }
    return unusedNegatives;
  }

  public String toString() {
    String retVal = new String();
    retVal += "Theorem Application (branch formula --> sequent formula):\n<br>";
    for (int i = 0; i < this.initialMatches.size(); i++) {
      retVal += this.initialMatches.get(i) + "\n<br>";
    }
    return retVal;
  }
}
