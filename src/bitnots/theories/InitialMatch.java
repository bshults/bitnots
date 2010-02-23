package bitnots.theories;

import bitnots.tableaux.*;
import bitnots.expressions.*;

/**
 * This class is used to remember the way in which
 * a sequent formula matched a TableauFormula. It
 * is the first step towards all full-fledged
 * TheoremApplication.
 *
 * @author <a href="mailto:b-shults@bethel.edu">Benjamin Shults</a>
 * @author Daniel W. Farmer
 * @version 1.1
 */
public class InitialMatch {

  /** The substitution allowing the unification of the formulas.  This
   * contains sequent variables. */
  private Substitution sub;

  /** The branch formula that was matched */
  private TableauFormula branchFormula;

  /** The sequent formula that was matched */
  private Formula sequentFormula;

  /** The sequent that owns the sequent formula */
  private KBSequent sequent;

  /**
   * Creates an InitialMatch which remembers
   * how two formulas (one from the tableau, one from
   * a sequent) are matched (thanks to a substitution).
   * @param s The match-enabling substitution
   * @param tf The tableau formula
   * @param sf The sequent formula
   * @param sequent The sequent that owns the sequent formula
   */
  public InitialMatch(Substitution s, TableauFormula tf,
                      Formula sf, KBSequent sequent) {
    this.sub = s;
    this.sequent = sequent;
    this.branchFormula = tf;
    this.sequentFormula = sf;
  }

  /**
   * Gets the sequent that was matched.
   * @return the sequent that owns the matched sequent formula
   */
  public KBSequent getSequent() {
    return this.sequent;
  }

  public boolean equals(Object o) {
    if (o instanceof InitialMatch) {
      InitialMatch im = (InitialMatch) o;

      if (im.getSequent() != this.getSequent() ||
          im.getBranchFormula() != this.branchFormula ||
          im.getSequentFormula() != this.sequentFormula) {
        return false;
      } else
        return true;
    } else
      return false;
  }

  /**
   * Gets the substitution.
   * @return the match-enabling substitution
   */
  public Substitution getSubstitution() {
    return this.sub;
  }

  /**
   * Gets the matched branch formula.
   * @return the tableau's branch formula
   *   that was matched
   */
  public TableauFormula getBranchFormula() {
    return this.branchFormula;
  }

  /**
   * Gets the sequent formula that was matched.
   * @return the matched sequent formula
   */
  public Formula getSequentFormula() {
    return this.sequentFormula;
  }

  public String toString() {
    return "  " + this.branchFormula + " --> " + this.sequentFormula;
  }
}
