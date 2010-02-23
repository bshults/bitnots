package bitnots.theories;

import bitnots.expressions.*;
import java.util.*;

/**
 * KBSequent.java
 *
 * <p>A sequent within the knowledge base is a combination of
 * positive and negative formulas that need to be matched
 * in the application of the epsilon-rule.</p>
 *
 * @author Benjamin Shults
 * @author Daniel W. Farmer
 * @version 1.0
 */
// TODO make this a TreeNode.
public class KBSequent {

  /** The list of positive Formulas to be matched. */
  private List<Formula> positiveFormulas;

  /** The list of negative Formulas to be matched. */
  private List<Formula> negativeFormulas;

  /** The format of this sequent. */
  private String format;

  /** The name of the theorem represented by this sequent. */
  private String name;

  /** Times this sequent has been used */
  // TODO: this shouldn't be here.  Use a HashMap or something to track 
  // how many times one of these has been applied.
  // private int timesApplied;

  /**
   * Returns true if and only if this sequent has a single formula, it
   * is negative and it is an equality predicate.
   * @return true if and only if this sequent has a single formula, it
   * is negative and it is an equality predicate.
   */
  public boolean isEquality() {
    return this.getPositives().isEmpty() &&
      this.getNegatives().size() == 1 &&
      this.getNegatives().get(0) instanceof Predicate &&
      ((Predicate) this.getNegatives().get(0)).
      getConstructor().toString().equals("=");
  }

  /**
   * Gets the positive formulas.
   * @return The list of positive formulas.
   */
  public List getPositives() {
    return Collections.unmodifiableList(this.positiveFormulas);
  }

  /**
   * Gets the negative formulas.
   * @return The list of negative formulas.
   */
  public List getNegatives() {
    return Collections.unmodifiableList(this.negativeFormulas);
  }

  /**
   * Gets the sequent's format.
   * @return this sequent's format.
   */
  public String getFormat() {
    return this.format;
  }

  /**
   * Gets the sequent's name.
   * @return this sequent's name.
   */
  public String getName() {
    return this.name;
  }

  /**
   * Sets the list of positive formulas.
   * @param pos this sequent's positive formulas.
   */
  public void setPositives(List pos) {
    if (pos == null)
      this.positiveFormulas = new ArrayList();
    else
      this.positiveFormulas = pos;
  }

  /**
   * Sets the list of negative formulas.
   * @param neg this sequent's negative formulas.
   */
  public void setNegatives(List neg) {
    if (neg == null)
      this.negativeFormulas = new ArrayList();
    else
      this.negativeFormulas = neg;
  }

  /**
   * Sets the format.
   * @param form this sequent's format.
   */
  public void setFormat(String form) {
    if (form == null)
      this.format = "";
    else
      this.format = form;
  }

  /**
   * Sets the name.
   * @param nam this sequent's name.
   */
  public void setName(String nam) {
    if (nam == null)
      this.name = "";
    else
      this.name = nam;
  }

  /**
   * Adds a positive formula to the sequent.
   * @param pos the positive formula to be added.
   */
  public void addPositive(Formula pos) {
    this.positiveFormulas.add(pos);
  }

  /**
   * Adds a negative formula to the sequent.
   * @param neg the negative formula to be added.
   */
  public void addNegative(Formula neg) {
    this.negativeFormulas.add(neg);
  }

  /**
   * Tells this sequent it's been applied.
     void applied() {
    ++this.timesApplied;
  }*/


  /**
   * Returns the number of times this sequent has been applied.
   * @return time the sequent's been applied.
  int getTimesApplied() {
    return this.timesApplied;
  }
   */

  /**
   * Resets the number of times applied to zero.
  public void resetTimesApplied() {
    this.timesApplied = 0;
  }
   */

  @Override
  public String toString() {
    String retVal = new String(" ");
    retVal += this.name + ":\n   ";
    for (int a = 0; a < this.positiveFormulas.size(); a++) {
      retVal += "Sp " + ((Formula) this.positiveFormulas.get(a));
      retVal += "\n   ";
    }
    for (int b = 0; b < this.negativeFormulas.size(); b++) {
      retVal += "Sh " + ((Formula) this.negativeFormulas.get(b));
      retVal += "\n   ";
    }
    return retVal;
  }

  /**
   * Initializes empty lists of formulas and strings
   */
  public KBSequent() {
    this(new ArrayList(), new ArrayList(), "", "");
  }

  /**
   * Full constructor
   *
   * @param pos the positive formulas
   * @param neg the negative formulas
   * @param form the sequent's format
   * @param nam the sequent's name
   */
  public KBSequent(ArrayList pos, ArrayList neg, String form,
                   String nam) {
    this.setPositives(pos);
    this.setNegatives(neg);
    this.setFormat(form);
    this.setName(nam);
    // this.timesApplied = 0;
  }

}
