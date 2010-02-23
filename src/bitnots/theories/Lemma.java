package bitnots.theories;

import bitnots.expressions.*;
import java.io.*;

/**
 * Lemma.java
 *
 *
 * Created: Fri Jul 18 13:46:28 2003
 *
 * @author Pete Wall
 * @version 1.0
 */
public class Lemma extends TheoryElement implements Serializable {

  /**
   * Creates a Lemma with the given information.
   * @param lemma the lemma.
   * @param name the name of this Lemma.
   * @param desc the description of this Lemma.
   */
  public Lemma(Formula lemma, String name, String desc) {
    super(lemma, name, desc);
  } // Lemma constructor

  /**
   * Creates a Lemma with the given information.
   * @param lemma the lemma.
   * @param name the name of this Lemma.
   */
  public Lemma(Formula lemma, String name) {
    this(lemma, name, "");
  } // Lemma constructor
  
  /**
  * Returns the lemma for this Lemma definition.
  * @return the lemma for this Lemma definition.
  */
  public Formula getLemma() {
    return this.getFormula();
  }
  
  /**
   * Returns a string representation of the Lemma definition.
   * Used in debugging.
   * @return a string representation of the Lemma definition.
   */
  public String toString() {
    return new String("(def-lemma " + this.getName() + " " +
                       this.getLemma() + " (string \"" +
                       this.getDescription() + "\"))");
  }
  
} // Lemma

