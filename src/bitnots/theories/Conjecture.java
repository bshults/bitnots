package bitnots.theories;

import bitnots.expressions.*;
import java.io.*;

/**
 * Conjecture.java
 *
 *
 * Created: Fri Jul 18 13:46:28 2003
 *
 * @author <a href="mailto:bshults@PC3395">Benjamin Shults</a>
 * @author Pete Wall
 * @version 1.0
 */
public class Conjecture extends TheoryElement {

  public String toString() {
    return this.getFormula().toString();
  }
  
  /**
   * Creates a Conjecture with the given information.
   * @param conjecture the conjecture.
   * @param name the name of this Conjecture.
   * @param desc the description of this Conjecture.
   */
  public Conjecture(Formula conjecture, String name, String desc) {
    super(conjecture, name, desc);
  } // Conjecture constructor

  /**
   * Creates a Conjecture with the given information.
   * @param conjecture the conjecture.
   * @param name the name of this Conjecture.
   */
  public Conjecture(Formula conjecture, String name) {
    this(conjecture, name, "");
  } // Conjecture constructor
  
  /**
   * Returns a string representation of the Conjecture definition.
   * Used in debugging.
   * @return a string representation of the Conjecture definition.
  public String toString()
  {
    return new String("(def-target " + this.name + " " +
                       this.conjecture + " (string \"" +
                       this.description + "\"))");
  }
   */
  
} // Conjecture

