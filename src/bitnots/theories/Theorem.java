package bitnots.theories;

import bitnots.expressions.*;
import java.io.*;

/**
 * Theorem.java
 *
 *
 * Created: Fri Jul 18 13:46:28 2003
 *
 * @author <a href="mailto:bshults@PC3395">Benjamin Shults</a>
 * @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
 * @version 1.0
 */
public class Theorem extends TheoryElement {

  /**
   * Creates a Theorem with the given information.
   * @param theorem the theorem.
   * @param name the name of this Theorem.
   * @param desc the description of this Theorem.
   */
  public Theorem(Formula theorem, String name, String desc) {
    super(theorem, name, desc);
  } // Theorem constructor

  /**
   * Creates a Theorem with the given information.
   * @param theorem the theorem.
   * @param name the name of this Theorem.
   */
  public Theorem(Formula theorem, String name) {
    this(theorem, name, "");
  } // Theorem constructor
  
  /**
   * Returns the theorem for this Theorem definition.
   * @return the theorem for this Theorem definition.
   */
  public Formula getTheorem() {
    return this.getFormula();
  }
  
  /**
   * Returns a string representation of the Theorem definition.  Used
   * in debugging.
   * @return a string representation of the Theorem definition.
   */
  public String toString()
  {
    return new String("(def-theorem " + this.getName() + " " +
                       this.getTheorem() + " (string \"" +
                       this.getDescription() + "\"))");
  }
  
} // Theorem

