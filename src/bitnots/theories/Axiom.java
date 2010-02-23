package bitnots.theories;

import bitnots.expressions.*;
import java.io.*;

/**
 * Axiom.java
 *
 *
 * Created: Fri Jul 18 13:46:28 2003
 *
 * @author <a href="mailto:bshults@PC3395">Benjamin Shults</a>
 * @author Pete Wall
 * @version 1.0
 */
public class Axiom extends TheoryElement {

  /**
  * Returns the axiom for this Axiom definition.
  * @return the axiom for this Axiom definition.
  */
  public Formula getAxiom() {
    return this.getFormula();
  }
  
  /**
   * Creates a Axiom with the given information.
   * @param axiom the axiom.
   * @param name the name of this Axiom.
   * @param desc the description of this Axiom.
   */
  public Axiom(Formula axiom, String name, String desc) {
    super(axiom, name, desc);
  } // Axiom constructor

  /**
   * Creates a Axiom with the given information.
   * @param axiom the axiom.
   * @param name the name of this Axiom.
   */
  public Axiom(Formula axiom, String name) {
    this(axiom, name, "");
  } // Axiom constructor
} // Axiom

