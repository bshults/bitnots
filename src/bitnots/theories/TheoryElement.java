package bitnots.theories;

import java.io.*;

import bitnots.expressions.Formula;

/**
 * Abstract class to be extended by definitions, axioms, etc.
 * 
 * @author <a href="mailto:bshults@PC3395">Benjamin Shults</a>
 * @version 1.0
 */

public abstract class TheoryElement implements Serializable {
  private Formula formula;
  private String name;
  private String description;

  /**
   * Returns the axiom for this Axiom definition.
   * @return the axiom for this Axiom definition.
   */
  public Formula getFormula() {
    return this.formula;
  }
  
  /**
   * Returns the name for this Axiom definition.
   * @return the name for this Axiom definition.
   */
  public String getName() {
    return this.name;
  }
  
  /**
   * Returns the description for this Axiom definition.
   * @return the description for this Axiom definition.
   */
  public String getDescription() {
    return this.description;
  }

  /**
   * Creates a TheoryElement with the given information.
   * @param formula the formula.
   * @param name the name of this TheoryElement.
   * @param desc the description of this TheoryElement.
   */
  public TheoryElement(Formula formula, String name, String desc) {
    this.formula = formula;
    this.name = name;
    this.description = desc;
  }

  /**
   * Creates a TheoryElement with the given information.
   * @param formula the axiom.
   * @param name the name of this Axiom.
   */
  public TheoryElement(Formula formula, String name) {
    this(formula, name, "");
  }
} // TheoryElement

