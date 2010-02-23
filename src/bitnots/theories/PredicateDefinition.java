package bitnots.theories;

import bitnots.expressions.*;
import java.io.*;

/**
 * PredicateDefinition.java
 *
 *
 * Created: Fri Jul 18 13:46:28 2003
 *
 * @author <a href="mailto:bshults@PC3395">Benjamin Shults</a>
 * @version 1.0
 */
public class PredicateDefinition extends TheoryElement {

  private Predicate definiendum;
  private String format;

  /**
   * Creates a PredicateDefinition with the given information.
   * @param definiendum the predicate being defined.
   * @param definiens the definition.
   * @param format the format string for output.
   * @param name the name of this PredicateDefinition.
   */
  public PredicateDefinition(Predicate definiendum, Formula definiens,
                             String format, String name) {
    super(definiens, name, "Definition of " + definiendum.getConstructor().getName());
    this.definiendum = definiendum;
    this.format = format;
  } // PredicateDefinition constructor

  /**
   * Returns the definiendum for this Predicate definition.
   * @return the definiendum for this Predicate definition.
   */
  public Predicate getDefiniendum() {
    return this.definiendum;
  }

  /**
   * Returns the definiens for this Predicate definition.
   * @return the definiens for this Predicate definition.
   */
  public Formula getDefiniens() {
    return this.getFormula();
  }
  
  /**
  * Returns the format for this Predicate definition.
  * @return the format for this Predicate definition.
  */
  public String getFormat() {
    return this.format;
  }
  
  /**
   * Returns a string representation of the predicate definition.
   * Used in debugging.
   * @return a string representation of the predicate definition.
   */
  public String toString() {
    return new String("(def-predicate " + this.definiendum + " " +
                       this.getDefiniens() + " (string \"" +
                       this.getName() + "\") (format \"" +
                       this.format + "\"))");
  }
} // PredicateDefinition

