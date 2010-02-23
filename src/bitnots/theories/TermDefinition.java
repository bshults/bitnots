package bitnots.theories;

import bitnots.expressions.*;

import java.io.*;
import java.util.Arrays;

/**
 * TermDefinition.java
 *
 *
 * Created: Fri Jul 18 13:46:28 2003
 *
 * @author <a href="mailto:bshults@PC3395">Benjamin Shults</a>
 * @version 1.0
 */
public class TermDefinition extends TheoryElement {

  private ComplexTerm definiendum;
  private ComplexTerm definiens;
  private String format;

  /**
   * Creates a TermDefinition with the given information.
   * @param definiendum the term being defined.
   * @param definiens the definition.
   * @param format the format string for output.
   * @param name the name of this TermDefinition.
   */
  public TermDefinition(ComplexTerm definiendum, ComplexTerm definiens,
                        String format, String name) {
    super(FormulaFactory.getFormula(PredicateConstructor.EQUALITY_SYMBOL,
                                    new Object[] {Arrays.asList(definiendum, definiens)}), 
                                    name,
                                    "Definition of " + definiendum.getConstructor().getName());
    this.definiendum = definiendum;
    this.definiens = definiens;
    this.format = format;
  } // TermDefinition constructor
  
  /**
   * Returns the definiendum for this Term definition.
   * @return the definiendum for this Term definition.
   */
  public Term getDefiniendum() {
    return this.definiendum;
  }

  /**
   * Returns the definiens for this Term definition.
   * @return the definiens for this Term definition.
   */
  public Term getDefiniens() {
    return this.definiens;
  }
  
  /**
   * Returns the format for this Term definition.
   * @return the format for this Term definition.
   */
  public String getFormat() {
    return this.format;
  }
  
  /**
   * Returns a string representation of the term definition.
   * Used in debugging.
   * @return a string representation of the term definition.
   */
  public String toString() {
    return new String("(def-term " + this.definiendum + " " +
                       this.definiens + " (string \"" +
                       this.getName() + "\") (format \"" +
                       this.format + "\"))");
  }
} // TermDefinition

