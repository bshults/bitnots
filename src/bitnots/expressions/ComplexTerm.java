package bitnots.expressions;

import java.io.*;

/**
 * <p>Terms that are not variables should extend this class.
 * 
 * @author Benjamin Shults
 * @version .2
 */

public abstract class ComplexTerm extends Term implements Cloneable {
  
  private TermConstructor symbol;

  public TermConstructor getConstructor() {
    return this.symbol;
  }

  protected void setConstructor(TermConstructor s) {
    this.symbol = s;
  }

  public ComplexTerm clone() {
    try {
      return (ComplexTerm) super.clone();
    } catch (CloneNotSupportedException e) {
      throw new Error();
    }
  }

  protected ComplexTerm(TermConstructor sym) {
    this.symbol = sym;
  }

} // ComplexTerm
