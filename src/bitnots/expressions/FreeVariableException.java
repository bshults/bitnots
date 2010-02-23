package bitnots.expressions;

/**
 * @author Benjamin Shults
 * @version .2
 */

public class FreeVariableException extends Exception {

  public FreeVariableException(Throwable t) {
    super(t);
  }
  
  public FreeVariableException(String s, Throwable t) {
    super(s, t);
  }
  
  public FreeVariableException(String s) {
    super(s);
  }
  
  public FreeVariableException() {
  }
  
} // FreeVariableException
