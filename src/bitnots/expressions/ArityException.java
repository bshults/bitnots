package bitnots.expressions;

/**
 * Thrown when an arity clash is found someplace other than in the
 * parser.  This really should never occur and this class may be
 * depricated in the future.
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

// TODO: try to make sure this can't happen, then get rid of this
// class.

public class ArityException extends RuntimeException {

  public ArityException(Throwable t) {
    super(t);
  }
  
  public ArityException(String s) {
    super(s);
  }

  public ArityException(String s, Throwable t) {
    super(s, t);
  }

  public ArityException() {
  }

}// ArityException
