package bitnots.eventthreads;

/**
 * UnexpectedExceptionEvent.java
 *
 *
 * Created: Fri Jan 02 13:43:22 2004
 *
 * @author <a href="mailto:bshults@PC3395">Benjamin Shults</a>
 * @version 1.0
 */
public class UnexpectedExceptionEvent extends java.util.EventObject {

  protected Exception exception;

  public Exception getException() {
    return this.exception;
  }

  public UnexpectedExceptionEvent(Object source, Exception e) {
    super(source);
    this.exception = e;
  } // UnexpectedExceptionEvent constructor
  
} // UnexpectedExceptionEvent
