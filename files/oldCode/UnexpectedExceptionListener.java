package bitnots.eventthreads;

/**
 * UnexpectedExceptionListener.java
 *
 *
 * Created: Fri Jan 02 14:18:53 2004
 *
 * @author <a href="mailto:bshults@PC3395">Benjamin Shults</a>
 * @version 1.0
 */

public interface UnexpectedExceptionListener extends java.util.EventListener {

  public void unexpectedEvent(UnexpectedExceptionEvent uee);

}// UnexpectedExceptionListener

