package bitnots.tableaux;

import java.util.EventListener;


/**
 * 
 * @author bshults
 *
 */
public interface SpliceListener extends EventListener {
  
  /**
   * @see SpliceEvent
   * @param e
   */
  public void spliceOccurred(SpliceEvent e);

}
