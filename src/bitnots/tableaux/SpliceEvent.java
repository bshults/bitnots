package bitnots.tableaux;

import java.util.EventObject;


/**
 * Fired when a node is spliced into a tree.
 * @author bshults
 *
 */
public class SpliceEvent extends EventObject {
  
  private TableauNode newLocation;

  /**
   * 
   * @return the node under which the splice occurred.
   */
  public TableauNode getOldLocation() {
    return (TableauNode) this.getSource();
  }
  
  /**
   * 
   * @return the node that was spliced in.
   */
  public TableauNode getNewLocation() {
    return this.newLocation;
  }
  
  /**
   * 
   * @param source the node under which the splice occurred.
   * @param newLocation the node that was spliced in.
   */
  public SpliceEvent(TableauNode source, TableauNode newLocation) {
    super(source);
    this.newLocation = newLocation;
  }
}
