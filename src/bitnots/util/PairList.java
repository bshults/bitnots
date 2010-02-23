package bitnots.util;

/**
 * @author Benjamin Shults
 * @version .2
 */

public class PairList  {
  
  /** The key to this pair. */
  public Object key;
  /** The data associated with key in this list. */
  public Object data;
  /** The next pair in the list. */
  public PairList next;

  /** There is no such thing as an empty PairList. */
  public PairList(Object key, Object data) {
    this.key = key;
    this.data = data;
  }
  
  public PairList(Object key, Object data, PairList next) {
    this.key = key;
    this.data = data;
    this.next = next;
  }
  
  public Object assoc(Object key) {
    if (key.equals(this.key))
      return this.data;
    else if (this.next != null)
      return this.next.assoc(key);
    else
      return null;
  }

} // PairList

