package bitnots.util;



import java.util.*;



/**
 * 
 * @author bshults
 *
 * @param <E>
 */
public interface CloneableIterator<E> extends Iterator<E>, Cloneable {



  /**
   * 
   * @return
   */
  public CloneableIterator<E> clone();



}



