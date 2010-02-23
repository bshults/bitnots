package bitnots.util;



import java.util.Collection;

import java.util.Comparator;

import java.util.NoSuchElementException;
import java.util.Iterator;

import java.util.AbstractSet;
import java.util.SortedSet;
import java.io.*;

/**

 *

 * @author <a href="mailto:shults@PIPPIN">Benjamin Shults</a>

 * @version

 */



public class Collections {

  public static boolean every(Collection c, PredicateBlock p) {
    Iterator it = c.iterator();
    while (it.hasNext()) {
      if (!p.test(it.next()))
        return false;
    }
    return true;
  }

  public static void deleteIf(Collection c, PredicateBlock p) {
    Iterator it = c.iterator();
    while (it.hasNext()) {
      if (p.test(it.next()))
        it.remove();
    }
  }

  /**
   * Returns an immutable, sorted set containing only the specified
   * object.  The returned set is serializable.
   *
   * @param o the sole object to be stored in the returned set.
   * @return an immutable, sorted set containing only the specified
   * object.  */
  public static SortedSet singletonSortedSet(Object o) {
    return new SingletonSet(o);
  }

  private static class SingletonSet extends AbstractSet
    implements Serializable, CloneableSortedSet {

    private Comparable element;

    SingletonSet(Object o) {
      this.element = (Comparable) o;
    }

    // why not, it's immutable!
    public Object clone() {
      return this;
    }

    public Comparator comparator() {
      return null;
    }

    public SortedSet headSet(Object e) {
      if (this.element.compareTo(e) < 0)
        return this;
      else
        throw new IllegalArgumentException();
    }

    public SortedSet tailSet(Object e) {
      if (this.element.compareTo(e) > 0)
        return this;
      else
        throw new IllegalArgumentException();
    }

    public Object first() {
      return this.element;
    }

    public Object last() {
      return this.element;
    }

    public SortedSet subSet(Object from, Object to) {
      if (this.element.compareTo(from) > 0 &&
          this.element.compareTo(to) < 0)
        return this;
      else
        throw new IllegalArgumentException();
    }

    public Iterator iterator() {
      return new Iterator() {
          private boolean hasNext = true;
          public boolean hasNext() {
            return hasNext;
          }
          public Object next() {
            if (hasNext) {
              hasNext = false;
              return element;
            }
            throw new NoSuchElementException();
          }
          public void remove() {
            throw new UnsupportedOperationException();
          }
        };
    }

    public int size() {
      return 1;
    }

    public boolean contains(Object o) {
      return element == null ? o == null : element.equals(o);
    }
  }

  public static void appendToBuffer(Collection c, StringBuffer sb,
                                    String terminator) {
    Iterator it = c.iterator();
    while (it.hasNext()) {
      sb.append(it.next() + terminator);
    }
  }

  public static void appendToBuffer(Collection c, StringBuffer sb,
                                    String beginner, String terminator) {
    Iterator it = c.iterator();
    while (it.hasNext()) {
      sb.append(beginner + it.next() + terminator);
    }
  }

}// Collections

