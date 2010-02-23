package bitnots.util;



import java.util.*;



/**

 * This is a simple implementation of an association list using linked

 * pairs.

 * 

 * @author Benjamin Shults

 * @version .2

 */



public class AssociationList {

  

  /** This is the first pair in the pair list. */

  private PairList head;



  // Currently, this does not remove the previous association.

  public void put(Object key, Object data) {

    this.head = new PairList(key, data, this.head);

  }



  /** This returns the object associated with key.  If there is no

   * pair with key as its key, then null is returned. */

  public Object assoc(Object key) {

    if (this.head == null)

      return null;

    else

      return this.head.assoc(key);

  }



  public boolean containsKey(Object o) {

    return this.assoc(o) != null;

  }



  private class AIterator implements Iterator {

    // Invariant: The only time current==previous is when next has not

    // been called since the last call to remove.



    private PairList current = null;

    private PairList previous = null;



    public Object next() {

      if (!this.hasNext())

          throw new NoSuchElementException();        

      if (this.current == null) {

        this.current = AssociationList.this.head;

      } else {

        this.previous = this.current;

        this.current = this.current.getNext();

      }

      return this.current;

    }



    public boolean hasNext() {

      return this.current == null ? AssociationList.this.head != null :

        this.current.next != null;

    }



    public void remove() {

      if (this.current == this.previous)

        throw new IllegalStateException();

      if (this.previous == null)

        AssociationList.this.head = this.current.getNext();

      else

        this.previous.setNext(this.current.getNext());

      // This is key to getting rid of a subtle bug in the

      // relationship between next and remove.

      this.current = this.previous;

    }

  }



  public AssociationList() {}

  

  /** Create an alist with some initial data. */

  public AssociationList(Object key, Object data) {

    this.head = new PairList(key, data);

  }

  

  private static class PairList implements Map.Entry {

  

    /** The key to this pair. */

    private Object key;

    /** The value associated with key in this list. */

    private Object value;

    /** The next pair in the list. */

    private PairList next;



    public PairList getNext() {

      return this.next;

    }



    public void setNext(PairList n) {

      this.next = n;

    }



    public Object getKey() {

      return this.key;

    }



    public Object getValue() {

      return this.value;

    }



    public Object setValue(Object o) {

      throw new UnsupportedOperationException();

    }



    public boolean equals(Object o) {

      if (o instanceof Map.Entry) {

        Map.Entry e2 = (Map.Entry) o;

        return (this.getKey()==null ?

                e2.getKey()==null : this.getKey().equals(e2.getKey()))  &&

          (this.getValue()==null ?

           e2.getValue()==null : this.getValue().equals(e2.getValue()));

      } else

        return false;

    }



    public int hashCode() {

      return (this.getKey() == null ? 0 : this.getKey().hashCode()) ^

        (this.getValue() == null ? 0 : this.getValue().hashCode());

    }



    private Object assoc(Object key) {

      if (key.equals(this.key))

        return this.value;

      else if (this.next != null)

        return this.next.assoc(key);

      else

        return null;

    }



    /** There is no such thing as an empty PairList. */

    private PairList(Object key, Object value) {

      this(key, value, null);

    }

  

    private PairList(Object key, Object value, PairList next) {

      if (key == null || value == null)

        throw new NullPointerException();

      else {

        this.key = key;

        this.value = value;

        this.next = next;

      }

    }

  

  } // PairList

} // AssociationList

