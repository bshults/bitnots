package bitnots.util;

import java.util.*;

/**
 * This is not a good collection API citizen due to the fact that I allow
 * modification of the collection through methods that are not part of the
 * Collection API and the fact that this allows shared structure between
 * instances. Thus it is possible to break the synchronization and unmodifiable
 * guarantees from the Collections class.
 * 
 * This class should be used with EXTREME CAUTION.
 * 
 * @author Benjamin Shults
 * @version .2
 */

// TODO: get rid of all uses of ExposedLinkedList except when it is
// really being utilized. Also make ExposedLinkedList stop
// implementing Collection.
// TODO: I'm thinking seriously of having the newHyps and newGoals
// fields in ProofNode be instances of this.
public class ExposedLinkedList<E> extends AbstractSet<E> implements Cloneable {

  protected Node<E> head = null;

  protected Node<E> tail = null;

  // public Object[] toArray() {
  // throw new UnsupportedOperationException();
  // }

  // public Object[] toArray(Object[] o) {
  // throw new UnsupportedOperationException();
  // }

  // public boolean containsAll(Collection c) {
  // throw new UnsupportedOperationException();
  // }

  // public boolean addAll(Collection c) {
  // throw new UnsupportedOperationException();
  // }

  // public boolean removeAll(Collection c) {
  // throw new UnsupportedOperationException();
  // }

  // public boolean retainAll(Collection c) {
  // throw new UnsupportedOperationException();
  // }

  /**
   * 
   * @param o the thing to be removed.
   * @return true if the collection changed.
   */
  @Override
  public boolean remove(Object o) {
    if (this.head == null)
      return false;
    else if (this.head.userObject.equals(o)) {
      this.head = this.head.next;
      if (this.head == null)
        this.tail = null;
      return true;
    } else {
      Node<E> current = this.head;
      for (; current != this.tail; current = current.next) {
        if (current.next.userObject.equals(o)) {
          if (current.next == this.tail)
            this.tail = current;
          current.next = current.next.next;
          return true;
        }
      }
      return false;
    }
  }

  /**
   * @see java.util.AbstractCollection#isEmpty()
   */
  @Override
  public boolean isEmpty() {
    return this.head == null;
  }

  /**
   * This destroys <code>ell</code> and the resulting stack shares structure
   * with the argument. <b>The argument must never be used after this operation
   * as it is left in an invalid state.</b>
   * @param ell 
   */
  public void prepend(ExposedLinkedList<E> ell) {
    if (ell.tail != null) {
      ell.tail.next = this.head;
      this.head = ell.head;
    }
  }

  /**
   * @see java.lang.Object#clone()
   */
  @Override
  public ExposedLinkedList<E> clone() {
    ExposedLinkedList<E> retVal;
    try {
      retVal = (ExposedLinkedList<E>) super.clone();
      Iterator<E> it = this.iterator();
      while (it.hasNext()) {
        retVal.addEnd(it.next());
      }
    } catch (CloneNotSupportedException e) {
      throw new Error(e);
    }
    return retVal;
  }

  public void push(E o) {
    this.addFront(o);
  }

  public E first() {
    if (this.head == null)
      throw new NoSuchElementException();
    else
      return this.head.userObject;
  }

  public Object pop() {
    if (this.head == null)
      throw new NoSuchElementException();
    else {
      Object retVal = this.head.userObject;
      this.head = this.head.next;
      if (this.head == null)
        this.tail = null;
      return retVal;
    }
  }

  @Override
  public String toString() {
    if (this.isEmpty())
      return "()";
    StringBuffer sb = new StringBuffer("(");
    Iterator<E> it = this.iterator();
    sb.append(it.next());
    while (it.hasNext()) {
      sb.append(" " + it.next());
    }
    sb.append(")");
    return sb.toString();
  }

  /**
   * Modify the receiver by appending the argument. The argument is not
   * modified.
   * @param l the ExposedLinkedList that is to be appended to this.
   */
  public ExposedLinkedList<E> nconc(ExposedLinkedList<E> l) {

    if (this.tail == null) {
      this.tail = l.tail;
      this.head = l.head;
    } else {
      this.tail.next = l.head;
      this.tail = l.tail;
    }
    return this;
  }

  /**
   * Returns a new ExposedLinkedList containing the same elements as the
   * receiver and l. Neither list is modified. The receiver is copied and the
   * result shares structure with the argument.
   * @param l the ExposedLinkedList that is to be appended to this.
   */
  public ExposedLinkedList<E> append(ExposedLinkedList<E> l) {
    if (this.tail == null)
      return l;
    ExposedLinkedList<E> value = this.clone();
    value.tail.next = l.head;
    value.tail = l.tail;
    return value;
  }

  public void enqueue(E o) {
    this.addEnd(o);
  }

  public void addEnd(E o) {
    if (this.head == null)
      this.addFront(o);
    else
      this.tail = this.tail.next = new Node<E>(o);
  }

  /**
   * Adds to the end.
   */
  @Override
  public boolean add(E o) {
    this.addEnd(o);
    return true;
  }

  public void addFront(E o) {
    if (this.head == null)
      this.head = this.tail = new Node<E>(o);
    else
      this.head = new Node<E>(o, this.head);
  }

  public ExposedLinkedList() {
  }

  public ExposedLinkedList(E o, ExposedLinkedList<E> l) {
    this.head = new Node<E>(o, l.head);
    this.tail = l.tail;
  }

  public ExposedLinkedList(E o) {
    this.head = this.tail = new Node<E>(o);
  }

  /**
   * The iterator that this returns is not thread safe. The client must
   * synchronize the code from its creation until its last use.
   */
  @Override
  public Iterator<E> iterator() {
    return new NodeIterator();
  }

  public class NodeIterator implements Iterator<E> {

    private Node<E> previous = null;

    private Node<E> current = null;

    public E next() {
      if (!this.hasNext())
        throw new NoSuchElementException();
      if (this.current == null) {
        this.current = ExposedLinkedList.this.head;
      } else {
        this.previous = this.current;
        this.current = this.current.next;
      }
      return this.current.userObject;
    }

    public boolean hasNext() {
      if (this.current == null)
        return ExposedLinkedList.this.head != null;
      else
        return this.current.next != null;
    }

    /**
     * This is supported here.
     */
    public void remove() {
      if (this.previous == this.current)
        throw new IllegalStateException();
      else if (this.previous == null) {
        ExposedLinkedList.this.head = this.current.next;
        this.current = null;
      } else {
        this.previous.next = this.current.next;
        this.current = this.previous;
      }
    }

    // public NodeIterator(ExposedLinkedList ell) {
    // this.current = ell.head;
    // }
  }

  public static class Node<E> {

    protected E userObject;

    protected Node<E> next;

    public Node(E o) {
      this(o, null);
    }

    public Node(E o, Node<E> n) {
      this.userObject = o;
      this.next = n;
    }
  }

  @Override
  public void clear() {
    this.head = null;
    this.tail = null;
  }

  // Because this allows shared structure, there is no longer a
  // guarantee that the size field is accurate.
  @Override
  public int size() {
    int retVal;
    Iterator<E> it = this.iterator();
    for (retVal = 0; it.hasNext(); retVal++, it.next())
      ;
    return retVal;
  }

  @Override
  public boolean contains(Object o) {
    Iterator<E> it = this.iterator();
    while (it.hasNext()) {
      if (it.next().equals(o))
        return true;
    }
    return false;
  }

} // ExposedLinkedList
