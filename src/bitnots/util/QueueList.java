package bitnots.util;

import java.util.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class QueueList<E> {

  protected Node<E> head = null;
  protected Node<E> tail = null;

  public E peekFront() {
    if (this.head == null)
      throw new NoSuchElementException();
    else
      return this.head.userObject;
  }

  public E dequeue() {
    if (this.head == null)
      throw new NoSuchElementException();
    else {
      E value = this.head.userObject;
      this.head = this.head.next;
      if (this.head == null)
        this.tail = null;
      return value;
    }
  }

  public void enqueue(E o) {
    if (this.tail == null)
      this.tail = this.head = new Node<E>(o);
    else {
      this.tail.next = new Node<E>(o);
      this.tail = this.tail.next;
    }
  }

  public boolean isEmpty() {
    return this.head == null;
  }

  public synchronized void clear() {
    this.head = null;
    this.tail = null;
  }

  public QueueList() {}
  
  protected static class Node<E> {
    protected E userObject;
    protected Node next;

    protected Node(E o) {
      this(o, null);
    }

    protected Node(E o, Node n) {
      this.userObject = o;
      this.next = n;
    }
  }

}// QueueList
