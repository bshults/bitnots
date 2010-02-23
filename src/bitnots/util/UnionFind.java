package bitnots.util;

/**
 * UnionFind.java
 *
 *
 * Created: Tue Jun 08 15:21:37 2004
 *
 * @author <a href="mailto:b-shults@bethel.edu">Benjamin Shults</a>
 * @version 1.0
 */

public class UnionFind {

  private Object userObject;

  private UnionFind find;

  public Object getUserObject() {
    return this.userObject;
  }

  // flattenning find.
  public UnionFind find() {
    UnionFind next = this.find;
    if (next != this) {
      return this.find = next.secretFind();
    }
    return this;
  }

  // flattenning find.
  private UnionFind secretFind() {
    UnionFind next = this.find;
    if (next != this) {
      return this.find = next.secretFind();
    }
    return this;
  }

  public UnionFind union(UnionFind a, UnionFind b) {
    a = a.find();
    b = b.find();
    return b.find().find = a.find();
  }

  public UnionFind(Object userObject) {
    this.userObject = userObject;
    this.find = this;
  } // UnionFind constructor
  
} // UnionFind
