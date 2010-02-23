package bitnots.util;



import java.util.*;



/**

 * TODO: Don't use a comparator with this, it won't work.

 * 

 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>

 * @version .2

 */



public class SortedArraySet extends AbstractSet implements SortedSet,

                                                           Cloneable {



  /**

   * The amount to add to the length of the array when expansion is

   * needed.

   */

  private static final int incrementalRellocation = 3;



  private static final int defaultCapacity = 3;



  protected Object[] elements = null;

  protected int size = 0;

//  protected Comparator comparator = null;



  public boolean add(Object o) {

    if (o instanceof Comparable) {

      Comparable c = (Comparable) o;

      int mid = this.size / 2;

      // high will always be out of range.

      int high = this.size;

      // low will be the lowest possible point.

      int low = 0;

      do {

        int comp = c.compareTo(this.elements[mid]);

        if (comp == 0)

          return false;

        else if (comp < 0)

          high = mid;

        else

          low = mid + 1;

        mid = (high + low) / 2;

      } while (mid > low);

      // at this point, if mid == high then c < elements[low] 

      if (mid == high)

        // insert at low

        this.justInsert(c, low);

      else if (high == this.size) // mid < high so insert at high

        this.addToEnd(c);

      else

        this.justInsert(c, high);

      return true;

    } else

      return false;

  }



  /**

   * If you expect what you are adding to be near the front, then this

   * may be faster than add depending on how expensive your comparison

   * function is, etc.

   */

  public boolean addNearFront(Object o) {

    if (o instanceof Comparable) {

      Comparable c = (Comparable) o;

      for (int i = 0; i < this.size; ++i) {

        int comp = c.compareTo(this.elements[i]);

        if (comp < 0) {

          this.justInsert(o, i);

          return true;

        } else if (comp == 0)

          return false;

      }

      this.addToEnd(o);

      return true;

    } else

      return false;

  }



  /**

   * Yeah, but this does binary search.

   */

  public boolean contains(Object o) {

    if (o instanceof Comparable) {

      Comparable c = (Comparable) o;

      int mid = this.size / 2;

      // high will always be out of range.

      int high = this.size;

      // low will be the lowest possible point.

      int low = 0;

      do {

        int comp = c.compareTo(this.elements[mid]);

        if (comp == 0)

          return true;

        else if (comp < 0)

          high = mid;

        else

          low = mid + 1;

        mid = (high + low) / 2;

      } while (mid > low);

      return false;

    } else

      return false;

  }



  public Object remove(int index) {

    if (index < 0 || index >= this.size)

      throw new IndexOutOfBoundsException();

    Object value = this.elements[index];

    System.arraycopy(this.elements, index + 1, this.elements, index,

                     (this.size - index) - 1);

    this.size--;

    return value;

  }



  public boolean remove(Object o) {

    if (o instanceof Comparable) {

      Comparable c = (Comparable) o;

      int mid = this.size / 2;

      // high will always be out of range.

      int high = this.size;

      // low will be the lowest possible point.

      int low = 0;

      do {

        int comp = c.compareTo(this.elements[mid]);

        if (comp == 0) {

          this.remove(mid);

          return true;

        } else if (comp < 0)

          high = mid;

        else

          low = mid + 1;

        mid = (high + low) / 2;

      } while (mid > low);

      return false;

    } else

      return false;

  }



  public void clear() {

    for (int i = 0; i < this.size; ++i)

      this.elements[i] = null;

    this.size = 0;

  }



  private void addToEnd(Object o) {

    if (this.size == this.elements.length)

      this.expand();

    this.elements[this.size++] = o;

  }



  private void justInsert(Object o, int index) {

    if (this.size == this.elements.length)

      this.expand();

    this.size++;

    System.arraycopy(this.elements, index, this.elements, index + 1,

                     this.size - index);

    this.elements[index] = o;

  }



  public int size() {

    return this.size;

  }



  public void ensureCapacity(int cap) {

    int expandBy = cap - this.elements.length;

    if (expandBy <= 0)

      return;

    else {

      this.expandBy(expandBy);

    }

  }



  private void expand() {

    this.expandBy(incrementalRellocation);

  }



  private void expandBy(int expandBy) {

    Object[] elts = new Object[this.elements.length + expandBy];

    System.arraycopy(this.elements, 0, elts, 0, this.elements.length);

    this.elements = elts;

  }



  public Object clone() {

    SortedArraySet value = new SortedArraySet(this.elements.length);

    System.arraycopy(this.elements, 0, value.elements, 0,

                     this.elements.length);

//    value.comparator = this.comparator;

    value.size = this.size;

    return value;

  }



  public Object get(int index) {

    if (index >= this.size() || index < 0)

      throw new IndexOutOfBoundsException();

    return this.elements[index];

  }



  public Comparator comparator() {

    return null;                // this.comparator;

  }



  /**

   * TODO: implement this.

   */

  public SortedSet headSet(Object toElement) {

    throw new IllegalArgumentException();
  }



  public Object last() {

    return this.get(this.size() - 1);

  }



  public Object first() {

    return this.get(0);

  }



  /**

   * TODO: implement this.

   */

  public SortedSet subSet(Object fromElement, Object toElement) {

    throw new IllegalArgumentException();
  }



  /**

   * TODO: implement this.

   */

  public SortedSet tailSet(Object fromElement) {

    throw new IllegalArgumentException();
  }



  public Iterator iterator() {

    return new Itr();

  }



  private class Itr implements Iterator {

    /**

     * Index of element to be returned by subsequent call to next.

     */

    int cursor = 0;

    boolean nextCalled = false;



    public boolean hasNext() {

      return cursor != size();

    }



    public Object next() {

      this.nextCalled = true;

      try {

        return SortedArraySet.this.get(cursor++);

      } catch(IndexOutOfBoundsException e) {

        throw new NoSuchElementException();

      }

    }



    public void remove() {

      if (!this.nextCalled)

        throw new IllegalStateException();

      this.nextCalled = false;

      SortedArraySet.this.remove(--this.cursor);

    }

  }



  public SortedArraySet() {

//    this(defaultCapacity, null);

    this(defaultCapacity);

  }



  public SortedArraySet(int capacity) {

//     this(capacity, null);

     this.elements = new Object[capacity];

  }



//   public SortedArraySet(Comparator c) {

//     this(defaultCapacity, c);

//   }



//   public SortedArraySet(int capacity, Comparator c) {

//     this.comparator = c

//     this.element = new Object[capacity];

//   }



}// SortedArraySet



