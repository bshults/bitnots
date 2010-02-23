package bitnots.util;

import java.util.SortedSet;

public interface CloneableSortedSet extends Cloneable, SortedSet {
  public Object clone();
}
