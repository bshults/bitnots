package bitnots.tableaux;

import java.util.Comparator;


/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version
 */

public class DescendantComparator implements Comparator {

  // implementation of java.util.Comparator interface

  public boolean equals(Object param1) {
    return param1 instanceof DescendantComparator;
  }

  public int compare(Object param1, Object param2) {
    TableauNode pn1 = (TableauNode) param1;
    TableauNode pn2 = (TableauNode) param2;
    if (pn1 == pn2)
      return 0;
    else if (pn1.isDescentantOf(pn2))
      return -1;
    else
      return 1;
  }
}

