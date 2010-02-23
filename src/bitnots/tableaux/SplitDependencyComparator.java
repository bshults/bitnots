package bitnots.tableaux;

import java.util.*;

/**
 * SplitDependencyComparator.java
 *
 * <p>Compares two TableauFormulas by number
 * of involved splits. A Formula which depends
 * on fewer splits is 'higher' in a tableau
 * and if it can be matched with a theorem from
 * the knowledge base, it will close more branches
 * than a respectively 'lower' Formula.</p>
 *
 * @author Daniel W. Farmer
 * @version 1.0
 */

public class SplitDependencyComparator implements Comparator {

  /**
   * Compares the height of two TableauFormulas. 'High'
   * formulas in a tableau depend on fewer splits than
   * 'low' formulas.
   *
   * @param o1 - the first formula to be compared.
   * @param o2 - the second formula to be compared.
   * @return -1, 0, or 1 as the first formula has fewer,
   * the same number, or more splits involved than the second.
   */
  public int compare(Object o1, Object o2) {
    if (!(o1 instanceof TableauFormula) ||
        !(o2 instanceof TableauFormula)) {
      throw new IllegalArgumentException("Not TableauFormulas!");
    }

    TableauFormula tn1 = (TableauFormula)o1;
    TableauFormula tn2 = (TableauFormula)o2;
    int tn1Splits = (tn1.getInvolvedSplits()).size();
    int tn2Splits = (tn2.getInvolvedSplits()).size();
    if (tn1Splits < tn2Splits)
      return -1;
    else if (tn1Splits == tn2Splits)
      return 0;
    else
      return 1;
  }
}
