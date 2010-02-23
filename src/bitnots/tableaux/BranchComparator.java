package bitnots.tableaux;

import bitnots.expressions.*;
import java.io.*;
import java.util.*;

/**
 * BranchComparator.java
 *
 * <p>Compares two branches in a tree and ranks them by
 * number of branch closers for the sake of epsilon-rule
 * application. The TableauNodes that are passed in must
 * be leaves of the tree in order to represent unique
 * branches.</p>
 *
 * @author Benjamin Shults
 * @author Daniel W. Farmer
 * @version 1.0
 */
public class BranchComparator
    implements Comparator {

  /** Maps each branch (represented by its leaf node) to its rating */
  private HashMap<TableauNode, Double> branchRatings;

  /** Initializes Comparator to remember branch ratings */
  public BranchComparator() {
    this.branchRatings = new HashMap<TableauNode, Double>();
  }

  /**
   * Compares the epsilon rating of two branches, represented
   * uniquely by the leaf nodes at the bottom of the branch.
   * The greater the rank, the easier it is to close that
   * branch, and conversely, the lower the rank, the harder
   * it is to close that branch.
   * Note: Likely candidates for application of the epsilon
   * rule will be branches with low (or 0) rank.
   *
   * @param o1 - the first branch to be compared.
   * @param o2 - the second branch to be compared.
   * @return -1, 0, or 1 as the first branch's rank is
   * less than, equal to, or greater than the second.
   */
  public int compare(Object o1, Object o2) {

    // get references to the nodes
    TableauNode b1 = (TableauNode) o1;
    TableauNode b2 = (TableauNode) o2;

    // b1 and b2 must be leaves to represent unique branches
    if (!b1.isLeaf() || !b2.isLeaf()) {
      throw new IllegalArgumentException("Node is not a leaf");
    }

    double b1Value = this.getRating(b1);   // branch 1's rating
    double b2Value = this.getRating(b2);   // branch 2's rating

    // compare the two branches
    if (b1Value < b2Value) {
      // higher ranked branches should go first.
      return 1;
    }
    else if (b1Value == b2Value) {
      return 0;
    }
    else {
      return -1;
    }
  }

  /**
   * Calculates the epsilon rule rating of a given branch.  Higher rating
   * means the branch probably needs more help.
   * Goes through the branch closers on every node of the
   * branch (represented uniquely by its leaf), and calculates
   * 10^(-|bc|) where |bc| is the number of pairs in the
   * branch closing substitution.
   * Each branch's rank is the sum of this ratio for each
   * of its branch closers. The greater the rank, the easier
   * it is to close that branch, and conversely, the lower
   * the rank, the harder it is to close that branch.
   * Note: Likely candidates for application of the epsilon
   * rule will be branches with low (or 0) rank.
   *
   * @todo consider making the base of the exponent related to the number of
   * splits above the BC.  Or perhaps multiply the exponent by the number
   * of splits.
   * @param tn the leaf node at the bottom of the branch
   * @return this branch's rating
   */
  private double getRating(TableauNode tn) {

    double returnVal = 0;   // this branch's rating

    Double tempRating;  // used to get a rating from the hash
    tempRating = this.branchRatings.get(tn); // check hash

    if (tempRating == null) {

      List<BranchCloser> bcs;  // the current list of branch closers

      // calculate rating for this branch
      while (tn != null) {
        bcs = tn.getBranchClosers();

        if (bcs.isEmpty()) {
          tn = tn.getParent();  // move up the tree
          continue;
        }

        // go through each branch closer and calculate
        // 10^(-|bc|) where |bc| is the number of pairs
        // in the branch closing substitution - each
        // branch's rank is the sum of this ratio for
        // each of its branch closers
        for (int i = 0; i < bcs.size(); i++) {
          returnVal += Math.pow(10, -bcs.get(i).getSubst().size());
        }

        if (tn.getParent() != null) {
          tn = tn.getParent(); // move up the tree
        }
        else {
          break;  // done
        }
      }
      // remember this branch's rating
      this.branchRatings.put(tn, Double.valueOf(returnVal));
    }
    else {    // rating is already in memory
      returnVal = tempRating.doubleValue();
    }
    return returnVal;   // return this branch's rank
  }
}
