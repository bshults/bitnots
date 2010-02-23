package bitnots.parse;

import bitnots.expressions.*;
import bitnots.prover.*;
import bitnots.tableaux.*;
import bitnots.theories.*;

import java.util.*;

/**
 * The ReduceSequentTask class is an AbstractTableauTask, able to be
 * run on a TaskQueue.  This task takes a Tableau, a Theory, and one
 * or two Strings (a name and a format String).  It reduces the Tableau
 * using alpha, beta, and delta inverse rules.  After that, it searches
 * each branch for unused formulas which are formed into KBSequents,
 * and then placed in the Theory object.
 *
 * @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
 * @version 1.0
 */

// TODO: most of this shouldn't be in this package.  Move parts of this 
// to the theory and the prover packages.

public class ReduceSequentTask extends AbstractTableauTask {
  /**
   * The Theory object where we will insert sequents.
   */
  private Theory theory;

  /**
   * The name of the theory element that will be attached to all
   * generated sequents.
   */
  private String name;

  /**
   * The format of the theory element that will be attached to all
   * generated sequents.
   */
  private String format;

  /**
   * Creates a ReduceSequentTask, used to run on the given Tableau
   * and then inserting the resulting sequents into the given Theory
   * object.
   * @param tab The tableau to reduce.
   * @param theory The Theory object where we will insert sequents.
   * @param name The name that will be attached to all generated
   * sequents.
   */
  public ReduceSequentTask(Tableau tab, Theory theory, String name) {
    this(tab, theory, name, new String());
  }

  /**
   * Creates a ReduceSequentTask, used to run on the given Tableau
   * and then inserting the resulting sequents into the given Theory
   * object.
   * @param tab The tableau to reduce.
   * @param the The Theory object where we will insert sequents.
   * @param name The name that will be attached to all generated
   * sequents.
   * @param format The format that will be attached to all generated
   * sequents.
   */
  public ReduceSequentTask(Tableau tab, Theory theory,
                           String name, String format) {
    super(tab);
    this.theory = theory;
    this.name = name;
    this.format = format;
  }

  /**
   * Runs the AllAlphasTask, OneBetaTask, and the
   * AllDeltaInversesTask as many times as needed to reduce the
   * formula down.  Then the tableau is broken down by branch and
   * entered as KBSequents into a Theory object.
   */
  public void run() {
    Prover.expandKBSequent(this.tableau);

    // iterate through the branches, generating a KBSequent for
    // each one.
    Iterator<TableauNode> branchIter = this.tableau.undoneLeaves().iterator();
    while (branchIter.hasNext()) {
      KBSequent sequent = new KBSequent();
      sequent.setName(this.name);
      sequent.setFormat(this.format);

      TableauNode leafNode = branchIter.next();

      // Add negative formulas
      Iterator goalIter = leafNode.getAllUnusedNegatives().iterator();
      while (goalIter.hasNext()) {
        TableauFormula tf = (TableauFormula) goalIter.next();
        ReduceSequentTask.this.checkTFforTriggers(tf);
        sequent.addNegative(tf.getFormula());
        theory.addToFormulaMap(tf.getFormula(), sequent);
      }

      // Add positive formulas
      Iterator hypIter = leafNode.getAllUnusedPositives().iterator();
      while (hypIter.hasNext()) {
        TableauFormula tf = (TableauFormula) hypIter.next();
        ReduceSequentTask.this.checkTFforTriggers(tf);
        sequent.addPositive(tf.getFormula());
        theory.addToFormulaMap(tf.getFormula(), sequent);
      }
      this.theory.addSequent(sequent);
    }
  }

  /**
   * This method takes a TableauFormula, and if it represents
   * an equality predicate where one of the arguments is a
   * class member term, it stores that predicate inside the theory,
   * to be later delivered to the tableau in which it will be used.
   * @param tf The TableauFormula to check for potential triggers.
   * @author Daniel W. Farmer
   */
  public void checkTFforTriggers(TableauFormula tf) {

    Formula formula = tf.getFormula();
    if (!(formula instanceof Predicate))
      return;

    Predicate p = (Predicate) formula;

    if (p.getConstructor().toString().equals("=")) {

      Iterator terms = p.arguments();
      Term t1 = (Term) terms.next();
      Term t2 = (Term) terms.next();

      if (t1 instanceof ComplexTerm) {
        ComplexTerm ct1 = (ComplexTerm) t1;
        if (ct1.getConstructor().getName().equals("the-class-of-all"))
          this.theory.addClassMemberEQ(p);
      }
      if (t2 instanceof ComplexTerm) {
        ComplexTerm ct2 = (ComplexTerm) t2;
        if (ct2.getConstructor().getName().equals("the-class-of-all"))
          this.theory.addClassMemberEQ(p);
      }
    }
  }
}
