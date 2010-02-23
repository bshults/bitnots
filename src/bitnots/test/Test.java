package bitnots.test;

import bitnots.expressions.Formula;
import bitnots.gui.Bitnots;
import bitnots.gui.TheoryLoader;
import bitnots.prover.EpsilonAllTheWayTask;
import bitnots.tableaux.Tableau;
import bitnots.tableaux.TableauNode;
import bitnots.theories.Conjecture;
import bitnots.theories.Theory;


/**
 * 
 * @author bshults
 *
 */
public class Test {
  
  public static void main(String... args) {
    Bitnots.loadClasses();
    Test.testQ1Theorems();
    Test.testNonTheorems();
    System.out.println();
    System.out.println("Starting challenges");
    System.out.println();
    Test.testChallenges();
    System.out.println();
    System.out.println("Starting Q = 2");
    System.out.println();
    Test.testQ2Theorems();
    System.out.println();
    System.out.println("Starting Q = 3");
    System.out.println();
    Test.testToughTheorem();
  }

  public static void testQ1Theorems() {
    Theory theory = TheoryLoader.getTheoryFromFile("files/bitnots/q1Theorems.ipr");
    for (Conjecture conj: theory.getConjectures()) {
      Formula currentFormula = conj.getFormula();
      System.out.println(currentFormula);
      Tableau tableau = new Tableau(currentFormula, theory);
      tableau.setMakeLemmas(true);
      if (tableau.getCloser() == null)
        new EpsilonAllTheWayTask(tableau).run();
      if (tableau.getCloser() == null)
        throw new Error("Failed to prove Q1: " + currentFormula);
      System.out.println(tableau.getCloser());
    }
  }
  
  public static void testChallenges() {
    Theory theory = TheoryLoader.getTheoryFromFile("files/bitnots/challenge-q=1.ipr");
    for (Conjecture conj: theory.getConjectures()) {
      Formula currentFormula = conj.getFormula();
      System.out.println(currentFormula);
      Tableau tableau = new Tableau(currentFormula, theory);
      tableau.setMakeLemmas(true);
      if (tableau.getCloser() == null)
        new EpsilonAllTheWayTask(tableau).run();
      if (tableau.getCloser() == null)
        throw new Error("Failed to prove Challenge: " + currentFormula);
      System.out.println(tableau.getCloser());
    }
    theory = TheoryLoader.getTheoryFromFile("files/bitnots/challenge-q=2.ipr");
    for (Conjecture conj: theory.getConjectures()) {
      Formula currentFormula = conj.getFormula();
      System.out.println(currentFormula);
      Tableau tableau = new Tableau(currentFormula, theory);
      tableau.setQLimit(2);
      tableau.setMakeLemmas(true);
      if (tableau.getCloser() == null)
        new EpsilonAllTheWayTask(tableau).run();
      if (tableau.getCloser() == null)
        throw new Error("Failed to prove Challenge: " + currentFormula);
      System.out.println(tableau.getCloser());
    }
    theory = TheoryLoader.getTheoryFromFile("files/bitnots/challenge-q=3.ipr");
    for (Conjecture conj: theory.getConjectures()) {
      Formula currentFormula = conj.getFormula();
      System.out.println(currentFormula);
      Tableau tableau = new Tableau(currentFormula, theory);
      tableau.setQLimit(3);
      tableau.setMakeLemmas(true);
      if (tableau.getCloser() == null)
        new EpsilonAllTheWayTask(tableau).run();
      if (tableau.getCloser() == null)
        throw new Error("Failed to prove Challenge: " + currentFormula);
      System.out.println(tableau.getCloser());
    }
    theory = TheoryLoader.getTheoryFromFile("files/bitnots/challenge-q=4.ipr");
    for (Conjecture conj: theory.getConjectures()) {
      Formula currentFormula = conj.getFormula();
      System.out.println(currentFormula);
      Tableau tableau = new Tableau(currentFormula, theory);
      tableau.setQLimit(4);
      tableau.setMakeLemmas(true);
      if (tableau.getCloser() == null)
        new EpsilonAllTheWayTask(tableau).run();
      if (tableau.getCloser() == null)
        throw new Error("Failed to prove Challenge: " + currentFormula);
      System.out.println(tableau.getCloser());
    }
  }
  
  public static void testToughTheorem() {
    Theory theory = TheoryLoader.getTheoryFromFile("files/bitnots/out-of-memory.ipr");
    for (Conjecture conj: theory.getConjectures()) {
      Formula currentFormula = conj.getFormula();
      System.out.println(currentFormula);
      Tableau tableau = new Tableau(currentFormula, theory);
      tableau.setQLimit(3);
      tableau.setMakeLemmas(true);
      if (tableau.getCloser() == null)
        new EpsilonAllTheWayTask(tableau).run();
      if (tableau.getCloser() == null)
        throw new Error("Failed to prove out-of-memory: " + currentFormula);
      System.out.println(tableau.getCloser());
    }
  }
  
  public static void testQ2Theorems() {
    Theory theory = TheoryLoader.getTheoryFromFile("files/bitnots/q2Theorems.ipr");
    for (Conjecture conj: theory.getConjectures()) {
      Formula currentFormula = conj.getFormula();
      System.out.println(currentFormula);
      Tableau tableau = new Tableau(currentFormula, theory);
      tableau.setQLimit(2);
      tableau.setMakeLemmas(true);
      if (tableau.getCloser() == null)
        new EpsilonAllTheWayTask(tableau).run();
      if (tableau.getCloser() == null)
        throw new Error("Failed to prove Q2: " + currentFormula);
      System.out.println(tableau.getCloser());
    }
  }
  
  public static void testNonTheorems() {
    Theory theory = TheoryLoader.getTheoryFromFile("files/bitnots/non-theorems.ipr");
    for (Conjecture conj: theory.getConjectures()) {
      Formula currentFormula = conj.getFormula();
      System.out.println(currentFormula);
      Tableau tableau = new Tableau(currentFormula, theory);
      // tableau.setQLimit(2);
      tableau.setMakeLemmas(true);
      if (tableau.getCloser() == null)
        new EpsilonAllTheWayTask(tableau).run();
      if (tableau.getCloser() != null)
        throw new Error("Proved Non-Theorem: " + currentFormula);
    }
  }
  
  public static final void validateAncestorPath(TableauNode t) {
    // TableauNode t = Bitnots.FRAME.getTableau().getRoot();
    TableauNode parent = t.getParent();
    if (parent != null)
      if (parent.getChildren().indexOf(t) == -1) {
        System.out.println("Error");
      } else
        Test.validateAncestorPath(parent);
  }

  public static boolean parentChildRelations(TableauNode tableauNode) {
    if (tableauNode.getParent() != null)
      if (tableauNode.getParent().getChildren().indexOf(tableauNode) == -1)
        return false;
    for (TableauNode child: tableauNode.getChildren()) {
      if (child.getParent() != tableauNode)
        return false;
    }
    return true;
  }
}
