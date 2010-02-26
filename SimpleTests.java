import java.util.*;
import java.beans.*;
import java.io.*;
import javax.swing.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.*;
import bitnots.expressions.*;
import bitnots.parse.*;
import bitnots.tableaux.*;
import bitnots.prover.*;
import bitnots.theories.*;
import bitnots.eventthreads.*;

/**
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class SimpleTests {
  
  static boolean readyForNext = false;

  public static void main(String[] args) {
    ClassLoader loader = ClassLoader.getSystemClassLoader();
    try {

      TableauFormula.registerConstructor(
        Predicate.class, NegativePredicate.SIGN,
        NegativePredicate.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
        Truth.class, NegativeTruth.SIGN,
        NegativeTruth.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
        Falsity.class, NegativeFalsity.SIGN,
        NegativeFalsity.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Not.class, NegativeNot.SIGN,
                                         NegativeNot.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Or.class, NegativeOr.SIGN,
                                         NegativeOr.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(And.class, NegativeAnd.SIGN,
                                         NegativeAnd.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Implies.class, NegativeImplies.SIGN,
                                         NegativeImplies.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Iff.class, NegativeIff.SIGN,
                                         NegativeIff.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Forall.class, NegativeForall.SIGN,
                                         NegativeForall.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Forsome.class, NegativeForsome.SIGN,
                                         NegativeForsome.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
        Predicate.class, PositivePredicate.SIGN,
        PositivePredicate.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
        Truth.class, PositiveTruth.SIGN,
        PositiveTruth.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
        Falsity.class, PositiveFalsity.SIGN,
        PositiveFalsity.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Not.class, PositiveNot.SIGN,
                                         PositiveNot.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Or.class, PositiveOr.SIGN,
                                         PositiveOr.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(And.class, PositiveAnd.SIGN,
                                         PositiveAnd.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Implies.class, PositiveImplies.SIGN,
                                         PositiveImplies.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Iff.class, PositiveIff.SIGN,
                                         PositiveIff.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Forall.class, PositiveForall.SIGN,
                                         PositiveForall.class.getConstructor(
                                           new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Forsome.class, PositiveForsome.SIGN,
                                         PositiveForsome.class.getConstructor(
                                           new Class[] {Formula.class}));
    } catch (NoSuchMethodException e) {
      e.printStackTrace();
    }
    
    final JFrame frame = new JFrame("View Tree");
    JPanel contents = new JPanel();
    contents.setPreferredSize(new Dimension(600, 400));
    frame.setContentPane(contents);
    contents.add(new JLabel(UIManager.getIcon("Tree.leafIcon")));
    contents.add(new JLabel(UIManager.getIcon("Tree.closedIcon")));
    contents.add(new JLabel(UIManager.getIcon("Tree.openIcon")));
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.pack();
    frame.show();
  }

  public static void testExpressions() {
    
  }

  public static void testFormulas() {
  }

  public static void testTerms() {
  }

  public static void testSubstitutions() {
  }

  public static void testUnification() {
  }

} // SimpleTests
