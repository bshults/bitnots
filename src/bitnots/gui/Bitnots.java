package bitnots.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JSplitPane;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.WindowConstants;

import bitnots.expressions.And;
import bitnots.expressions.Falsity;
import bitnots.expressions.Forall;
import bitnots.expressions.Formula;
import bitnots.expressions.Forsome;
import bitnots.expressions.Iff;
import bitnots.expressions.Implies;
import bitnots.expressions.Not;
import bitnots.expressions.Or;
import bitnots.expressions.Predicate;
import bitnots.expressions.Truth;
import bitnots.prover.SomeEpsilonsTask;
import bitnots.tableaux.NegativeAnd;
import bitnots.tableaux.NegativeFalsity;
import bitnots.tableaux.NegativeForall;
import bitnots.tableaux.NegativeForsome;
import bitnots.tableaux.NegativeIff;
import bitnots.tableaux.NegativeImplies;
import bitnots.tableaux.NegativeNot;
import bitnots.tableaux.NegativeOr;
import bitnots.tableaux.NegativePredicate;
import bitnots.tableaux.NegativeTruth;
import bitnots.tableaux.PositiveAnd;
import bitnots.tableaux.PositiveFalsity;
import bitnots.tableaux.PositiveForall;
import bitnots.tableaux.PositiveForsome;
import bitnots.tableaux.PositiveIff;
import bitnots.tableaux.PositiveImplies;
import bitnots.tableaux.PositiveNot;
import bitnots.tableaux.PositiveOr;
import bitnots.tableaux.PositivePredicate;
import bitnots.tableaux.PositiveTruth;
import bitnots.tableaux.Tableau;
import bitnots.tableaux.TableauFormula;
import bitnots.test.Test;
import bitnots.theories.Theory;


/**
 * This is a source of the following bound properties: "tableau" and "theory".
 * @author bshults
 *
 */
public class Bitnots extends JFrame {

  public static final int DEFAULT_Q_LIMIT = 0;
  
  public static Bitnots FRAME;

  public static final ConfirmingExecutorService taskQueue =
    new ConfirmingExecutorService();
  
  /**
   * 
   * @param args
   * @throws ClassNotFoundException 
   * @throws InstantiationException 
   * @throws IllegalAccessException 
   * @throws UnsupportedLookAndFeelException 
   */
  public static void main(String[] args) throws ClassNotFoundException,
  InstantiationException, IllegalAccessException,
  UnsupportedLookAndFeelException {
    
    // set up LAF
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    
    // load classes
    Bitnots.loadClasses();
    
    Bitnots.FRAME = new Bitnots();
    
    Bitnots.FRAME.init();
    
    // set up and display frame
    Bitnots.FRAME.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    Bitnots.FRAME.pack();
    Bitnots.FRAME.setVisible(true);
  }
  
  public static void loadClasses() {
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
    }
    catch (NoSuchMethodException e) {
      e.printStackTrace();
    }
  }

  // TODO ensure that tableau and theory data are not touched from the
  // AWT thread and ensure that GUI data is not touched from my TaskQueue.
  
  private Theory currentTheory;
  
  private Tableau currentTableau;

  // TODO: create a table of possible value of this keyed on the names.
  private GoActionListener goAction;
  
  public Theory getTheory() {
    return this.currentTheory;
  }
  
  public void setTheory(Theory t) {
    Theory old = this.currentTheory;
    this.currentTheory = t;
    this.firePropertyChange("theory", old, t);
  }
  
  public Tableau getTableau() {
    return this.currentTableau;
  }
  
  public void setTableau(Tableau t) {
    Tableau old = this.currentTableau;
    this.currentTableau = t;
    this.firePropertyChange("tableau", old, t);
  }
  
  public GoActionListener getGoAction() {
    return this.goAction;
  }

  private void init() {
    // load a theory from a file
    
    // TODO set up menus to load a theory file.
    
    // TODO write a wrapper around taskQueue.enqueue that checks to see if
    // the thread is busy and, if so, enqueues the runnable with some
    // additional code to 1) the runnable to notify the user and prompts
    // about continuing to wait, 2) notify the waiting dialog when it
    // is dequeued.
    
    // TODO: Figure out synchronization between the threads and consider
    // putting more code on the engine thread.
    
    // TODO switch to java.util.concurrent classes for event thread:
    // Execurors.newSingleThreadExecutor():ExecutorService
    // Have a single object that increments its counter when something
    // is enqueued and decrements it when a task is finished.
    // Then I can check this number to know if the thread is busy.

    // FIXME add menu to load new file.
    
    this.goAction =
      new GoActionListener("Step", new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          if (Bitnots.this.getTableau().getCloser() != null)
            JOptionPane.showMessageDialog(Bitnots.this,
                                          "Done: " + Bitnots.this.getTableau().getCloser());
          else
            Bitnots.taskQueue.executeWithPrompt(new SomeEpsilonsTask(Bitnots.this.getTableau()));
          Test.validateAncestorPath(Bitnots.this.currentTableau.getRoot().getFirstLeaf());
        }});
    this.goAction.putValue(Action.NAME, "Go");

    this.currentTheory = TheoryLoader.loadDefaultFile();

    // create and set up the initial tableau
    Formula currentFormula = 
      this.currentTheory.getConjectures().iterator().next().getFormula();
    this.currentTableau = new Tableau(currentFormula, this.currentTheory);
    // this.currentTableau.setQLimit(2);
    this.currentTableau.setMakeLemmas(true);
    // TODO: this needs to change places.
    this.currentTheory.resetSequentTimesApplied();

    JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                                          new BranchView(this.currentTableau),
                                          new TheoryView(this.currentTheory));
    splitPane.setResizeWeight(.7);
    this.add(splitPane);
  }
  
  public Bitnots() {
    super("Bitnots");
  }
  
}
