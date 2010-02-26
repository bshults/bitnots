package bitnots.gui;

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

public class EqualityTester {
  
  static boolean readyForNext = false;

  public static void main(String[] args) throws
    ClassNotFoundException, InstantiationException, IllegalAccessException,
    UnsupportedLookAndFeelException {

    UIManager.setLookAndFeel(
      UIManager.getSystemLookAndFeelClassName());

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

    OldParser parser;
    TheoryElement currentElement = null;
    try {

      parser = new OldParser("files/=test.ipr");

      final JFrame frame = new JFrame("View Tree");
      frame.setLocation(20, 20);
      JPanel contents = new JPanel(new BorderLayout()) {
          public void setBounds(int x, int y, int w, int h) {
            super.setBounds(x, y, w, h);
            this.setPreferredSize(new Dimension(w, h));
          }
        };
      contents.setPreferredSize(new Dimension(600, 400));
      frame.setContentPane(contents);

      do {
        currentElement = parser.readDefun();
        if (currentElement instanceof Conjecture) {
          Formula currentFormula = ((Conjecture) currentElement).getFormula();
          final Tableau tableau = new Tableau(currentFormula);
          tableau.setQLimit(2);
          System.out.println(tableau);
          final TaskQueue taskQueue = new TaskQueue();
          taskQueue.addUnexpectedExceptionListener(
            new UnexpectedExceptionListener() {
              public void unexpectedEvent(UnexpectedExceptionEvent uee) {
                JOptionPane.showMessageDialog(frame,
                                              "Task: " + uee.getSource() +
                                              "\nhad a problem: " +
                                              uee.getException(),
                                              "Error in task",
                                              JOptionPane.ERROR_MESSAGE);
              }
            });
          taskQueue.enqueue(new FOLEQUnifyTask(tableau));
          final JTree jTree = new JTree(tableau.getRoot());
          contents.add(new JScrollPane(jTree));
          Box buttons = new Box(BoxLayout.Y_AXIS);
          contents.add(buttons, BorderLayout.EAST);
          JButton bcs = new JButton("Branch Closers");
          buttons.add(bcs);
          bcs.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                TreePath path = jTree.getSelectionPath();
                if (path != null) {
                  TableauNode node =
                    (TableauNode) path.getLastPathComponent();
                  Iterator bcs = node.getBranchClosers().iterator();
                  while (bcs.hasNext()) {
                    System.out.println(bcs.next());
                  }
                }
              }
            });
          JButton step = new JButton("Step");
          buttons.add(step);
          step.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                if (tableau.getCloser() != null)
                  JOptionPane.showMessageDialog(frame,
                                                "Done: " + tableau.getCloser());
                else
                  taskQueue.enqueue(new FOLEQUnifyTask(tableau));
              }
            });
          JButton finish = new JButton("Finish");
          buttons.add(finish);
          finish.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                if (tableau.getCloser() != null)
                  JOptionPane.showMessageDialog(frame,
                                                "Done: " + tableau.getCloser());
                else {
                  taskQueue.enqueue(new FOLEQToQLimitTask(tableau));
                  taskQueue.enqueue(new AlertTask(tableau, frame));
                }
              }
            });
          JButton refresh = new JButton("Refresh Tree");
          buttons.add(refresh);
          refresh.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                ((DefaultTreeModel) jTree.getModel()).reload();
              }
            });
          JButton next = new JButton("Next Theorem");
          buttons.add(next);
          next.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                synchronized (frame) {
                  readyForNext = true;
                  frame.notify();
                }
              }
            });
          JButton increment = new JButton("Increment Q-limit");
          buttons.add(increment);
          increment.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                taskQueue.enqueue(new IncrementQLimitTask(tableau));
              
              }
            });

          class QLimitListener extends JLabel implements PropertyChangeListener {
            Tableau tableau;
            QLimitListener(Tableau tableau) {
              super("Q-limit: " + tableau.getQLimit());
              this.tableau = tableau;
            }
            public void propertyChange(PropertyChangeEvent e) {
              this.setText("Q-limit: " + e.getNewValue());
            }
          }
          QLimitListener qLimit = new QLimitListener(tableau);
          tableau.addPropertyChangeListener("qLimit", qLimit);
          buttons.add(qLimit);

          JButton decrement = new JButton("Decrement Q-limit");
          buttons.add(decrement);
          decrement.addActionListener(new ActionListener() {
              public void actionPerformed(ActionEvent e) {
                taskQueue.enqueue(new DecrementQLimitTask(tableau));
              }
            });
          frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
          frame.pack();
          frame.setVisible(true);
          synchronized (frame) {
            while (!readyForNext)
              frame.wait();
            readyForNext = false;
          }
          contents.removeAll();
        }
      } while (parser.hasNextToken());
    } catch (java.util.NoSuchElementException e) {
      e.printStackTrace();
      return;
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      return;
    } catch (InterruptedException e) {
      e.printStackTrace();
      return;
    } catch (WrongNonterminalException e) {
      System.out.println(e.getMessage());
      e.printStackTrace();
      return;
    } catch (EOFException e) {
      System.out.println("Hooray!  Reached end of file!");
      return;
    } catch (IOException e) {
      e.printStackTrace();
      return;
    } catch (NestedBindingException nbe) {
      nbe.printStackTrace();
      return;
    }
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

} // EqualityTester
