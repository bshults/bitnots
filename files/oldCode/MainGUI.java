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

public class MainGUI {

  static boolean readyForNext = false;
  static boolean newFileReplacing = false;

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
    
    try {
      final JFrame frame = new JFrame("View Tree");
      frame.setLocation(20, 20);
      final JPanel contents = new JPanel(new BorderLayout()) {
        public void setBounds(int x, int y, int w, int h) {
          super.setBounds(x, y, w, h);
          this.setPreferredSize(new Dimension(w, h));
        }
      };
      
      // TO AUTOMATICALLY LOAD A FILE:
      //   comment out the next eleven lines, uncomment the file of your choice
      //   found below.
      /*
      JFileChooser chooser;
      if (System.getProperties().containsKey("user.dir"))
        chooser = new JFileChooser(System.getProperty("user.dir"));
      else
        chooser = new JFileChooser();
      chooser.addChoosableFileFilter(
            new InputFileFilter("ipr", "Bitnots input file (.ipr)"));
      chooser.addChoosableFileFilter(
            new InputFileFilter("p", "TPTP input file (.p)"));
      int returnVal = chooser.showOpenDialog(frame);
      if (returnVal != JFileChooser.APPROVE_OPTION)
        System.exit(0);
      final IPRParser parser = new IPRParser(
                                  chooser.getSelectedFile().getAbsolutePath());
      */

      // final AbstractParser parser = new Parser("files/epsilontests/basic-fail.ipr");
      // final AbstractParser parser = new Parser("files/epsilontests/basic-test.ipr");
      // final AbstractParser parser = new Parser("files/epsilontests/basic-test2.ipr");
      // final AbstractParser parser = new Parser("files/epsilontests/basic-test3.ipr");
      // final AbstractParser parser = new Parser("files/epsilontests/basic-test4.ipr");
      // final AbstractParser parser = new Parser("files/epsilontests/basic-test5.ipr");
      // final AbstractParser parser = new Parser("files/comprehension1.ipr");
      // final AbstractParser parser = new Parser("files/comprehension2.ipr");
      // final AbstractParser parser = new Parser("files/regularity1.ipr");
      // final AbstractParser parser = new Parser("files/diss/qualified-locally-compact.ipr");
      // final AbstractParser parser = new Parser("files/diss/haus/1-4-4.ipr");
      final AbstractParser parser = new IPRParser("files/formatted/theorems.ipr");
      // final AbstractParser parser = new Parser("files/formatted/out-of-memory.ipr");
      // final AbstractParser parser = new Parser("files/formatted/wos8lemma.ipr");
      // final AbstractParser parser = new Parser("files/formatted/=test.ipr");

      contents.setPreferredSize(new Dimension(600, 400));
      frame.setContentPane(contents);
//      List targets = parser.getAllConjectures();

      for (int i = 0; i < parser.getAllConjectures().size() || newFileReplacing; i++) {
        if (newFileReplacing) {
          i = 0;
//          targets = parser.getAllConjectures();
          newFileReplacing = false;
        }

        Formula currentFormula = ((Conjecture) parser.getAllConjectures().get(i)).getFormula();

        // This needs to be reloaded if a new file is loaded or appended
        Theory theory = parser.getTheory();
        final Tableau tableau = new Tableau(currentFormula, theory);
        tableau.setQLimit(2);
        tableau.setMakeLemmas(true);
        theory.resetSequentTimesApplied();
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

        taskQueue.enqueue(new FOLUnifyTask(tableau));

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
              taskQueue.enqueue(new OneEpsilonTask(tableau));
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
              taskQueue.enqueue(new EpsilonAllTheWayTask(tableau));
              taskQueue.enqueue(new AlertTask(tableau, frame));
            }
          }
        });

        final JCheckBox expand = new JCheckBox(" Tree Auto-Expand");

        JButton refresh = new JButton("Refresh Tree");
        buttons.add(refresh);
        refresh.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            ((DefaultTreeModel) jTree.getModel()).reload();
            if (expand.isSelected()) {
              TreeNode root = (TreeNode) jTree.getModel().getRoot();
              expandAll(jTree, new TreePath(root));
            }
          }
        });

        buttons.add(expand);

        final JButton condense = new JButton("View Condensed");
        buttons.add(condense);
        condense.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              if (jTree.getModel().getRoot() == tableau.getRoot()) {
                if (tableau.getCondensedRoot() == null) {
                  JOptionPane.showMessageDialog(frame,
                                                "No condensed view available!");
                } else {
                  jTree.setModel(new DefaultTreeModel(
                      tableau.getCondensedRoot()));
                  ((DefaultTreeModel) jTree.getModel()).reload();
                  if (expand.isSelected()) {
                    TreeNode root = (TreeNode) jTree.getModel().getRoot();
                    expandAll(jTree, new TreePath(root));
                  }
                  condense.setText("View Full Tree");
                }
              } else {
                jTree.setModel(new DefaultTreeModel(tableau.getRoot()));
                ((DefaultTreeModel) jTree.getModel()).reload();
                if (expand.isSelected()) {
                  TreeNode root = (TreeNode) jTree.getModel().getRoot();
                  expandAll(jTree, new TreePath(root));
                }
                condense.setText("View Condensed");
              }
            }
        });

        JButton next = new JButton("Next Theorem");
        buttons.add(next);
        next.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            Variable.resetSymbolCounter();
            FunctionConstructor.resetSymbolCounter();
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

        class QLimitListener extends JLabel implements PropertyChangeListener
        {
            Tableau tableau;
            QLimitListener(Tableau tableau) {
                super("  Q-limit: " + tableau.getQLimit());
                this.tableau = tableau;
            }

            public void propertyChange(PropertyChangeEvent e) {
                this.setText("  Q-limit: " + e.getNewValue());
            }
        }

        QLimitListener qLimit = new QLimitListener(tableau);
        tableau.addPropertyChangeListener("qLimit", qLimit);
        buttons.add(qLimit);

        JButton decrement = new JButton("Decrement Q-limit");
        buttons.add(decrement);
        decrement.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            if (tableau.getQLimit() == 0)
              frame.getToolkit().beep();
            else
              taskQueue.enqueue(new DecrementQLimitTask(tableau));
          }
        });

        JButton printKB = new JButton("Print Knowledge Base");
        buttons.add(printKB);
        printKB.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            taskQueue.enqueue(new PrintKBTask(tableau));
          }
        });
        
        final JCheckBox replace = new JCheckBox(" Replace on Load");
        replace.setSelected(true);
        
        JButton loadNewFile = new JButton("Open Input File");
        buttons.add(loadNewFile);
        loadNewFile.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            JFileChooser chooser;
            if (System.getProperties().containsKey("user.dir"))
              chooser = new JFileChooser(System.getProperty("user.dir"));
            else
              chooser = new JFileChooser();
            chooser.addChoosableFileFilter(
                new InputFileFilter("ipr", "Bitnots input file (.ipr)"));
            chooser.addChoosableFileFilter(
                new InputFileFilter("p", "TPTP input file (.p)"));
            
            if ((chooser.showOpenDialog(contents)) ==
                                                  JFileChooser.APPROVE_OPTION) {
              try {
                if (replace.isSelected())
                {
                  // Act as if Next Theorem was pushed.
                  parser.parseReplacingWithFile(
                                   chooser.getSelectedFile().getAbsolutePath());
                  Variable.resetSymbolCounter();
                  FunctionConstructor.resetSymbolCounter();
                  MainGUI.newFileReplacing = true;
                  synchronized (frame) {
                    readyForNext = true;
                    frame.notify();
                  }
                }
                else
                  parser.parseAppendingWithFile(
                                   chooser.getSelectedFile().getAbsolutePath());
              } catch (FileNotFoundException fnfe) {
                // This will only be thrown if the file is deleted
                // after being chosen.  Unlikely, but it's still
                // good to check.
              }
            }
          }
        });
        buttons.add(replace);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);

        synchronized (frame) {
          while (!readyForNext)
            frame.wait();
          System.out.println("Out of loop");
          readyForNext = false;
        }
        contents.removeAll();
      }
    } catch (java.util.NoSuchElementException e) {
      e.printStackTrace();
      return;
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      return;
    } catch (InterruptedException e) {
      e.printStackTrace();
      return;
    }
  }

  /**
   * Used to expand the tree when it is refreshed.
   * Code taken from the
   * <a href = http://javaalmanac.com/egs/javax.swing.tree/ExpandAll.html>
   * Java Almanac</a>.
   * @param jTree The tree to expand.
   * @param parent The current path.
   */
  private static void expandAll(JTree jTree, TreePath parent) {
    // Traverse children
    TreeNode node = (TreeNode) parent.getLastPathComponent();
    if (node.getChildCount() >= 0) {
      for (Enumeration e = node.children(); e.hasMoreElements(); ) {
        TreeNode n = (TreeNode) e.nextElement();
        TreePath path = parent.pathByAddingChild(n);
        expandAll(jTree, path);
      }
    }
    jTree.expandPath(parent);
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

} // MainGUI
