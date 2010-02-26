package bitnots.gui;

import java.util.ArrayList;
import java.util.Iterator;
import java.beans.*;
import java.io.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import bitnots.expressions.*;
import bitnots.parse.*;
import bitnots.tableaux.*;
import bitnots.prover.*;
import bitnots.theories.*;
import bitnots.eventthreads.*;
import javax.swing.event.*;

/**
 * This version of MainGUI tries to be closer to the ultimate
 * goal that we have for this program.  It includes a menu system
 * (complete with about box!) and an easy-to-navigate user
 * interface that uses tabs and split panes.  If there is a
 * knowledge base, it is visible on the left of the workspace,
 * but if there is none, in an effort to make an efficient use
 * of space, the layout is slightly different in order to maximize
 * JTree visibility.
 *
 * @author <a href="mailto:b-shults@bethel.edu">Benjamin Shults</a>
 * @author Daniel W. Farmer
 * @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
 * @version .9
 */

public class MenuMainGUI {

  public static InputFileFilter bitnotsFilter = new InputFileFilter(
                                  "ipr", "Bitnots input file (.ipr)");
  public static InputFileFilter tptpFilter = new InputFileFilter(
                                  "p", "TPTP input file (.p)");

  static boolean readyForNext = false;
  static boolean guiNeedsRebuild = true;
  static int currentTarget = 0;

  /** A reference to the current selected node */
  static TableauNode currentNode = null;

  /** The tableau's default q-limit */
  static int DEFAULT_Q_LIMIT = 2;

  public static void main(String[] args) throws
      ClassNotFoundException, InstantiationException, IllegalAccessException,
      UnsupportedLookAndFeelException {

    UIManager.setLookAndFeel(
        UIManager.getSystemLookAndFeelClassName());

    ClassLoader loader = ClassLoader.getSystemClassLoader();
    MenuMainGUI.loadClasses();

    try {
      // create the main frame
      final JFrame frame = new JFrame("Bitnots");
      frame.setLocation(50, 50);
      final PropertyChangeSupport pcs = new PropertyChangeSupport(frame);

      // have the user choose a file on load
      JFileChooser chooser = (System.getProperties().containsKey("user.dir")) ?
                             new JFileChooser(System.getProperty("user.dir")) :
                             new JFileChooser();
      chooser.addChoosableFileFilter(MenuMainGUI.bitnotsFilter);
      chooser.addChoosableFileFilter(MenuMainGUI.tptpFilter);
      chooser.setFileFilter(MenuMainGUI.bitnotsFilter);
      int returnVal = chooser.showOpenDialog(frame);
      if (returnVal != JFileChooser.APPROVE_OPTION)
        System.exit(0);
      final IPRParser parser = new IPRParser(
          chooser.getSelectedFile().getAbsolutePath());

      while (MenuMainGUI.guiNeedsRebuild) {
        MenuMainGUI.guiNeedsRebuild = false;
        
        JPanel contents = new JPanel(new BorderLayout()) {
          public void setBounds(int x, int y, int w, int h) {
            super.setBounds(x, y, w, h);
            this.setPreferredSize(new Dimension(w, h));
          }
        };
        contents.setPreferredSize(new Dimension(600, 400));
        frame.setContentPane(contents);


        //
        // Begin menu structure code
        //
        MenuBar myMenuBar = new MenuBar();
        Menu file = new Menu("File");
        Menu proof = new Menu("Proof");
        Menu tools = new Menu("Tools");
        Menu about = new Menu("About");
  
        final MenuItem open = new MenuItem("Open...",
                              new MenuShortcut(KeyEvent.VK_O));
        final MenuItem openAndReplace = new MenuItem("Open and Replace...",
                                        new MenuShortcut(KeyEvent.VK_O, true));
        final MenuItem quit = new MenuItem("Quit");
        file.add(open);
        file.add(openAndReplace);
        file.addSeparator();
        file.add(quit);
  
        final MenuItem refresh = new MenuItem("Refresh Tree",
                                 new MenuShortcut(KeyEvent.VK_R));
        final MenuItem step = new MenuItem("Step",
                              new MenuShortcut(KeyEvent.VK_F5));
        final MenuItem stepAndRefresh = new MenuItem("Step and Refresh",
                                        new MenuShortcut(KeyEvent.VK_F6));
        final MenuItem takeNSteps = new MenuItem("Take n Steps...");
        final MenuItem finish = new MenuItem("Finish",
                                new MenuShortcut(KeyEvent.VK_F7));
        final MenuItem finishAndRefresh = new MenuItem("Finish and Refresh",
                                          new MenuShortcut(KeyEvent.VK_F8));
        final MenuItem condensedView = new MenuItem("Condensed View");
        final MenuItem nextTheorem = new MenuItem("Next Theorem",
                                     new MenuShortcut(KeyEvent.VK_F9));
        proof.add(refresh);
        proof.add(step);
        proof.add(stepAndRefresh);
        proof.add(takeNSteps);
        proof.add(finish);
        proof.add(finishAndRefresh);
        proof.addSeparator();
        proof.add(condensedView);
        proof.addSeparator();
        proof.add(nextTheorem);
  
        final MenuItem viewBCs = new MenuItem("View Branch Closers");
        final MenuItem viewTAs = new MenuItem("View Theorem Applications");
        final MenuItem editQ = new MenuItem("Edit Q-Limit");
        tools.add(viewBCs);
        tools.add(viewTAs);
        tools.addSeparator();
        tools.add(editQ);
  
        final MenuItem aboutBitnots = new MenuItem("About Bitnots...");
        about.add(aboutBitnots);
  
        myMenuBar.add(file);
        myMenuBar.add(proof);
        myMenuBar.add(tools);
        myMenuBar.add(about);
  
        frame.setMenuBar(myMenuBar);
  
        //
        // End menu structure code
        //
  
        // use the parser to initialize conjectures and knowledge base
        Theory theory = parser.getTheory();
        final boolean hasKB = !theory.getKB().isEmpty();
  
        //
        // Begin GUI layout code
        //
  
        JPanel buttons; // contains the primary function buttons
        JSplitPane splitPane = null; // only used if there is a knowledge base
        if (!hasKB)
          buttons = new JPanel();
        else {
          buttons = new JPanel(new BorderLayout());
          final JTextArea kbArea = new JTextArea(); // contains the KB
          kbArea.setEditable(false);
          kbArea.setText(theory.toString());
          JPanel kbPanel = new JPanel(new BorderLayout());
          JLabel header =
              new JLabel("<html><b>Knowledge Base:</b></html",
                         SwingConstants.CENTER);
          kbPanel.add(header, BorderLayout.NORTH);
          kbPanel.add(new JScrollPane(kbArea), BorderLayout.CENTER);
          kbPanel.setMinimumSize(new Dimension(175, 0));
  
          // create a split pane for better KB visualization
          splitPane =
              new JSplitPane(JSplitPane.VERTICAL_SPLIT, buttons, kbPanel);
          splitPane.setOneTouchExpandable(false);
          splitPane.setDividerLocation(175);
        }
        buttons.setMinimumSize(new Dimension(175, 35));
  
        // used to view all unused hyps and goals
        final JLabel nodeViewLabel = new JLabel("", JLabel.CENTER);
        // used to view only hyps and goals at the current node
        final JLabel nodeViewLabel2 = new JLabel("", JLabel.CENTER);
        // used to switch between the two above JLabels
        final JTabbedPane nodeViews = new JTabbedPane(JTabbedPane.BOTTOM);
        nodeViews.addTab("View All Unused Formulas", null,
                         new JScrollPane(nodeViewLabel),
                         "click here to see all unused formulas at this node");
        nodeViews.addTab("View Node's Formulas", null,
                         new JScrollPane(nodeViewLabel2),
                         "click here to see only this node's formulas");
        nodeViews.setBorder(new BevelBorder(BevelBorder.LOWERED));
        // the split pane to contain the two above views
        final JSplitPane nodeViewSplitPane =
            new JSplitPane(JSplitPane.VERTICAL_SPLIT, nodeViews, new JPanel());
  
        // set up node view panel with tabbed pane and navigation buttons
        JPanel nodePanel = new JPanel(new BorderLayout());
        nodeViewSplitPane.setDividerLocation(300);
        nodePanel.add(nodeViewSplitPane, BorderLayout.CENTER);
        JPanel navButtons = new JPanel();
        nodePanel.add(navButtons, BorderLayout.NORTH);
  
        // navigation buttons for the node view
        final JButton parent = new JButton("Parent");
        navButtons.add(parent);
        final JButton firstChild = new JButton("1st Child");
        navButtons.add(firstChild);
        final JButton nextSib = new JButton("Next Sibling");
        navButtons.add(nextSib);
        final JButton prevSib = new JButton("Previous Sibling");
        navButtons.add(prevSib);
        final JButton getBCs = new JButton("View Branch Closers");
        navButtons.add(getBCs);
        final JButton getTAs = new JButton("View Theorem Applications");
        if (hasKB) // (no KB = no TAs)
          navButtons.add(getTAs);
  
          // the main display (shows either the tree view or the node view)
        final JTabbedPane rightPane = new JTabbedPane();
        rightPane.addTab("Tree View", null, new JScrollPane(),
                         "click here to view the full tree");
        rightPane.addTab("Node View", null, nodePanel,
                         "click here to navigate through the tree node by node");
  
        // the main split pane
        // (between the display and the primary proof buttons)
        JSplitPane mainSplitPane;
        if (!hasKB) {
          mainSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
                                         buttons, rightPane);
          mainSplitPane.setDividerLocation(40);
        } else {
          mainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                                         splitPane, rightPane);
          mainSplitPane.setDividerLocation(200);
        }
        mainSplitPane.setOneTouchExpandable(false);
        contents.add(mainSplitPane);
  
        // Q-limit tools
        JPanel qlPanel = new JPanel();
        JButton increment = new JButton("+");
        qlPanel.add(increment);
        final JButton decrement = new JButton("-");
        qlPanel.add(decrement);
  
        // the name of the current target
        final JLabel theoremName = new JLabel("", SwingConstants.CENTER);
        if (hasKB) {
          theoremName.setBorder(new EtchedBorder());
          buttons.add(theoremName, BorderLayout.NORTH);
        } else
          buttons.add(theoremName);
  
          // the main tool panel
        JPanel toolPanel = new JPanel();
        if (hasKB)
          toolPanel.setBorder(new EtchedBorder());
        JButton sr = new JButton("Step & Refresh");
        JButton fr = new JButton("Finish & Refresh");
        JButton nt = new JButton("Next Theorem >>");
        toolPanel.add(sr);
        toolPanel.add(fr);
        toolPanel.add(nt);
        if (hasKB)
          buttons.add(toolPanel, BorderLayout.CENTER);
        else
          buttons.add(toolPanel);
  
          /** Label that keeps track of the current q-limit. */
        class QLimitListener extends JLabel implements PropertyChangeListener {
          Tableau tableau;
          TaskQueue tq;
          QLimitListener() {
            super("  Q-limit: " + DEFAULT_Q_LIMIT);
            this.tableau = null;
          }
  
          QLimitListener(Tableau tableau) {
            super("  Q-limit: " + tableau.getQLimit());
            this.tableau = tableau;
          }
  
          public void propertyChange(PropertyChangeEvent e) {
            if (e.getPropertyName().equals("qLimit")) {
              if (this.tableau == null)
                return;
              if (tableau.getQLimit() < 1)
                decrement.setEnabled(false);
              else
                decrement.setEnabled(true);
              this.setText("  Q-limit: " + e.getNewValue());
            }
          }
  
          public void reset(Tableau tab) {
            tableau = tab;
            // update gui by causing event to be fired
            tableau.setQLimit(DEFAULT_Q_LIMIT);
          }
        }
  
        QLimitListener qLimit = new QLimitListener();
        if (hasKB) {
          qlPanel.setBorder(new EtchedBorder());
          buttons.add(qlPanel, BorderLayout.SOUTH);
        } else
          buttons.add(qlPanel);
        qlPanel.add(qLimit);
  
        /** Label that keeps track of the number of times regularity
         * has been enforced.*/
        class RegularityEnforcedListener extends JLabel implements ActionListener {
          int timesEnforced;
          RegularityEnforcedListener() {
            super("Regularity enforced 0 time(s)");
            timesEnforced = 0;
          }
  
          public void actionPerformed(ActionEvent e) {
            ++timesEnforced;
            this.setText("Regularity enforced " + timesEnforced + " time(s)");
          }
  
          public void reset() {
            timesEnforced = 0;
            this.setText("Regularity enforced 0 time(s)");
          }
        }
  
        RegularityEnforcedListener rel = new RegularityEnforcedListener();
        toolPanel.add(rel);
  
        //
        // End GUI layout code
        //
  
        //
        // Begin invariant ActionListener code
        //
        
        quit.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            System.exit(0); // TODO: make this un-dangerous
          }
        });
  
        aboutBitnots.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            String about = new String();
            about += "Bitnots Automated Theorem Prover\n";
            about += "Copyright (c) 2004 - Benjamin Price Shults\n\n";
            about += " Design: Benjamin P. Shults, PhD.\n\n";
            about += " Contributors:\n";
            about += "   - Jacob X (Western Carolina University)\n";
            about += "   - Pete Wall (Bethel University)\n";
            about += "   - Daniel Farmer (Bethel University)\n";
            about += "   - Jonas Bambi (Bethel University)";
            JOptionPane.showMessageDialog(frame, about);
          }
        });
  
        //
        // End invariant ActionListener code
        //
        
        // THE LOOP (goes through each of the input file's targets)
        for (; MenuMainGUI.currentTarget < parser.numberOfTargets();
                                            MenuMainGUI.currentTarget++) {
          
          if (MenuMainGUI.guiNeedsRebuild)
            break;
  
          // update the target
          Conjecture currentElement =(Conjecture) parser.getAllConjectures()
                                              .get(MenuMainGUI.currentTarget);
          Formula currentFormula = currentElement.getFormula();
  
          // set up the tableau
          final Tableau tableau = new Tableau(currentFormula, theory);
          tableau.setQLimit(DEFAULT_Q_LIMIT);
          tableau.setMakeLemmas(true);
          currentNode = tableau.getRoot();
          theory.resetSequentTimesApplied();
          System.out.println(tableau);
  
          // create the task queue
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
  
          // create the visible tree
          final JTree jTree = new JTree(tableau.getRoot());
          if (rightPane.getSelectedIndex() == 0)
            rightPane.setComponentAt(0, new JScrollPane(jTree));
          else
            nodeViewSplitPane.setBottomComponent(new JScrollPane(jTree));
  
            //
            // Begin ActionListener code
            //
  
            // code to view branch closers at a given node
          ActionListener bcListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              if (currentNode == null)
                return;
              if (currentNode.getBranchClosers().isEmpty()) {
                JOptionPane.showMessageDialog(frame, "No Branch Closers");
              } else {
                Iterator bcs = currentNode.getBranchClosers().iterator();
                String message = new String("<html><ul>\n");
                while (bcs.hasNext()) {
                  message += "<li>" + (BranchCloser) bcs.next() + "</li>\n";
                }
                message += "</ul></html>";
                JOptionPane.showMessageDialog(frame, new JLabel(message));
              }
            }
          };
          viewBCs.addActionListener(bcListener);
  
          // code to view TheoremApplications at a given node
          ActionListener taListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              synchronized (currentNode) {
                if (currentNode == null)
                  return;
                if (currentNode.getTheoremApplications() == null ||
                    currentNode.getTheoremApplications().isEmpty()) {
                  JOptionPane.showMessageDialog(frame, "No Theorem Applications");
                } else {
                  Iterator tas = currentNode.getTheoremApplications().iterator();
                  String message = new String("<html><ul>\n");
                  while (tas.hasNext()) {
                    message += "<li>" + (TheoremApplication) tas.next() +
                        "</li>\n";
                  }
                  message += "</ul></html>";
                  JScrollPane taDisplay = new JScrollPane(new JLabel(message));
                  taDisplay.setPreferredSize(new Dimension(450, 350));
                  JOptionPane.showMessageDialog(null, taDisplay,
                                                "Theorem Applications",
                                                JOptionPane.INFORMATION_MESSAGE);
                }
              }
            }
          };
          viewTAs.addActionListener(taListener);
  
          // code to edit the q-limit
          ActionListener qListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              while (true) {
                String input =
                    JOptionPane.showInputDialog(frame,
                                                "Please enter the new Q-Limit:");
                if (input == null || input == "")
                  return;
                Integer newQ;
                try {
                  newQ = new Integer(input);
                } catch (NumberFormatException cce) {
                  JOptionPane.showMessageDialog(frame,
                                                "Invalid input - Please try again.");
                  continue;
                }
                if (newQ.intValue() < 0) {
                  JOptionPane.showMessageDialog(frame,
                                    "Invalid input - Please try again.");
                  continue;
                }
                tableau.setQLimit(newQ.intValue());
                break;
              }
            }
          };
          editQ.addActionListener(qListener);
  
          // one step with no refresh
          ActionListener sListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              if (tableau.getCloser() != null)
                JOptionPane.showMessageDialog(frame,
                                              "Done: " + tableau.getCloser());
              else
                taskQueue.enqueue(new OneEpsilonTask(tableau));
            }
          };
          step.addActionListener(sListener);
  
          // one step with refresh
          ActionListener srListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              if (tableau.getCloser() != null)
                JOptionPane.showMessageDialog(frame,
                                              "Done: " + tableau.getCloser());
              else {
                taskQueue.enqueue(new OneEpsilonTask(tableau));
                taskQueue.enqueue(new RefreshTreeTask(tableau, jTree));
              }
            }
          };
          stepAndRefresh.addActionListener(srListener);
  
          // finish with no refresh
          ActionListener fListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              if (tableau.getCloser() != null)
                JOptionPane.showMessageDialog(frame,
                                              "Done: " + tableau.getCloser());
              else {
                taskQueue.enqueue(new EpsilonAllTheWayTask(tableau));
                taskQueue.enqueue(new AlertTask(tableau, frame));
              }
            }
          };
          finish.addActionListener(fListener);
  
          // finish with refresh
          ActionListener frListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              if (tableau.getCloser() != null)
                JOptionPane.showMessageDialog(frame,
                                              "Done: " + tableau.getCloser());
              else {
                taskQueue.enqueue(new EpsilonAllTheWayTask(tableau));
                taskQueue.enqueue(new AlertTask(tableau, frame));
                taskQueue.enqueue(new RefreshTreeTask(tableau, jTree));
              }
            }
          };
          finishAndRefresh.addActionListener(frListener);
  
          // refresh tree
          ActionListener rListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              taskQueue.enqueue(new RefreshTreeTask(tableau, jTree));
            }
          };
          refresh.addActionListener(rListener);
  
          // code to view the condensed (or full) tree
          ActionListener cvListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              if (jTree.getModel().getRoot() == tableau.getRoot()) {
                if (tableau.getCondensedRoot() == null) {
                  JOptionPane.showMessageDialog(frame,
                                                "No condensed view available!");
                } else {
                  jTree.setModel(new DefaultTreeModel(
                      tableau.getCondensedRoot()));
                  taskQueue.enqueue(new RefreshTreeTask(tableau, jTree));
                  condensedView.setLabel("Full View");
                }
              } else {
                jTree.setModel(new DefaultTreeModel(tableau.getRoot()));
                taskQueue.enqueue(new RefreshTreeTask(tableau, jTree));
                condensedView.setLabel("Condensed View");
              }
            }
          };
          condensedView.addActionListener(cvListener);
  
          // code to move on to the next theorem
          ActionListener ntListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              condensedView.setLabel("Condensed View");
              if (MenuMainGUI.currentTarget == parser.numberOfTargets() - 2) {
                nextTheorem.setEnabled(false);
              }
              Variable.resetSymbolCounter();
              FunctionConstructor.resetSymbolCounter();
              synchronized (frame) {
                readyForNext = true;
                frame.notify();
              }
            }
          };
          nextTheorem.addActionListener(ntListener);
  
          // q-limit modification listeners
          ActionListener incrementListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              taskQueue.enqueue(new IncrementQLimitTask(tableau));
            }
          };
          increment.addActionListener(incrementListener);
          ActionListener decrementListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              taskQueue.enqueue(new DecrementQLimitTask(tableau));
            }
          };
          decrement.addActionListener(decrementListener);
  
          // checks on the current node's state when the user goes
          // from one view to another
          ChangeListener viewListener = new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
              if (rightPane.getSelectedIndex() == 0)
                rightPane.setComponentAt(0, new JScrollPane(jTree));
              else
                nodeViewSplitPane.setBottomComponent(new JScrollPane(jTree));
              pcs.firePropertyChange("current node", null, null);
            }
          };
          rightPane.addChangeListener(viewListener);
  
          // this updates the current node based on the user's
          // current selection in the JTree
          TreeSelectionListener treeListener = new TreeSelectionListener() {
            public void valueChanged(TreeSelectionEvent tse) {
              currentNode = (TableauNode) tse.getPath().getLastPathComponent();
              pcs.firePropertyChange("current node", null, null);
            }
          };
          jTree.addTreeSelectionListener(treeListener);
  
          // node view navigation button listeners
          ActionListener parentListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              currentNode = (TableauNode) currentNode.getParent();
              pcs.firePropertyChange("current node", null, null);
            }
          };
          parent.addActionListener(parentListener);
  
          ActionListener firstChildListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              currentNode = (TableauNode) currentNode.getChildAt(0);
              pcs.firePropertyChange("current node", null, null);
            }
          };
          firstChild.addActionListener(firstChildListener);
  
          ActionListener nextSibListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              currentNode = (TableauNode) currentNode.getParent().getChildAt(
                  currentNode.getIndex() + 1);
              pcs.firePropertyChange("current node", null, null);
            }
          };
          nextSib.addActionListener(nextSibListener);
  
          ActionListener prevSibListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              currentNode = (TableauNode) currentNode.getParent().getChildAt(
                  currentNode.getIndex() - 1);
              pcs.firePropertyChange("current node", null, null);
            }
          };
          prevSib.addActionListener(prevSibListener);
  
          // open new files...
          ActionListener openAndReplaceListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              JFileChooser chooser =
                               (System.getProperties().containsKey("user.dir")) ?
                               new JFileChooser(System.getProperty("user.dir")) :
                               new JFileChooser();
              chooser.addChoosableFileFilter(MenuMainGUI.bitnotsFilter);
              chooser.addChoosableFileFilter(MenuMainGUI.tptpFilter);
              chooser.setFileFilter(MenuMainGUI.bitnotsFilter);
              int returnVal = chooser.showOpenDialog(frame);
              if (returnVal == JFileChooser.APPROVE_OPTION) {
                try {
                  parser.parseReplacingWithFile(
                            chooser.getSelectedFile().getAbsolutePath());
                  // Act as if Next Theorem was pushed.
                  if (MenuMainGUI.currentTarget == parser.numberOfTargets() - 2)
                    nextTheorem.setEnabled(false);
                  Variable.resetSymbolCounter();
                  FunctionConstructor.resetSymbolCounter();
                  boolean willHaveKB = !parser.getTheory().getKB().isEmpty();
                  if (hasKB != willHaveKB)
                    MenuMainGUI.guiNeedsRebuild = true;
                  MenuMainGUI.currentTarget = 0;
                  synchronized (frame) {
                    readyForNext = true;
                    frame.notify();
                  }
                } catch (FileNotFoundException fnfe) {
                  // This will only be thrown if the file is deleted
                  // after being selected.  Unlikely, but it's good to
                  // check, still.
                }
              }
            }
          };
          openAndReplace.addActionListener(openAndReplaceListener);
  
          ActionListener openListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              JFileChooser chooser =
                               (System.getProperties().containsKey("user.dir")) ?
                               new JFileChooser(System.getProperty("user.dir")) :
                               new JFileChooser();
              chooser.addChoosableFileFilter(MenuMainGUI.bitnotsFilter);
              chooser.addChoosableFileFilter(MenuMainGUI.tptpFilter);
              chooser.setFileFilter(MenuMainGUI.bitnotsFilter);
              int returnVal = chooser.showOpenDialog(frame);
              if (returnVal == JFileChooser.APPROVE_OPTION) {
                try {
                  parser.parseAppendingWithFile(
                            chooser.getSelectedFile().getAbsolutePath());
                  boolean willHaveKB = !parser.getTheory().getKB().isEmpty();
                  if (!hasKB && willHaveKB)
                    MenuMainGUI.guiNeedsRebuild = true;
                  MenuMainGUI.currentTarget -= 1;
                  synchronized (frame) {
                    readyForNext = true;
                    frame.notify();
                  }
                } catch (FileNotFoundException fnfe) {
                  // This will only be thrown if the file is deleted
                  // after being selected.  Unlikely, but it's good to
                  // check, still.
                }
              }
            }
          };
          open.addActionListener(openListener);
  
          // code to take n steps into the proof (and refresh)
          ActionListener nStepsListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              int num = 0;
              // get a good number from the user
              while (true) {
                String input = JOptionPane.showInputDialog(frame,
                    "Please enter the number of steps you would like to take:");
                if (input == null || input == "")
                  return;
                try {
                  num = (new Integer(input)).intValue();
                  if (num < 0)
                    throw new NumberFormatException();
                  break;
                } catch (NumberFormatException nfe) {
                  JOptionPane.showMessageDialog(frame, "Invalid input");
                }
              }
              // take that number of steps into the proof
              for (int i = 0; i < num; ++i) {
                if (tableau.getCloser() != null) {
                  JOptionPane.showMessageDialog(frame,
                                                "Done: " + tableau.getCloser());
                  break;
                } else
                  taskQueue.enqueue(new OneEpsilonTask(tableau));
              }
              taskQueue.enqueue(new RefreshTreeTask(tableau, jTree));
            }
          };
          takeNSteps.addActionListener(nStepsListener);
  
          getBCs.addActionListener(bcListener);
          getTAs.addActionListener(taListener);
          sr.addActionListener(srListener);
          fr.addActionListener(frListener);
  
          if (MenuMainGUI.currentTarget == parser.numberOfTargets() - 1)
            nt.setEnabled(false);
          else
            nt.addActionListener(ntListener);
  
          rel.reset();
          tableau.addActionListener(rel);
  
          //
          // End ActionListener code
          //
  
          // whevener the current node is changed, this updates
          // what buttons are enabled and disabled as well as the
          // current node display
  
          pcs.addPropertyChangeListener(new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent e) {
              if (!e.getPropertyName().equals("current node"))
                return;
  
              parent.setEnabled(currentNode.getParent() != null);
              firstChild.setEnabled(!currentNode.getChildren().isEmpty());
              nextSib.setEnabled(!(currentNode.getIndex() == -1 ||
                  currentNode.getIndex() ==
                      (currentNode.getParent().getChildCount() - 1)));
              prevSib.setEnabled(!(currentNode.getIndex() <= 0));
  
                // find the current path with all necessary TreeNodes
              ArrayList pathArray = new ArrayList();
              TableauNode tempNode = currentNode;
              do {
                pathArray.add(0, tempNode);
              } while ((tempNode = (TableauNode) tempNode.getParent()) != null);
              TreePath currentPath = new TreePath(pathArray.toArray());
              int row = jTree.getRowForPath(currentPath);
  
              // update selected node in the JTree
              jTree.setSelectionRow(row);
  
              // update node contents display
              nodeViewLabel.setText(currentNode.toHTMLString(true));
              nodeViewLabel2.setText(currentNode.toHTMLString(false));
            }
          });
  
          // update the target name
          theoremName.setText("Current target: " + currentElement.getName());
  
          // disable next theorem menu item if there's only one target
          if (MenuMainGUI.currentTarget == parser.numberOfTargets() - 1)
            nextTheorem.setEnabled(false);
  
            // reset the q-limit listener
          qLimit.reset(tableau);
          tableau.addPropertyChangeListener("qLimit", qLimit);
          // this is kludgey, but it works to refresh the gui
          taskQueue.enqueue(new IncrementQLimitTask(tableau));
          taskQueue.enqueue(new DecrementQLimitTask(tableau));
  
          // set up the JTree by setting the current node
          pcs.firePropertyChange("current node", null, null);
  
          // set up the window and make it visible
          frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
          frame.pack();
          frame.setVisible(true);
  
          synchronized (frame) {
            while (!readyForNext)
              frame.wait();
            readyForNext = false;
          }
  
          // GUI components created outside of the main loop
          // need to have their action listeners removed at
          // the end of the loop so that they don't end up
          // with multiple ones.
          viewBCs.removeActionListener(bcListener);
          viewTAs.removeActionListener(taListener);
          getBCs.removeActionListener(bcListener);
          getTAs.removeActionListener(taListener);
          editQ.removeActionListener(qListener);
          step.removeActionListener(sListener);
          stepAndRefresh.removeActionListener(srListener);
          finish.removeActionListener(fListener);
          finishAndRefresh.removeActionListener(frListener);
          increment.removeActionListener(incrementListener);
          decrement.removeActionListener(decrementListener);
          rightPane.removeChangeListener(viewListener);
          jTree.removeTreeSelectionListener(treeListener);
          parent.removeActionListener(parentListener);
          firstChild.removeActionListener(firstChildListener);
          nextSib.removeActionListener(nextSibListener);
          prevSib.removeActionListener(prevSibListener);
          sr.removeActionListener(srListener);
          fr.removeActionListener(frListener);
          condensedView.removeActionListener(cvListener);
          nextTheorem.removeActionListener(ntListener);
          takeNSteps.removeActionListener(nStepsListener);
          open.removeActionListener(openListener);
          openAndReplace.removeActionListener(openAndReplaceListener);
        }
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
   *
   */
  private static void loadClasses() {
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
} // MenuMainGUI
