package bitnots.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import bitnots.prover.EpsilonAllTheWayTask;
import bitnots.prover.SomeEpsilonsTask;
import bitnots.tableaux.BranchCloser;
import bitnots.tableaux.Tableau;
import bitnots.tableaux.TableauFormula;
import bitnots.tableaux.TableauNode;
import bitnots.theories.TheoremApplication;
import bitnots.util.ImageUtils;
import javax.swing.JSplitPane;

/**
 * This is a source of the bound property: node.
 * @author bshults
 *
 */
public class BranchView extends JPanel implements PropertyChangeListener {

  protected TableauNode branch;
  private JTextField branchIDTF = new JTextField(40);
  private BranchViewTablesPane tables;
  private BranchClosersView bcView = new BranchClosersView();
  private TheoremAppView taView = new TheoremAppView();
  
  @Override
  public Dimension getPreferredSize() {
    return new Dimension(800, 600);
  }

  private void setNode(TableauNode node) {
    TableauNode oldValue = this.branch;
    this.branch = node;
    this.firePropertyChange("node", oldValue, node);
  }
  
  /**
   * 
   * @return Returns the branch.
   */
  public TableauNode getBranch() {
    return this.branch;
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    if (evt.getPropertyName().equals("tableau")) {
      Bitnots frame = (Bitnots) evt.getSource();
      if (frame.getTableau().getRoot() != this.branch.getTableau().getRoot()) {
        this.setNode(frame.getTableau().getRoot());
      }
    }
  }

  /**
   * Displays the first branch of <code>tableau</code>.
   * 
   * @param tableau
   * @throws NullPointerException if <code>tableau</code> is null or empty.
   */
  public BranchView(Tableau tableau) {
    this(tableau.getRoot());
  }

  /**
   * Display info about <code>node</code>.
   * 
   * @param node the TableauNode to display.
   * @throws NullPointerException if <code>node</code> is null.
   */
  public BranchView(TableauNode node) {
    this.branch = node;
    this.addPropertyChangeListener("node", new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        BranchView.this.branchIDTF.setText(BranchView.this.branch.getPathString());
        BranchView.this.tables.resetModels(BranchView.this.branch);
        Object[] values = BranchView.this.branch.getBranchClosers().toArray();
        BranchView.this.bcView.setListData(values);
        Object[] tas = BranchView.this.branch.getTheoremApplications().toArray();
        BranchView.this.taView.setListData(tas);
      }
    });

    this.taView.addListSelectionListener(new ListSelectionListener() {
      @Override
      public void valueChanged(ListSelectionEvent e) {
        // highlight the involved formulas in the various tables.
        TheoremApplication ta =
          (TheoremApplication) BranchView.this.taView.getSelectedValue();
        BranchView.this.tables.negAtoms.clearSelection();
        BranchView.this.tables.posAtoms.clearSelection();
        if (ta != null) {
          for (TableauFormula form: ta.getBranchFormulas()) {
            int index =
                ((AtomNegativeTableModel)
                BranchView.this.tables.negAtoms.getModel()).data.indexOf(form);
            if (index >= 0)
              BranchView.this.tables.negAtoms.changeSelection(index, 0, true, false);
            else {
              index =
                ((AtomPositiveTableModel)
                BranchView.this.tables.posAtoms.getModel()).data.indexOf(form);
              if (index >= 0)
                BranchView.this.tables.posAtoms.changeSelection(index, 0, true, false);
            }
          }
        }
      }
    });
    // TODO: add an option to apply the selected branch closer and clean up
    // the tree.

    this.bcView.addListSelectionListener(new ListSelectionListener() {
      @Override
      public void valueChanged(ListSelectionEvent e) {
        // highlight the involved formulas in the various tables.
        BranchCloser bc = 
          (BranchCloser) BranchView.this.bcView.getSelectedValue();
        BranchView.this.tables.negAtoms.clearSelection();
        BranchView.this.tables.posAtoms.clearSelection();
        if (bc != null && (bc.getSequents() == null || bc.getSequents().isEmpty())) {
          for (TableauFormula goal: bc.getGoals()) {
            BranchView.this.tables.negAtoms.changeSelection(
                    ((AtomNegativeTableModel) BranchView.this.tables.negAtoms.getModel()).data.indexOf(goal),
                    0, false, false);
          }
          for (TableauFormula hyp: bc.getHyps()) {
            BranchView.this.tables.posAtoms.changeSelection(
                    ((AtomPositiveTableModel) BranchView.this.tables.posAtoms.getModel()).data.indexOf(hyp),
                    0, false, false);
          }
        } else {
          // TODO highlight formulas involved in the sequent application.
        }
      }
    });
    
    this.setLayout(new BorderLayout());

    // NORTH: Branch ID
    JPanel north = new JPanel(new FlowLayout(FlowLayout.LEFT));
    north.add(new JLabel("Branch ID:"));
    north.add(this.branchIDTF);
    this.add(north, BorderLayout.NORTH);

    // CENTER: branch view

    this.tables = new BranchViewTablesPane(this.branch); 
    this.add(this.tables);

    // SOUTH: controls
    
    JPanel south = new JPanel();
    JPanel controls = new JPanel();
    this.add(south, BorderLayout.SOUTH);
    south.add(controls);
    
    // JMenu tasks = new JMenu("Tasks");
    
    final JButton goButton = new JButton(Bitnots.FRAME.getGoAction());
    goButton.setHorizontalAlignment(SwingConstants.LEFT);
    
    // add a Go button whose action is customizable by a right-click
    // popup menu
    
    final JPopupMenu popup = new JPopupMenu();
    // TODO: give these actions: change the action and string
    // of Bitnots.FRAME.goAction
    // these should also take the specified action.
    JMenuItem step = new JMenuItem("Step");
    step.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent arg0) {
        Bitnots.FRAME.getGoAction().change("Step", new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            if (Bitnots.FRAME.getTableau().getCloser() != null)
              JOptionPane.showMessageDialog(Bitnots.FRAME,
                                            "Done: " + Bitnots.FRAME.getTableau().getCloser());
            else
              Bitnots.taskQueue.executeWithPrompt(new SomeEpsilonsTask(Bitnots.FRAME.getTableau()));
          }
        });
        Bitnots.FRAME.getGoAction().actionPerformed(arg0);
      }});
    popup.add(step);
    JMenuItem finish = new JMenuItem("Finish");
    finish.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent arg0) {
        Bitnots.FRAME.getGoAction().change("Finish", new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            if (Bitnots.FRAME.getTableau().getCloser() != null)
              JOptionPane.showMessageDialog(Bitnots.FRAME,
                                            "Done: " + Bitnots.FRAME.getTableau().getCloser());
            else
              Bitnots.taskQueue.executeWithPrompt(new EpsilonAllTheWayTask(Bitnots.FRAME.getTableau()));
          }
        });
        Bitnots.FRAME.getGoAction().actionPerformed(arg0);
      }});
    popup.add(finish);
    
    goButton.addMouseListener(new PopupListener(popup));
    
    // TODO add View TheoremApps, 

    // TODO set this button as a listener to PropertyChangeEvent on Q-limit
    // so that it can always display the current Q-limit.
    JButton qLimitButton = new JButton(new AbstractAction("Q-Limit") {
      @Override
      public void actionPerformed(ActionEvent arg0) {
        while (true) {
          String input =
              JOptionPane.showInputDialog(Bitnots.FRAME,
                                          "Please enter the new Q-Limit:",
                                          Bitnots.FRAME.getTableau().getQLimit());
          if (input == null || "".equals(input))
            return;
          final int newQ;
          try {
            newQ = Integer.parseInt(input);
          } catch (NumberFormatException cce) {
            JOptionPane.showMessageDialog(Bitnots.FRAME,
                                          "Invalid input - Please try again.");
            continue;
          }
          if (newQ < 0) {
            JOptionPane.showMessageDialog(Bitnots.FRAME,
                              "Invalid input - Please try again.");
            continue;
          }
          Bitnots.taskQueue.executeWithPrompt(new Runnable() {
            public void run() {
              Bitnots.FRAME.getTableau().setQLimit(newQ);
            }
          });
          break;
        }
      }
    });
    controls.add(qLimitButton);

    // Popup menu to increment or decrement Q-limit
    final JPopupMenu qPopup = new JPopupMenu();
    // TODO: give these actions: change the action and string
    // of Bitnots.FRAME.goAction
    // these should also take the specified action.
    JMenuItem inc = new JMenuItem("+1");
    inc.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent arg0) {
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            Bitnots.FRAME.getTableau().setQLimit(Bitnots.FRAME.getTableau().getQLimit() + 1);
          }
        });
      }});
    JMenuItem dec = new JMenuItem("-1");
    inc.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent arg0) {
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            Bitnots.FRAME.getTableau().setQLimit(Bitnots.FRAME.getTableau().getQLimit() - 1);
          }});
      }});
    qPopup.add(inc);
    qPopup.add(dec);
    
    qLimitButton.addMouseListener(new PopupListener(qPopup));
    
    JButton condenseButton = new JButton(new AbstractAction("Condense") {
      @Override
      public void actionPerformed(ActionEvent e) {
        if (JOptionPane.showConfirmDialog(Bitnots.FRAME,
                                          "This will prune parts of the tree and cannot be undone.  Do you want to continue?",
                                          "Are you sure?",
                                          JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
          Bitnots.taskQueue.executeWithPrompt(new Runnable() {
            public void run() {
              Bitnots.FRAME.getTableau().condense();
            }
          });
        }
      }});
    controls.add(condenseButton);
    controls.add(goButton);
    
    // TODO add stop or pause button
    
    // WEST: navigation
   
    JPanel west = new JPanel(new BorderLayout());
    JPanel navigation = new JPanel(new GridLayout(6, 2, 5, 5));
    navigation.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    
    this.add(west, BorderLayout.WEST);
    west.add(navigation, BorderLayout.NORTH);
    
    // TODO: consider checking for Shift key depressed and, if so, do everything
    // on the AWT thread.
    
    JButton parent = 
      new JButton(new AbstractAction("Up", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/Parent.png")), 
                                                               20, 20)) {
      @Override
        public void actionPerformed(ActionEvent e) {
          Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          @Override
            public void run() {
              final TableauNode parent = BranchView.this.branch.getParent();
              SwingUtilities.invokeLater(new Runnable() {
              @Override
                public void run() {
                  if (parent != null) {
                    BranchView.this.setNode(parent);
                    BranchView.this.tables.resetModels(parent);
                  }
                }});
            }});
        }});
    parent.setHorizontalAlignment(SwingConstants.LEFT);
    JButton child = 
      new JButton(new AbstractAction("Down",
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/FirstChild.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) {//"First Child");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            if (!BranchView.this.branch.isLeaf()) {
              final TableauNode child = BranchView.this.branch.getChildAt(0);
              SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                  BranchView.this.setNode(child);
                }});
            }}});
      }
    });
    child.setHorizontalAlignment(SwingConstants.LEFT);
    JButton nextSib = 
      new JButton(new AbstractAction("Sib", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/NextSib.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "Next Sibling");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            if (BranchView.this.branch.getParent() != null) {
              int index = BranchView.this.branch.getIndex();
              if (index < BranchView.this.branch.getParent().getChildCount() - 1) {
                final TableauNode sib = BranchView.this.branch.getParent().getChildAt(index + 1);
                SwingUtilities.invokeLater(new Runnable() {
                  public void run() {
                    BranchView.this.setNode(sib);
                  }
                });
              }}}});
      }});
    nextSib.setHorizontalAlignment(SwingConstants.LEFT);
    JButton prevSib =
      new JButton(new AbstractAction("Sib", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/PrevSib.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "Previous Sibling");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            if (BranchView.this.branch.getParent() != null) {
              int index = BranchView.this.branch.getIndex();
              if (index > 0) {
                final TableauNode sib = BranchView.this.branch.getParent().getChildAt(index - 1);
                SwingUtilities.invokeLater(new Runnable() {
                  public void run() {
                    BranchView.this.setNode(sib);
                  }
                });
              }}
          }
        });
      }});
    prevSib.setHorizontalAlignment(SwingConstants.LEFT);
    JButton nextBranch = 
      new JButton(new AbstractAction("Branch", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/NextLeaf.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "Next Leaf");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            final TableauNode branch = BranchView.this.branch.getNextLeaf();
            if (branch != null) {
              SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                  BranchView.this.setNode(branch);
                }
              });
            }
          }
        });
      }});
    nextBranch.setHorizontalAlignment(SwingConstants.LEFT);
    JButton prevBranch = 
      new JButton(new AbstractAction("Branch", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/PrevLeaf.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "Previous Leaf");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            final TableauNode branch = BranchView.this.branch.getPrevLeaf();
            if (branch != null) {
              SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                  BranchView.this.setNode(branch);
                }
              });
            }
          }
        });
      }
      });
    prevBranch.setHorizontalAlignment(SwingConstants.LEFT);
    JButton firstBranch = 
      new JButton(new AbstractAction("Branch", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/FirstLeaf.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "First Leaf");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            final TableauNode branch = BranchView.this.branch.getTableau().getFirstLeaf();
            if (branch != null) {
              SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                  BranchView.this.setNode(branch);
                }
              });
            }
          }
        });
      }});
    firstBranch.setHorizontalAlignment(SwingConstants.LEFT);
    JButton lastBranch = 
      new JButton(new AbstractAction("Branch", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/LastLeaf.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "Last Leaf");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            final TableauNode branch = BranchView.this.branch.getTableau().getLastLeaf();
            if (branch != null) {
              SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                  BranchView.this.setNode(branch);
                }
              });
            }
          }});
      }});
    lastBranch.setHorizontalAlignment(SwingConstants.LEFT);
    JButton firstSib = 
      new JButton(new AbstractAction("Sib", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/FirstSib.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "First Sib");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            if (BranchView.this.branch.getParent() != null) {
              final TableauNode branch = BranchView.this.branch.getParent().getChildAt(0);
              if (branch != BranchView.this.branch) {
                SwingUtilities.invokeLater(new Runnable() {
                  public void run() {
                    BranchView.this.setNode(branch);
                  }
                });
              }
            }
          }});
      }});
    firstSib.setHorizontalAlignment(SwingConstants.LEFT);
    JButton lastSib = 
      new JButton(new AbstractAction("Sib", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/LastSib.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "Last Sib");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            if (BranchView.this.branch.getParent() != null) {
              final TableauNode branch = 
                BranchView.this.branch.getParent().getChildAt(BranchView.this.branch.getParent().getChildCount() - 1);
              if (branch != BranchView.this.branch) {
                SwingUtilities.invokeLater(new Runnable() {
                  public void run() {
                    BranchView.this.setNode(branch);
                  }
                });
              }
            }
          }
        });
      }});
    lastSib.setHorizontalAlignment(SwingConstants.LEFT);
    JButton goToRoot = 
      new JButton(new AbstractAction("Root", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/Root.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "Root");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            final TableauNode branch = BranchView.this.branch.getTableau().getRoot();
            if (branch != BranchView.this.branch) {
              SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                  BranchView.this.setNode(branch);
                }
              });
            }
          }
        });
      }});
    goToRoot.setHorizontalAlignment(SwingConstants.LEFT);
   
    navigation.add(goToRoot);
    JButton refresh = 
      new JButton(new AbstractAction("Refresh", 
                                     ImageUtils.scaleImageIcon(new ImageIcon(this.getClass().getResource("/bitnots/resources/Refresh.png")), 
                                                               20, 20)) {
      public void actionPerformed(ActionEvent e) { // "Refresh");
        Bitnots.taskQueue.executeWithPrompt(new Runnable() {
          public void run() {
            // FIXME implement refresh
            // TODO get current branch label. (or maybe find the node with these new formulas?)
            // TODO find the node with tha label.
            // TODO load that node
            // TODO force refresh
          }
        });
      }});
    goToRoot.setHorizontalAlignment(SwingConstants.LEFT);
   
    navigation.add(goToRoot);
    navigation.add(new JPanel());
    navigation.add(parent);
    navigation.add(child);
    navigation.add(prevSib);
    navigation.add(nextSib);
    navigation.add(firstSib);
    navigation.add(lastSib);
    navigation.add(prevBranch);
    navigation.add(nextBranch);
    navigation.add(firstBranch);
    navigation.add(lastBranch);
    
    // west.add(new JPanel());

    JSplitPane westCenter = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
    west.add(westCenter);
    JPanel branchClosers = new JPanel(new BorderLayout());
    branchClosers.add(new JScrollPane(this.bcView));
    JPanel branchClosersTop = new JPanel();
    branchClosersTop.add(new JLabel("Branch Closers"));
    branchClosers.add(branchClosersTop, BorderLayout.NORTH);
    JPanel westBottom = new JPanel(new BorderLayout());
    westBottom.add(new JLabel("Theorem Applications"), BorderLayout.NORTH);
    westBottom.add(new JScrollPane(this.taView));
    westCenter.add(branchClosers);
    westCenter.add(westBottom);
    west.add(westBottom, BorderLayout.SOUTH);
    
    Bitnots.FRAME.addPropertyChangeListener("tableau", this);
  }

}
