package bitnots.theories;

import java.util.*;
import java.io.*;
import bitnots.equality.*;
import bitnots.expressions.*;

/**
 * @author <a href="mailto:b-shults@bethel.edu">Benjamin Shults</a>
 * @author Daniel W. Farmer
 * @version 1.0
 */

// TODO: make this more immutable once it is created.

public class Theory implements Serializable  {

  /** the list of sequents */
  private List<KBSequent> knowledgeBase;

  /** the list of sequents */
  private List<Conjecture> conjectures;

  private DSTGraph congruenceClosure = new DSTGraph();

  /** class member equalities to be handed over to the
   * tableau during its construction
   */
  private ArrayList<Predicate> classMemberEQs = new ArrayList<Predicate>();

  /** Matches formulas to sets of sequents they appear in */
  private HashMap<Formula, Set<KBSequent>> formulas2Sequents = new HashMap<Formula, Set<KBSequent>>();

  public void addToFormulaMap(Formula f, KBSequent kbs) {
    if (this.formulas2Sequents.containsKey(f))
      this.formulas2Sequents.get(f).add(kbs);
    else {
      Set<KBSequent> s = new HashSet<KBSequent>();
      s.add(kbs);
      this.formulas2Sequents.put(f, s);
    }
  }

  public Map<Formula, Set<KBSequent>> getFormulaMap() {
    return this.formulas2Sequents;
  }

  public void addClassMemberEQ(Predicate p) {
    this.classMemberEQs.add(p);
  }

  public List<Predicate> getClassMemberEQs() {
    return this.classMemberEQs;
  }

  public Theory() {
    this.knowledgeBase = new ArrayList<KBSequent>();
    this.conjectures = new ArrayList<Conjecture>();
  }

  public DSTGraph getCongruenceClosure() {
    return this.congruenceClosure;
  }

  /**
   * Adds a sequent to the knowledge base
   * @param s the sequent to be added
   */
  public void addSequent(KBSequent s) {
    this.knowledgeBase.add(s);
    // If s is an equality, it is added to the congruence closure.
    this.congruenceClosure.add(s);
  }

  /**
   * Gets the knowledge base
   * @return the list of sequents in the knowledge base
   */
  public List<KBSequent> getKB() {
    return java.util.Collections.unmodifiableList(this.knowledgeBase);
  }

  /**
   * Adds a conjecture to the theory
   * @param c the conjecture to be added
   */
  public void addConjecture(Conjecture c) {
    this.conjectures.add(c);
  }

  /**
   * Gets the knowledge base
   * @return the list of sequents in the knowledge base
   */
  public List<Conjecture> getConjectures() {
    return java.util.Collections.unmodifiableList(this.conjectures);
  }

  /**
   * Resets the number of times all sequents in this Theory have been
   * applied.
   */
  // TODO: this shouldn't be here.  It should be in the tableau or the 
  // prover or something.
  public void resetSequentTimesApplied() {
    // for (int i = 0; i < knowledgeBase.size(); ++i)
      // TODO: replace this with HashMap reference
      // ((KBSequent) knowledgeBase.get(i)).resetTimesApplied();
  }

  public String toString() {
    String retVal = "\n";
    for (int i = 0; i < this.knowledgeBase.size(); i++) {
      KBSequent currentSequent = this.knowledgeBase.get(i);
      retVal += currentSequent.toString() + "\n";
    }
    return retVal;
  }
}
