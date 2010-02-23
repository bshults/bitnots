package bitnots.prover;

import java.util.*;
import bitnots.tableaux.*;
import bitnots.expressions.*;

/**
 * Collection of static methods for manipulating tableaux.
 */

public class Prover {

  //  private Tableau tableau;

  /**
   * Unless it is interrupted, this expands all class member
   * predicate formulas.
   * This relies on these formulas being added to the end.
   * Returns true if there was something to do.
   */
  public static boolean expandClassMemberPredicates(Tableau tableau) {
    Collection classMemberPredicates =
        tableau.getClassMemberPredicates();
    boolean retVal = false;
    while (!classMemberPredicates.isEmpty() &&
           !Thread.currentThread().isInterrupted()) {
      Iterator it =
          ((Collection) ((ArrayList) classMemberPredicates).clone()).iterator();
      while (it.hasNext()) {
   //     retVal = true;
        PredicateFormula pf = (PredicateFormula) it.next();
        pf.expand();
      }
    }
    return retVal;
  }

  /**
   * TODO implement
   */
  public static boolean expandComprehensionTriggers(Tableau tableau) {
    Map triggers = tableau.getComprehensionTriggers();
    boolean retVal = false;
    while (!triggers.isEmpty() &&
           !Thread.currentThread().isInterrupted()) {
      // TODO: is this line safe? should i clone something?
      Iterator it = triggers.entrySet().iterator();
      while (it.hasNext()) {
        retVal = true;
        Map.Entry e = (Map.Entry) it.next();
        PredicateFormula pf = (PredicateFormula) e.getKey();
        ClassTerm ct = (ClassTerm) e.getValue();
        pf.trigger(ct);
      }
    }
    return retVal;
  }

  /**
   * Unless it is interrupted, this expands all alphas.
   * This relies on alphas being added to the end.
   * Returns true if there was something to do.
   */
  public static boolean expandAllAlphas(Tableau tableau) {
    Collection alphas = tableau.getAlphas();
    boolean retVal = false;
    while (!alphas.isEmpty() && !Thread.currentThread().isInterrupted()) {
      Iterator it = ((Collection) ((ArrayList) alphas).clone()).iterator();
      while (it.hasNext()) {
        retVal = true;
        AlphaFormula alpha = (AlphaFormula) it.next();
        alpha.expand();
      }
    }
    return retVal;
  }

  /**
   * Unless it is interrupted, this expands all deltas.
   * This relies on deltas being added to the end.
   * Returns true if there was something to do.
   */
  public static boolean expandAllDeltas(Tableau tableau) {
    Collection deltas = tableau.getDeltas();
    boolean retVal = false;
    while (!deltas.isEmpty() && !Thread.currentThread().isInterrupted()) {
      Iterator it = ((Collection) ((ArrayList) deltas).clone()).iterator();
      while (it.hasNext()) {
        retVal = true;
        DeltaFormula delta = (DeltaFormula) it.next();
        delta.expand();
      }
    }
    return retVal;
  }

  public static boolean expandOneBeta(Tableau tableau) {
    // somhow, this is deleting a formula from the birthplace of the beta.
    Collection betas = tableau.getBetas();
    if (betas != null && !betas.isEmpty() &&
        !Thread.currentThread().isInterrupted()) {
      Iterator it = betas.iterator();
      BetaFormula beta = (BetaFormula) it.next();
      beta.expand();
      return true;
    }
    return false;
  }

  public static boolean expandOneGamma(Tableau tableau) {
    Collection gammas = tableau.getGammas();
    if (gammas != null && !gammas.isEmpty() &&
        !Thread.currentThread().isInterrupted()) {
      //      Iterator it = gammas.iterator();
      for (int i = 1; i <= tableau.getQLimit(); ++i) {
        Iterator it = gammas.iterator();
        while (it.hasNext()) {
          GammaFormula gamma = (GammaFormula) it.next();
          if (gamma.getCopies() < i) {
            gamma.expand();
            return true;
          }
        }
      }
    }
    return false;
  }

  public static void raiseQLimit(Tableau tableau) {
    tableau.setQLimit(tableau.getQLimit() + 1);
  }

  /**
   * Do all alphas, then all deltas, then either a single beta or a
   * single gamma.  Return true if something was done.  */
  public static boolean proveADBorG(Tableau tableau) {
    // TODO: set this up so that unification only happens if something
    // more than alphas and delta's are done.

    // Alphas
    boolean retVal = expandAllAlphas(tableau);

    // Deltas
    boolean temp = expandAllDeltas(tableau);

    // Beta or Gamma
    return (expandOneBeta(tableau) ||
            expandOneGamma(tableau) ||
            retVal || temp);
  }

  public static MultiBranchCloser unify(Tableau tableau) {
    return tableau.backtrackingUnify();
  }

  public static void expandKBSequent(Tableau tableau) {
    // Reduce the formula by expanding alpha, beta and deltas
    // using the alpha, beta, and delta inverse rules respectively
    boolean changed;
    do {
      changed = false;
      changed = changed || Prover.expandAllAlphas(tableau);
      changed = changed || Prover.expandOneBeta(tableau);

      ArrayList deltas = (ArrayList) tableau.getDeltas();
      boolean retVal = false;
      while (!deltas.isEmpty() &&
             !Thread.currentThread().isInterrupted()) {
        Iterator it = ((Collection) deltas.clone()).iterator();
        while (it.hasNext()) {
          retVal = true;
          DeltaFormula delta = (DeltaFormula) it.next();

          DeltaInverseFormula deltaInverse =
              new DeltaInverseFormula(delta);

          deltaInverse.expand();
        }
      }
      changed = changed || retVal;

    } while (changed);      // run until it stops changing
  }
  
  public static MultiBranchCloser unifyEquality(Tableau tableau) {
    return tableau.backtrackingUnifyEquality();
  }

  /**
   * Apply all alphas, deltas, comprehensions, and one beta or gamma.
   * Return true if something was done but the tableau is not known to
   * be a proof.  
   * @param tableau the tableau to operate on.
   * @return true if something was done but the tableau is not known to
   * be a proof.*/
  public static boolean prove(Tableau tableau) {
    // Alphas
    boolean retVal = Prover.expandAllAlphas(tableau);
    // Deltas
    retVal = Prover.expandAllDeltas(tableau) || retVal;
    // Comprehension Schema
    retVal = Prover.expandClassMemberPredicates(tableau) || retVal;
    // Beta or Gamma
    if (Prover.expandOneBeta(tableau))
      return true;
    if (Prover.unify(tableau) != null)
      return false;
    return Prover.expandOneGamma(tableau) || retVal;
  }

  /**
   * Looks for a proof using comprehension schema
   * and equality rules.  Hence the name.
   */
  public static boolean multiProve(Tableau tableau) {
    // Alphas
    boolean retVal = expandAllAlphas(tableau);
    // Deltas
    retVal = expandAllDeltas(tableau) || retVal;
    // expand comprehension triggers before doing comprehension stuff
    retVal = expandComprehensionTriggers(tableau) || retVal;
    // Comprehension Schema
    retVal = expandClassMemberPredicates(tableau) || retVal;
    // Brown's rule
    retVal = tableau.applyAllBrowns() || retVal;
    // Beta or Gamma
    if (expandOneBeta(tableau))
      return true;
    if (Prover.unifyEquality(tableau) != null)
      return false;
    return expandOneGamma(tableau) || retVal;
  }

  /**
   * Uses congruence closure to handle some equality.  Return true if
   * something was done but the tableau is not known to be a
   * proof.  */
  public static boolean proveEquality(Tableau tableau) {
    boolean retVal = expandAllAlphas(tableau);
    // Deltas
    retVal = expandAllDeltas(tableau) || retVal;
    // Brown's rule
    retVal = tableau.applyAllBrowns() || retVal;
    // Beta or Gamma
    if (expandOneBeta(tableau))
      return true;
    if (Prover.unifyEquality(tableau) != null)
      return false;
    return expandOneGamma(tableau) || retVal;
  }

  private Prover() {}
}
