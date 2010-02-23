package bitnots.tableaux;

/**
 * This class cannot be extended.  It has two instances: POSITIVE and
 * NEGATIVE.
 */

public abstract class FormulaHelper {

  public static final FormulaHelper POSITIVE = new Positive();

  public static final FormulaHelper NEGATIVE = new Negative();

//  public abstract void addToTableauNode(TableauNode pn, TableauFormula form);

  public abstract boolean getSign();

  // Cannot be extended outside this class.
  private FormulaHelper() {}

  private static final class Positive extends FormulaHelper {
/*    public final void addToTableauNode(TableauNode pn, TableauFormula form) {
      pn.addNewHyp(form);
    }*/

    public final boolean getSign() {
      return true;
    }
  }

  private static final class Negative extends FormulaHelper {
/*    public final void addToTableauNode(TableauNode pn, TableauFormula form) {
      pn.addNewGoal(form);
    }*/

    public final boolean getSign() {
      return false;
    }
  }

}

