package bitnots.tableaux;

/**
 * This is a hack until Java let's me import static fields.
 * 
 * <p>The value of HELPER is FormulaHelper.NEGATIVE.
 *
 * <p>The value of SIGN is false.
 */

public interface NegativeFormula {
  boolean SIGN = false;
  FormulaHelper HELPER = FormulaHelper.NEGATIVE;
}

