package bitnots.tableaux;

/**
 * This is a hack until Java let's me import static fields.
 * 
 * <p>The value of HELPER is FormulaHelper.POSITIVE.
 *
 * <p>The value of SIGN is true.
 */

public interface PositiveFormula {
  boolean SIGN = true;
  FormulaHelper HELPER = FormulaHelper.POSITIVE;
}

