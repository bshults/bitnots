package bitnots.tableaux;

import java.util.*;
import bitnots.expressions.*;

public class PositiveForall extends GammaFormula
  implements PositiveFormula {

  public PositiveForall(Formula f) {
    super(f, HELPER);
  }
}

