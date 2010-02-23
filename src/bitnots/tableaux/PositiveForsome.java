package bitnots.tableaux;

import java.util.*;
import bitnots.expressions.*;

public class PositiveForsome extends DeltaFormula
  implements PositiveFormula {

  public PositiveForsome(Formula f) {
    super(f, HELPER);
  }
}

