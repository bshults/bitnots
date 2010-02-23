package bitnots.tableaux;

import java.util.*;
import bitnots.expressions.*;

public class NegativeForsome extends GammaFormula
  implements NegativeFormula {

  public NegativeForsome(Formula f) {
    super(f, HELPER);
  }
}

