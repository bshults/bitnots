package bitnots.tableaux;

import java.util.*;
import bitnots.expressions.*;

public class NegativeForall extends DeltaFormula
  implements NegativeFormula {

  public NegativeForall(Formula f) {
    super(f, HELPER);
  }
  
}
