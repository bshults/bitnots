package bitnots.expressions;

import java.util.*;
import java.io.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class Forall extends FOLFormula {

  public static final String FORALL_STRING = "forall";

  public static final FOLConstructor SYMBOL =
    (FOLConstructor)
    Symbol.putOrGet(FOLConstructor.class, Forall.FORALL_STRING);

  public Formula replaceVariables(Map map) {
    return new Forall(this.getBoundVars(),
                      this.body.replaceVariables(map));
  }

  public Formula replaceUnboundOccurrencesWith(Variable v, Term t) {
    // NOTE: since free and bound vars are now distinct, this is fine.
    return new Forall(this.getBoundVars(),
                      this.getBody().replaceUnboundOccurrencesWith(v, t));
  }

  public Formula apply(Substitution s) {
    return new Forall(this.getBoundVars(),
                      this.getBody().apply(s));
  }

  public Forall(List bvs, Formula f) {
    super(SYMBOL, bvs, f);
  }
  
} // Forall

