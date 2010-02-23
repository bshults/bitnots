package bitnots.expressions;

import java.util.*;
import java.io.*;

/**
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class Forsome extends FOLFormula {

  public static final String FORSOME_STRING = "for-some";

  public static final FOLConstructor SYMBOL =
    (FOLConstructor)
    Symbol.putOrGet(FOLConstructor.class, Forsome.FORSOME_STRING);

  public Formula replaceVariables(Map map) {
    return new Forsome(this.getBoundVars(),
                       this.body.replaceVariables(map));
  }

  public Formula replaceUnboundOccurrencesWith(Variable v, Term t) {
    // NOTE: since free and bound vars are now distinct, this is fine.
    return new Forsome(this.getBoundVars(),
                       this.getBody().replaceUnboundOccurrencesWith(v, t));
  }

  public Formula apply(Substitution s) {
    return new Forsome(this.getBoundVars(),
                       this.getBody().apply(s));
  }

  public Forsome(List bvs, Formula f) {
    super(Forsome.SYMBOL, bvs, f);
  }
  
} // Forsome

