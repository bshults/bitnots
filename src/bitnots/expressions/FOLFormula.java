package bitnots.expressions;

import bitnots.equality.*;
import java.util.*;

/**
 * @author Benjamin Shults
 * @version .2
 */

public abstract class FOLFormula extends Formula {

  private List boundVars;
  public Formula body;

  public Formula getBody() {
    return this.body;
  }

  public List getBoundVars() {
    return boundVars;
  }

  /**
   * Checks that the receiver and argument are in the same class, then
   * compares the arguments.
   */
  public boolean equals(Object o) {  
    if (o.getClass() == this.getClass()) {
      Substitution sub = Substitution.createBoundVarSubstitution(
                this.getBoundVars(), ((FOLFormula) o).getBoundVars());

      if (sub != null) 
        if (this.getBody().equalsUnderSub(
                              ((FOLFormula) o).getBody(), sub))
          return true;  // Alpha congruent
        else
          return false;  // Bodies differ
      else
        return false;  // sub was null, (arity different)
    } else
      return false;  // Classes differ
  }

  public boolean equalsUnderSub(Formula f, Substitution s) {
    if (f instanceof FOLFormula) {
      FOLFormula form = (FOLFormula) f;
      Substitution sub = 
        Substitution.createBoundVarSubstitution(this.getBoundVars(),
                                                form.getBoundVars());
      Substitution newSub = Substitution.compatible(s, sub);
      if (newSub != null) {
        if (this.getBody().equalsUnderSub(form.getBody(), newSub))
          return true;
        return false;
      } else
        return false;
    } else
      return false;
  }

  public int hashCode() {
    int value = 0;
    value = this.getBody().hashCode();
    return this.getClass().hashCode() + value;
  }

  public Set freeVars() {
    return this.body.freeVars(this.boundVars);
  }

  public Set freeVars(Collection notFree) {
    ArrayList pass = new ArrayList(notFree.size() + this.boundVars.size());
    pass.addAll(notFree);
    pass.addAll(this.boundVars);
    return this.body.freeVars(pass);
  }

  int failFast(Formula f) {
    // TODO: fix
    throw new UnsupportedOperationException();
//      if (f instanceof FOLFormula) {
//        FOLFormula fol = (FOLFormula) f;
//        Formula[] forms = null;
//        // = replaceBoundVarWithNewConstant(new FOLFormula[] {this, fol});
//        Formula f1 = forms[0];
//        Formula f2 = forms[1];
//        return f1.failFast(f2);
//      } else
//        return Substitution.CLASH;
  }

  public Substitution unify(Formula f, Substitution s) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.getBoundVars(),
                                                ff.getBoundVars());
      return this.getBody().unify(ff.getBody(), s, bvs);
    } else
      return null;
  }

  // This implementation is inherited by several subclasses.
  public Substitution unify(Formula f, Substitution s,
                            Substitution bvs) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      bvs = Substitution.createBoundVarSubstitution(
        this.getBoundVars(), ff.getBoundVars(), bvs);
      return this.getBody().unify(ff.getBody(), s, bvs);
    } else
      return null;
  }

  public Substitution unify(Formula f, Substitution s, DSTGraph cc) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.getBoundVars(),
                                                ff.getBoundVars());
      if (bvs == null)
        return null;
      return this.getBody().unify(ff.getBody(), s, bvs, cc);
    } else
      return null;
  }

  // inherited by several subclasses
  public Substitution unify(Formula f, Substitution s,
                            Substitution bvs, DSTGraph cc) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      bvs = Substitution.createBoundVarSubstitution(
        this.getBoundVars(), ff.getBoundVars(), bvs);
      return this.getBody().unify(ff.getBody(), s, bvs, cc);
    } else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.getBoundVars(),
                                                ff.getBoundVars());
      return this.getBody().unifyRecApplied(ff.getBody(), s, bvs);
    } else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s,
                                      Substitution bvs) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      bvs = Substitution.createBoundVarSubstitution(
        this.getBoundVars(), ff.getBoundVars(), bvs);
      return this.getBody().unifyRecApplied(ff.getBody(), s, bvs);
    } else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.getBoundVars(),
                                                ff.getBoundVars());
      return this.getBody().unifyRecApplied(ff.getBody(), s, bvs, cc);
    } else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s,
                                      Substitution bvs, DSTGraph cc) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      bvs = Substitution.createBoundVarSubstitution(
        this.getBoundVars(), ff.getBoundVars(), bvs);
      return this.getBody().unifyRecApplied(ff.getBody(), s, bvs, cc);
    } else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.getBoundVars(),
                                                ff.getBoundVars());
      return this.getBody().unifyApplied(ff.getBody(), s, bvs, cc);
    } else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      Substitution bvs =
        Substitution.createBoundVarSubstitution(this.getBoundVars(),
                                                ff.getBoundVars());
      return this.getBody().unifyApplied(ff.getBody(), s, bvs);
    } else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s,
                                   Substitution bvs) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      bvs = Substitution.createBoundVarSubstitution(
        this.getBoundVars(), ff.getBoundVars(), bvs);
      return this.getBody().unifyApplied(ff.getBody(), s, bvs);
    } else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s,
                                   Substitution bvs, DSTGraph cc) {
    if (f.getClass() == this.getClass()) {
      //    if (f instanceof FOLFormula) {
      // getConstructor() == this.getConstructor()) {
      FOLFormula ff = (FOLFormula) f;
      bvs = Substitution.createBoundVarSubstitution(
        this.getBoundVars(), ff.getBoundVars(), bvs);
      return this.getBody().unifyApplied(ff.getBody(), s, bvs, cc);
    } else
      return null;
  }

  public boolean contains(Term t) {
    return this.getBody().contains(t);
  }

  public boolean contains(Formula f) {
    // TODO: use alphaEquivalence!
    if (this.equals(f))
      return true;
    else
      return this.getBody().contains(f);
  }

  public String toString() {
    //return "(" + this.getConstructor() + " " +
    //  this.printBoundVars() + "\n" + this.body.toString() + ")";
    return "(" + this.getConstructor() + " " +
        this.printBoundVars() + " " + this.body.toString() + ")";
  }

  protected String printBoundVars() {
    Iterator it = this.getBoundVars().iterator();
    StringBuffer sb = new StringBuffer("(");
    sb.append(it.next().toString());
    while (it.hasNext()) {
      sb.append(" " + it.next().toString());
    }
    sb.append(")");
    return sb.toString();
  }

  /**
   * This does not copy bvs.  Be careful not to modify bvs after
   * creating this.  */
  protected FOLFormula(FOLConstructor sym, List bvs, Formula body) {
    super(sym);
    this.boundVars = bvs;
    this.body = body;
  }

  protected FOLFormula(String sym, List bvs, Formula body) {
    this((FOLConstructor) Symbol.putOrGet(FOLConstructor.class, sym),
         bvs, body);
  }

} // FOLFormula

