package bitnots.expressions;

import bitnots.util.*;
import bitnots.equality.*;
//import edu.wcu.cs.util.*;
import java.util.*;

/**
 * @author Benjamin Shults
 * @version .2
 */

public abstract class PropositionalFormula extends Formula {
  
  private Collection<Formula> args;

  public Iterator<Formula> arguments() {
    return java.util.Collections.unmodifiableCollection(this.args).iterator();
  }

  /*public List getArguments() {
    return java.util.Collections.unmodifiableList(this.args);
  }*/

  public int getNumberOfArguments() {
    return this.args.size();
  }

  /**
   * Checks that the receiver and argument are in the same class, then
   * compares the arguments.
   */
  public boolean equals(Object o) {
    if (o.getClass() == this.getClass() &&
        this.getNumberOfArguments() ==
        ((PropositionalFormula) o).getNumberOfArguments()) {
      Iterator<Formula> mine = this.arguments();
      Iterator<Formula> yours = ((PropositionalFormula) o).arguments();
      while (mine.hasNext()) {
        if (!mine.next().equals(yours.next()))
          return false;
      }
      return true;
    } else
      return false;
  }
  
  public boolean equalsUnderSub(Formula f, Substitution s) {
    if (f.getClass() == this.getClass() &&
        this.getNumberOfArguments() ==
        ((PropositionalFormula) f).getNumberOfArguments()) {
      Iterator<Formula> itA = this.arguments();
      Iterator<Formula> itB = ((PropositionalFormula) f).arguments();
      while (itA.hasNext()) {
        Formula a = itA.next();
        Formula b = itB.next();
        if (!(a.equalsUnderSub(b, s)))
          return false;
      }
      return true;
    } else
      return false;
  }

  public int hashCode() {
    int value = 0;
    Iterator<Formula> it = this.arguments();
    while (it.hasNext())
      value += it.next().hashCode();
    return this.getClass().hashCode() + value;
  }

  /**
   * Return the set of free variables in the receiver.
   * @return the set of free variables in the receiver.
   */
  public Set freeVars() {
    HashSet value = new HashSet();
    Iterator<Formula> it = args.iterator();
    while (it.hasNext()) {
      value.addAll(it.next().freeVars());
    }
    return value;
  }

  /**
   * Return the set of free variables in the receiver excluding
   * variables in <b>notFree</b>.
   * @return the set of free variables in the receiver excluding
   * variables in <b>notFree</b>.  */
  public Set freeVars(Collection notFree) {
    HashSet value = new HashSet();
    Iterator it = args.iterator();
    while (it.hasNext()) {
      value.addAll(((Formula) it.next()).freeVars(notFree));
    }
    return value;
  }

  public Formula replaceVariables(Map map) {
    Collection newArgs = new ArrayList(this.args.size());
    Iterator args = this.arguments();
    while (args.hasNext())
      newArgs.add(((Formula) args.next()).replaceVariables(map));
    return FormulaFactory.getFormula(
      (PropositionalConstructor) this.getConstructor(),
      new Object[] {newArgs});
  }

  public Substitution unify(Formula f, Substitution s) {
    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
      if (pf.getConstructor().equals(this.getConstructor())) {
        // f must be a PropositionalFormula because there is no clash.
        Iterator receiver = this.arguments();
        Iterator argument = pf.arguments();
        // t1 iterates through the child terms of the receiver.
        Formula f1;
        // f2 iterates through the child terms of the argument.
        Formula f2;
        while (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unify(f2, s);
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unify(Formula f, Substitution s,
                            Substitution bvs) {
    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
      if (pf.getConstructor().equals(this.getConstructor())) {
        // f must be a PropositionalFormula because there is no clash.
        Iterator receiver = this.arguments();
        Iterator argument = pf.arguments();
        // t1 iterates through the child terms of the receiver.
        Formula f1;
        // f2 iterates through the child terms of the argument.
        Formula f2;
        while (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unify(f2, s, bvs);
        }
        return s;
      } else
        return null;
    } else
      return null;
  }

  public Substitution unify(Formula f, Substitution s, DSTGraph cc) {
    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
      if (pf.getConstructor().equals(this.getConstructor())) {
        // f must be a PropositionalFormula because there is no clash.
        Iterator receiver = this.arguments();
        Iterator argument = pf.arguments();
        // t1 iterates through the child terms of the receiver.
        Formula f1;
        // f2 iterates through the child terms of the argument.
        Formula f2;
        while (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unify(f2, s, cc);
          if (s == null)
            return null;
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unify(Formula f, Substitution s,
                            Substitution bvs, DSTGraph cc) {
    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
      if (pf.getConstructor().equals(this.getConstructor())) {
        // f must be a PropositionalFormula because there is no clash.
        Iterator receiver = this.arguments();
        Iterator argument = pf.arguments();
        // t1 iterates through the child terms of the receiver.
        Formula f1;
        // f2 iterates through the child terms of the argument.
        Formula f2;
        while (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unify(f2, s, bvs, cc);
          if (s == null)
            return null;
        }
        return s;
      } else
        return null;
    } else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s) {
    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
      if (pf.getConstructor().equals(this.getConstructor())) {
        // f must be a PropositionalFormula because there is no clash.
        Iterator receiver = this.arguments();
        Iterator argument = pf.arguments();
        // t1 iterates through the child terms of the receiver.
        Formula f1;
        // f2 iterates through the child terms of the argument.
        Formula f2;
        if (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unifyRecApplied(f2, s);
          while (receiver.hasNext()) {
            f1 = (Formula) receiver.next();
            f2 = (Formula) argument.next();
            s = f1.unify(f2, s);
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
      if (pf.getConstructor().equals(this.getConstructor())) {
        // f must be a PropositionalFormula because there is no clash.
        Iterator receiver = this.arguments();
        Iterator argument = pf.arguments();
        // t1 iterates through the child terms of the receiver.
        Formula f1;
        // f2 iterates through the child terms of the argument.
        Formula f2;
        if (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unifyRecApplied(f2, s, cc);
          while (receiver.hasNext()) {
            f1 = (Formula) receiver.next();
            f2 = (Formula) argument.next();
            s = f1.unify(f2, s, cc);
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s,
                                      Substitution bvs) {
    if (f.getClass() == this.getClass()) {
//    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
//      if (pf.getConstructor().equals(this.getConstructor())) {
      // f must be a PropositionalFormula because there is no clash.
      Iterator receiver = this.arguments();
      Iterator argument = pf.arguments();
      // t1 iterates through the child terms of the receiver.
      Formula f1;
      // f2 iterates through the child terms of the argument.
      Formula f2;
      if (receiver.hasNext()) {
        f1 = (Formula) receiver.next();
        f2 = (Formula) argument.next();
        s = f1.unifyRecApplied(f2, s, bvs);
        while (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unify(f2, s, bvs);
        }
      }
      return s;
    } else
      return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s,
                                      Substitution bvs, DSTGraph cc) {
    if (f.getClass() == this.getClass()) {
      PropositionalFormula pf = (PropositionalFormula) f;
      // f must be a PropositionalFormula because there is no clash.
      Iterator receiver = this.arguments();
      Iterator argument = pf.arguments();
      // t1 iterates through the child terms of the receiver.
      Formula f1;
      // f2 iterates through the child terms of the argument.
      Formula f2;
      if (receiver.hasNext()) {
        f1 = (Formula) receiver.next();
        f2 = (Formula) argument.next();
        s = f1.unifyRecApplied(f2, s, bvs, cc);
        while (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unify(f2, s, bvs, cc);
        }
      }
      return s;
    } else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
      if (pf.getConstructor().equals(this.getConstructor())) {
        // f must be a PropositionalFormula because there is no clash.
        Iterator receiver = this.arguments();
        Iterator argument = pf.arguments();
        // t1 iterates through the child terms of the receiver.
        Formula f1;
        // f2 iterates through the child terms of the argument.
        Formula f2;
        if (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unifyApplied(f2, s, cc);
          while (receiver.hasNext()) {
            f1 = (Formula) receiver.next();
            f2 = (Formula) argument.next();
            s = f1.unify(f2, s, cc);
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s) {
    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
      if (pf.getConstructor().equals(this.getConstructor())) {
        // f must be a PropositionalFormula because there is no clash.
        Iterator receiver = this.arguments();
        Iterator argument = pf.arguments();
        // t1 iterates through the child terms of the receiver.
        Formula f1;
        // f2 iterates through the child terms of the argument.
        Formula f2;
        if (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unifyApplied(f2, s);
          while (receiver.hasNext()) {
            f1 = (Formula) receiver.next();
            f2 = (Formula) argument.next();
            s = f1.unify(f2, s);
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s,
                                   Substitution bvs) {
    if (f.getClass() == this.getClass()) {
//    if (f instanceof PropositionalFormula) {
      PropositionalFormula pf = (PropositionalFormula) f;
//      if (pf.getConstructor().equals(this.getConstructor())) {
      // f must be a PropositionalFormula because there is no clash.
      Iterator receiver = this.arguments();
      Iterator argument = pf.arguments();
      // t1 iterates through the child terms of the receiver.
      Formula f1;
      // f2 iterates through the child terms of the argument.
      Formula f2;
      if (receiver.hasNext()) {
        f1 = (Formula) receiver.next();
        f2 = (Formula) argument.next();
        s = f1.unifyApplied(f2, s, bvs);
        while (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unify(f2, s, bvs);
        }
      }
      return s;
    } else
      return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s,
                                   Substitution bvs, DSTGraph cc) {
    if (f.getClass() == this.getClass()) {
      PropositionalFormula pf = (PropositionalFormula) f;
      // f must be a PropositionalFormula because there is no clash.
      Iterator receiver = this.arguments();
      Iterator argument = pf.arguments();
      // t1 iterates through the child terms of the receiver.
      Formula f1;
      // f2 iterates through the child terms of the argument.
      Formula f2;
      if (receiver.hasNext()) {
        f1 = (Formula) receiver.next();
        f2 = (Formula) argument.next();
        s = f1.unifyApplied(f2, s, bvs, cc);
        while (receiver.hasNext()) {
          f1 = (Formula) receiver.next();
          f2 = (Formula) argument.next();
          s = f1.unify(f2, s, bvs, cc);
        }
      }
      return s;
    } else
      return null;
  }

  int failFast(Formula f) {
    if (f instanceof PropositionalFormula) {
      if (f.getConstructor().equals(this.getConstructor())) {
        try {
          Iterator receiver = this.arguments();
          Iterator argument =
            ((PropositionalFormula) f).arguments();
          int retVal = Substitution.ALPHA;
          while (receiver.hasNext()) {
            switch (((Formula) receiver.next()).failFast(
                      (Formula) argument.next())) {
            case Substitution.CLASH:
              return Substitution.CLASH;
            case Substitution.DIFFERENCE:
              retVal = Substitution.DIFFERENCE;
              break;
            case Substitution.ALPHA:
              break;
            }
          }
          return retVal;
        } catch (NoSuchElementException nsee) {
          throw new ArityException("form1: " + this + "\nform2: " + f);
        }
      } else
        return Substitution.CLASH;
    } else
      return Substitution.CLASH;
  }

  public Formula apply(Substitution s) {
    Iterator it = this.arguments();
    Collection newV = new ArrayList(this.args.size());
    while (it.hasNext()) {
      newV.add(((Formula) it.next()).apply(s));
    }
    return FormulaFactory.getFormula(
      (PropositionalConstructor) this.getConstructor(), new Object[] {newV});
  }

  public boolean contains(final Formula f) {
    if (this.equals(f))
      return true;
    else {
      Iterator it = this.arguments();
      while (it.hasNext()) {
        if (((Formula) it.next()).contains(f))
          return true;
      }
      return false;
    }
  }

  public boolean contains(final Term t) {
    Iterator it = this.arguments();
    while (it.hasNext()) {
      if (((Formula) it.next()).contains(t))
        return true;
    }
    return false;
  }

  public Formula replaceUnboundOccurrencesWith(Variable v, Term t) {
    Collection newArgs = new ArrayList(this.args.size());
    Iterator args = this.arguments();
    while (args.hasNext()) {
      newArgs.add(((Formula) args.next()).replaceUnboundOccurrencesWith(v, t));
    }
    return FormulaFactory.getFormula(
      (PropositionalConstructor) this.getConstructor(), new Object[] {newArgs});
  }

  protected String printArgs() {
    return this.printArgs(" ");
  }

  protected String printArgs(String separator) {
    if (this.args == null)
      return "";
    else {
      Iterator it = this.arguments();
      StringBuffer sb = new StringBuffer();
      if (it.hasNext())
        sb.append(it.next().toString());
      while (it.hasNext())
	sb.append(separator + it.next().toString());
      return sb.toString();
    }
  }

  public String toString() {
    return "(" + this.getConstructor() + " " + this.printArgs("\n") + ")";
  }

  protected PropositionalFormula(PropositionalConstructor s, List args) {
    super(s);
    this.args = args;
  }
  
} // PropositionalFormula

