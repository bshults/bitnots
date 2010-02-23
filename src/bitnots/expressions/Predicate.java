package bitnots.expressions;

import java.util.*;
import bitnots.equality.*;
import java.io.*;

/**
 * Instances represent predicates.  Each has a symbol and a (possibly
 * empty) list of arguments.  This arguments will be terms.
 * @author Benjamin Shults
 * @version .2
 */

public class Predicate extends Formula {
  
  protected List args;

  public Iterator arguments() {
    return this.args.iterator();
  }

  public int hashCode() {
    int value = 0;
    Iterator it = this.arguments();
    while (it.hasNext())
      value += it.next().hashCode();
    return this.getConstructor().hashCode() + value;
  }

  public boolean equals(Object o) {
    if (o instanceof Predicate) {
      Predicate f = (Predicate) o;
      if (this.getConstructor().equals(f.getConstructor())) {
        Iterator myIt = this.arguments();
        Iterator yourIt = f.arguments();
        while (myIt.hasNext())
          if (!myIt.next().equals(yourIt.next()))
            return false;
        return true;
      } else
        return false;
    } else
      return false;
  }
  
  public boolean equalsUnderSub(Formula f, Substitution s) {
    return this.apply(s).equals(f.apply(s));
  }

  public Set freeVars() {
    Iterator it = args.iterator();
    HashSet value = new HashSet();
    while (it.hasNext()) {
      value.addAll(((Term) it.next()).freeVars());
    }
    return value;
  }

  /**
   * Return the set of free variables in the receiver excluding
   * variables in <b>notFree</b>.
   * @return the set of free variables in the receiver excluding
   * variables in <b>notFree</b>.  */
  public Set freeVars(Collection notFree) {
    Iterator it = args.iterator();
    HashSet value = new HashSet();
    while (it.hasNext()) {
      value.addAll(((Term) it.next()).freeVars(notFree));
    }
    return value;
  }

  int failFast(Formula f) {
    if (f instanceof Predicate) {
      if (((Predicate) f).getConstructor().equals(
            this.getConstructor())) {
        try {
          Iterator receiver = this.arguments();
          Iterator argument = ((Predicate) f).arguments();
          int retVal = Substitution.ALPHA;
          while (receiver.hasNext()) {
            switch (((Term) receiver.next()).failFast(
                      (Term) argument.next())) {
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
          throw new ArityException("term1: " + this + "\nterm2: " + f);
        }
      } else
        return Substitution.CLASH;
    } else
      return Substitution.CLASH;
  }

  public Formula replaceVariables(Map map) {
    ArrayList newArgs = new ArrayList(this.args.size());
    Iterator args = this.arguments();
    while (args.hasNext())
      newArgs.add(((Term) args.next()).replaceVariables(map));
    return FormulaFactory.getFormula(
      (PredicateConstructor) this.getConstructor(),
      new Object[] {newArgs});
  }

  public Substitution unify(Formula f, Substitution s) {
//    throws NotUnifiableException {
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        while (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unify(t2, s);
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
                            Substitution bvs) {
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        while (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unify(t2, s, bvs);
          if (s == null)
            return null;
        }
        return s;
      } else
        return null;
//        throw new NotUnifiableException(Substitution.CLASH);
    } else
        return null;
//      throw new NotUnifiableException(Substitution.CLASH);
  }

  public Substitution unify(Formula f, Substitution s, DSTGraph cc) {
//    throws NotUnifiableException {
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        while (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unify(t2, s, cc);
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
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        while (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unify(t2, s, bvs, cc);
          if (s == null)
            return null;
        }
        return s;
      } else
        return null;
//        throw new NotUnifiableException(Substitution.CLASH);
    } else
        return null;
//      throw new NotUnifiableException(Substitution.CLASH);
  }

  public Substitution unifyRecApplied(Formula f, Substitution s) {
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyRecApplied(t2, s);
          if (s == null)
            return null;
          while (receiver.hasNext()) {
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s);
            if (s == null)
              return null;
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
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyRecApplied(t2, s, bvs);
          if (s == null)
            return null;
          while (receiver.hasNext()) {
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, bvs);
            if (s == null)
              return null;
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyRecApplied(t2, s, cc);
          if (s == null)
            return null;
          while (receiver.hasNext()) {
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, cc);
            if (s == null)
              return null;
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyRecApplied(Formula f, Substitution s,
                                      Substitution bvs, DSTGraph cc) {
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyRecApplied(t2, s, bvs, cc);
          if (s == null)
            return null;
          while (receiver.hasNext()) {
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, bvs, cc);
            if (s == null)
              return null;
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s, DSTGraph cc) {
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyApplied(t2, s, cc);
          if (s == null)
            return null;
          while (receiver.hasNext()) {
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, cc);
            if (s == null)
              return null;
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s) {
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyApplied(t2, s);
          if (s == null)
            return null;
          while (receiver.hasNext()) {
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s);
            if (s == null)
              return null;
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
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyApplied(t2, s, bvs);
          if (s == null)
            return null;
          while (receiver.hasNext()) {
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, bvs);
            if (s == null)
              return null;
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyApplied(Formula f, Substitution s,
                                   Substitution bvs, DSTGraph cc) {
    if (f instanceof Predicate) {
      Predicate pred = (Predicate) f;
      if (pred.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = pred.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyApplied(t2, s, bvs, cc);
          if (s == null)
            return null;
          while (receiver.hasNext()) {
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, bvs, cc);
            if (s == null)
              return null;
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Formula replaceUnboundOccurrencesWith(Variable v, Term t) {
    List newArgs = new ArrayList(this.args.size());
    Iterator args = this.arguments();
    while (args.hasNext()) {
      newArgs.add(((Term) args.next()).replaceUnboundOccurrencesWith(v, t));
    }
    return new Predicate((PredicateConstructor) this.getConstructor(),
                         newArgs);
  }

  public boolean contains(final Term t) {
    Iterator it = this.arguments();
    while (it.hasNext()) {
      if (((Term) it.next()).contains(t))
        return true;
    }
    return false;
  }

  public boolean contains(final Formula f) {
    if (this.equals(f))
      return true;
    else {
      Iterator it = this.arguments();
      while (it.hasNext()) {
        if (((Term) it.next()).contains(f))
          return true;
      }
      return false;
    }
  }

  public Formula apply(Substitution s) {
    Iterator it = this.args.iterator();
    List newV = new ArrayList(this.args.size());
    while (it.hasNext()) {
      newV.add(((Term) it.next()).apply(s));
    }
    return new Predicate((PredicateConstructor) this.getConstructor(), newV);
  }

//    /** Make this version of add public. */
//    public void add(Term arg) {
//      if (this.args == null)
//        this.args = new ArrayBasedList(5);
//      this.args.add(arg);
//    }

  public List getArgs() {
    return Collections.unmodifiableList(args);
  }

//    protected String printArgs() {
//      return this.printArgs(" ");
//    }

//    protected String printArgs(String separator) {
//      if (this.args == null)
//        return "";
//      else {
//        Iterator it = this.args.iterator();
//        StringBuffer sb = new StringBuffer();
//        if (it.hasNext())
//          sb.append(it.next().toString());
//        while (it.hasNext())
//          sb.append(separator + it.next().toString());
//        return sb.toString();
//      }
//    }

  public String toString() {
    StringBuffer retVal = new StringBuffer("(");
    retVal.append(this.getConstructor().toString());
    Iterator it = this.arguments();
    while (it.hasNext()) {
      retVal.append(" " + it.next());
    }
    retVal.append(")");
    return retVal.toString();
  }

  protected Predicate(PredicateConstructor sym) {
    this(sym, new ArrayList());
  }

  protected Predicate(PredicateConstructor sym, List l) {
    super(sym);
    this.args = new ArrayList<Term>(l);
    //    sym.checkArity(l.size());
  }

} // Predicate

