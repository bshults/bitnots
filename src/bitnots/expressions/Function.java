package bitnots.expressions;

import bitnots.util.*;
import bitnots.equality.*;
import java.util.*;
import java.io.*;

/**
 * @author Benjamin Shults
 * @version .2
 */

public class Function extends ComplexTerm {

  /**
   * This maps symbols (FunctionConstructors) to constant terms.  This
   * is used by the getConstant (used by the parser) to ensure symbols
   * map to the same constant.
   */
  private static final Hashtable TABLE = new Hashtable(100);

  protected List args;

  private static final ArrayList skolems = new ArrayList();

  /**
   * The name of a skolem function is a variation of the name of <b>bv</b>.
   * @param bv the free variable that this function will replace and
   * on whose name the return value's name will be based.
   * @param freeVars the argument list to the new function.
   * @return a new skolem function whose name is a variation on the
   * name of <b>bv</b> and whose arguments are freeVars.  */
  public static Function createNewSkolemFunction(Variable bv,
                                                 List freeVars) {
    FunctionConstructor skolem =
      FunctionConstructor.createUniqueFunctionSymbol(bv);
    Function value = new Function(skolem, freeVars);
    Function.skolems.add(skolem);
    return value;
  }
  
  /**
   * The name of a constant is a variation of the string <b>id</b>.
   * @param id the string on which the return value's name will be
   * based.
   * @return a new constant whose name is a variation on <b>id</b>.
   */
  public static Function createNewConstant(String id) {
    FunctionConstructor skolem =
      FunctionConstructor.createUniqueFunctionSymbol(id);
    Function value = new Function(skolem);
    Function.skolems.add(skolem);
    return value;
  }
  
  /**
   * The hashcode for this object.  Determined on construction, so it
   * does not need to be determined upon every call.  This hinges on
   * the fact that the Variable's internals, the symbol, the internal
   * name and the internal number, are not going to change.  This
   * gives us a huge performace boost.
   */
  private int hashCode;
  
  /**
   * Recomputes the hash code for this object.  This method must be
   * called every time the internals of the object changes.
   */
  public void computeHashCode() {
    int value = 0;
    Iterator it = this.arguments();
    while (it.hasNext())
      value += it.next().hashCode();
    value += this.getConstructor().hashCode() + value;
    this.hashCode = value;
  }
  
  public int hashCode() {
//    UNCOMMENT THIS TO NOT ASSUME IMMUTABLE OBJECTS
//    this.computeHashCode();
    return this.hashCode;
  }

  public boolean equals(Object o) {
    if (o instanceof Function) {
      Function f = (Function) o;
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

  /**
   * If the receiver is a skolem function, this returns the index of
   * its function symbol in the list of skolem function symbols.
   * Otherwise, this returns -1.
   */
  public int isSkolem() {
    return Function.skolems.indexOf(this.getConstructor());
  }

  /**
   * This just needs a unique name.  The name needn't make any sense
   * since it should never be seen.
   * @return a new constant. */
  public static Function createTemporaryConstant() {
    return new Function(FunctionConstructor.createUniqueFunctionSymbol());
  }

  /**
   * Find or create a constant term with the given name.
   * @return */
  // TODO: purge this table
  public static Function getConstant(String name) {
    FunctionConstructor s = null;
    Function value =
      (Function)
      Function.TABLE.get(s = (FunctionConstructor)
                         Symbol.putOrGet(FunctionConstructor.class, name));
    if (value == null) {
      Function.TABLE.put(s, value = new Function(s));
    }
    return value;
  }

  public List getArguments() {
    return java.util.Collections.unmodifiableList(this.args);
  }

  public int getNumberOfArguments() {
    return this.args.size();
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

  public boolean isConstant() {
    return this.args.size() == 0;
  }

  public Iterator arguments() {
    return args.iterator();
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

  public boolean contains(final Term t) {
    if (this.alphaCongruent(t))
      return true;
    Iterator it = this.arguments();
    while (it.hasNext()) {
      if (((Term) it.next()).contains(t))
        return true;
    }
    return false;
  }

  public Term replaceUnboundOccurrencesWith(Variable v, Term t) {
    List newArgs = new ArrayList(this.args.size());
    Iterator args = this.arguments();
    while (args.hasNext()) {
      newArgs.add(((Term) args.next()).
                  replaceUnboundOccurrencesWith(v, t));
    }
    return new Function((FunctionConstructor)
                        this.getConstructor(), newArgs);
  }

  public Term replaceAllConstants(Function old, Function newC) {
    Iterator args = this.arguments();
    if (args.hasNext()) {
      List newArgs = new ArrayList(this.args.size());
      while (args.hasNext()) {
        newArgs.add(((Term) args.next()).
                    replaceAllConstants(old, newC));
      }
      return new Function((FunctionConstructor)
                          this.getConstructor(), newArgs);
    } else if (this.getConstructor() == old.getConstructor())
      return newC;
    else
      return this;
  }

  public int failFast(Term t) {
    if (t instanceof Variable) {
      return Substitution.DIFFERENCE;
    } else if (t instanceof Function) {
      if (((Function) t).getConstructor().equals(
            this.getConstructor())) {
        try {
          Iterator receiver = this.arguments();
          Iterator argument = ((Function) t).arguments();
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
          throw new ArityException("term1: " + this + "\nterm2: " + t);
        }
      } else
        return Substitution.CLASH;
    } else
      return Substitution.CLASH;
  }

  public Term replaceVariables(Map map) {
    Collection newArgs = new ArrayList(this.args.size());
    Iterator args = this.arguments();
    while (args.hasNext())
      newArgs.add(((Term) args.next()).replaceVariables(map));
    return TermFactory.getTerm(
      (FunctionConstructor) this.getConstructor(),
      new Object[] {newArgs});
  }

  // no guarantees
  public Substitution unify(Term t, Substitution s) {
    if (t instanceof Variable) {
      return t.apply(s).unifyRecApplied(this, s);
    } else if (t instanceof Function) {
      Function f = (Function) t;
      // TODO: If we're always sure that failFast has been called, we
      // don't need the following test.
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        while (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
//            System.out.println("About to unify " + t1 + "\nand " + t2 +
//                               "\nunder " + s);
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

  // no guarantees
  public Substitution unify(Term t, Substitution s,
                            Substitution bvs) {
    if (t instanceof Variable) {
      Term bvT = t.apply(bvs);
      if (bvT == t)
        return t.apply(s).unifyRecApplied(this, s, bvs);
      else if (this.equals(bvT))
        return s;
      else
        return null;
    } else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
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
    } else
      return null;
  }

  // no guarantees
  public Substitution unify(Term t, Substitution s, DSTGraph cc) {
    if (t instanceof Variable) {
      return t.apply(s).unifyRecApplied(this, s, cc);
    } else if (t instanceof Function) {
      Function f = (Function) t;
      // TODO: If we're always sure that failFast has been called, we
      // don't need the following test.
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
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
      } else { // if (cc.equivalent(this.apply(s), f.apply(s))) {
        Substitution value = cc.unifiable(this.apply(s), f.apply(s), s);
        if (value != null) {
          cc.setUsed(true);
          return value;
        } else
          return null;
      }
    } else
      return null;
  }

  // no guarantees
  public Substitution unify(Term t, Substitution s,
                            Substitution bvs, DSTGraph cc) {
    if (t instanceof Variable) {
      Term bvT = t.apply(bvs);
      if (bvT == t)
        return t.apply(s).unifyRecApplied(this, s, bvs, cc);
      else if (this.equals(bvT))
        return s;
      else
        return null;
    } else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
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
      } else { // if (cc.equivalent(this.apply(s), f.apply(s))) {
        Substitution value = cc.unifiable(this.apply(s), f.apply(s), s);
        if (value != null) {
          cc.setUsed(true);
          return value;
        } else
          return null;
      }
    } else
      return null;
  }

  Substitution unifyRecApplied(Term t, Substitution s) {
    if (t instanceof Variable) {
      return this.unifyApplied(t.apply(s), s);
    } else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyRecApplied(t2, s);
          while (receiver.hasNext()) {
            if (s == null)
              return null;
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s);
          }
        }
        return s;
      } else
        return null;
    } else
      return null;
  }

  Substitution unifyRecApplied(Term t, Substitution s,
                               Substitution bvs) {
    if (t instanceof Variable) {
      Term bvT = t.apply(bvs);
      if (bvT == t)
        return this.unifyApplied(t.apply(s), s, bvs);
      else if (this.equals(bvT))
        return s;
      else
        return null;
    } else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyRecApplied(t2, s, bvs);
          while (receiver.hasNext()) {
            if (s == null)
              return null;
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, bvs);
          }
        }
        return s;
      } else
        return null;
    } else
      return null;
  }

  Substitution unifyRecApplied(Term t, Substitution s, DSTGraph cc) {
    if (t instanceof Variable) {
      return this.unifyApplied(t.apply(s), s, cc);
    } else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyRecApplied(t2, s, cc);
          while (receiver.hasNext()) {
            if (s == null)
              return null;
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, cc);
          }
        }
        return s;
      } else { // if (cc.equivalent(this, f.apply(s))) {
        Substitution value = cc.unifiable(this, f.apply(s), s);
        if (value != null) {
          cc.setUsed(true);
          return value;
        } else
          return null;
      }
    } else
      return null;
  }

  Substitution unifyRecApplied(Term t, Substitution s,
                               Substitution bvs, DSTGraph cc) {
    if (t instanceof Variable) {
      Term bvT = t.apply(bvs);
      if (bvT == t)
        return this.unifyApplied(t.apply(s), s, bvs, cc);
      else if (this.equals(bvT))
        return s;
      else
        return null;
    } else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor())) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyRecApplied(t2, s, bvs, cc);
          while (receiver.hasNext()) {
            if (s == null)
              return null;
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, bvs, cc);
          }
        }
        return s;
      } else { // if (cc.equivalent(this, f.apply(s))) {
        Substitution value = cc.unifiable(this, f.apply(s), s);
        if (value != null) {
          cc.setUsed(true);
          return value;
        } else
          return null;
      }
    } else
      return null;
  }

  public Substitution unifyApplied(Term t, Substitution s) {
    if (t instanceof Variable) {
      return t.unifyApplied(this, s);
    } else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyApplied(t2, s);
          while (receiver.hasNext()) {
            if (s == null)
              return null;
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s);
          }
        }
        return s;
      } else
        return null;
    } else
      return null;
  }

  Substitution unifyApplied(Term t, Substitution s,
                            Substitution bvs) {
    if (t instanceof Variable)
      return t.unifyApplied(this, s, bvs);
    else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyApplied(t2, s, bvs);
          while (receiver.hasNext()) {
            if (s == null)
              return null;
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, bvs);
          }
        }
        return s;
      } else
        return null;
    } else
        return null;
  }

  public Substitution unifyApplied(Term t, Substitution s, DSTGraph cc) {
    if (t instanceof Variable) {
      return t.unifyApplied(this, s, cc);
    } else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyApplied(t2, s, cc);
          while (receiver.hasNext()) {
            if (s == null)
              return null;
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, cc);
          }
        }
        return s;
      } else { // if (cc.equivalent(this, f)) {
        Substitution value = cc.unifiable(this, f, s);
        if (value != null) {
          cc.setUsed(true);
          return value;
        } else
          return null;
      }
    } else
      return null;
  }

  Substitution unifyApplied(Term t, Substitution s,
                            Substitution bvs, DSTGraph cc) {
    if (t instanceof Variable)
      return t.unifyApplied(this, s, bvs, cc);
    else if (t instanceof Function) {
      Function f = (Function) t;
      if (f.getConstructor().equals(this.getConstructor()) &&
          f.args.size() == this.args.size()) {
        Iterator receiver = this.arguments();
        Iterator argument = f.arguments();
        // t1 iterates through the child terms of the receiver.
        Term t1;
        // t2 iterates through the child terms of the argument.
        Term t2;
        if (receiver.hasNext()) {
          t1 = (Term) receiver.next();
          t2 = (Term) argument.next();
          s = t1.unifyApplied(t2, s, bvs, cc);
          while (receiver.hasNext()) {
            if (s == null)
              return null;
            t1 = (Term) receiver.next();
            t2 = (Term) argument.next();
            s = t1.unify(t2, s, bvs, cc);
          }
        }
        return s;
      } else { // if (cc.equivalent(this, f)) {
        Substitution value = cc.unifiable(this, f, s);
        if (value != null) {
          cc.setUsed(true);
          return value;
        } else
          return null;
      }
    } else
        return null;
  }

  // must NOT modify the receiver.
  public Term apply(Substitution s) {
    Iterator it = this.args.iterator();
    List newV = new ArrayList(this.args.size());
    while (it.hasNext()) {
      newV.add(((Term) it.next()).apply(s));
    }
    return new Function((FunctionConstructor) this.getConstructor(), newV);
  }

  protected String printArgs() {
    return this.printArgs(" ");
  }

  protected String printArgs(String separator) {
    if (this.args == null)
      return "";
    else {
      Iterator it = this.args.iterator();
      StringBuffer sb = new StringBuffer();
      if (it.hasNext())
        sb.append(it.next().toString());
      while (it.hasNext())
        sb.append(separator + it.next().toString());
      return sb.toString();
    }
  }

  public String toString() {
    return "(" + this.getConstructor() + " " + this.printArgs() + ")";
  }

  public static Function getDummyFunction(List args) {
    return new Function(args);
  }

  public static Function createFunction(FunctionConstructor fs,
                                        List args) {
    return new Function(fs, args);
  }

  public Function(String name) {
    this((FunctionConstructor)
         Symbol.putOrGet(FunctionConstructor.class, name));
  }

  private Function(FunctionConstructor s) {
    this(s, null);
  }

  private Function(List args) {
    this(FunctionConstructor.DUMMY, args);
  }

  private Function(FunctionConstructor s, List args) {
    super(s);
    this.args = (args == null ? java.util.Collections.EMPTY_LIST : args);
    this.computeHashCode();
  }

} // Function
