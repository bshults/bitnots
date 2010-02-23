package bitnots.expressions;

import java.io.*;
import java.util.*;
import bitnots.util.*;

/**
 * An idempotent substitution.  Instances should be immutable though a
 * malicious client could break that.
 *
 * @author Benjamin Shults
 * @version .2
 */

public class Substitution implements Subst {

  private final static Substitution IDENTITY = new Substitution();

  /**
   * Used to indicate that two expressions are not unifiable without
   * equality.
   */
  public static final int CLASH = 0;

  /**
   * Used to indicate that two expressions have no obvious clash and
   * are not alpha-congruent but may or may not be unifiable.
   */
  public static final int DIFFERENCE = 1;

  /**
   * Used to indicate that two expressions are alpha-congruent.
   */
  public static final int ALPHA = 2;

  /**
   * Used to indicate that a clash due to variable occurrence.
   */
  // The problem with using this is that things don't fail as fast.
  public static final int OCCURRENCE = 3;

  /**
   * Used to indicate that two expressions clash due to a problem with
   * the bound variables.
   */
  public static final int BV = 4;

  protected Subst state = null;

  public int size() {
    return this.state.size();
  }

  public Object clone() {
    try {
      return super.clone();
    } catch (CloneNotSupportedException cnse) {
      cnse.printStackTrace();
      return null;
    }
  }

  public boolean equals(Object o) {
    if (o instanceof Substitution &&
        this.size() == ((Substitution) o).size()) {
      Iterator it = this.iterator();
      while (it.hasNext()) {
        Map.Entry pair = (Map.Entry) it.next();
        Variable v = (Variable) pair.getKey();
        if (!pair.getValue().equals(((Term) v).apply((Substitution) o))) {
          return false;
        }
      }
      return true;
    } else
      return false;
  }

  public Iterator iterator() {
    return this.state.iterator();
  }

  /*public int compareTo(Object o) {
    return this.state.compareTo(o);
  }*/

  public boolean containsKey(Variable v) {
    return this.state.containsKey(v);
  }

  public boolean isEmpty() {
    return this == Substitution.IDENTITY;
//    return this.state.isEmpty();
  }

  public String toString() {
    return this.state.toString();
  }

  /**
   * Create a new Substitution consisting of the receiver and the new
   * pair.  The return value is idempotent.  Also, this does
   * <b>not</b> <b>modify</b> the receiver.  The caller guarantees
   * that the receiver does not map <b>v</b> to anything.  The caller
   * guarantees that the receiver does not map anything to <b>v</b>.
   * The caller guarantees that the receiver is nilpotent on <b>t</b>.
   * @param v the Variable to be associated with <b>t</b>.
   * @param t the Term that will replace <b>v</b>.
   * @return a new Substitution consisting of the receiver and the
   * new pair.  The return value is idempotent. */
  public Substitution acons(Variable v, Term t) {
    return this.state.acons(v, t);
  }

  public Substitution removeSequentVars(final HashMap varMap) {
    if (this.isEmpty())
      return this;
    else {
      Iterator it = ((DefaultSubstitutionModel)
                     ((NonEmptySubstitution)
                      Substitution.this.state).model).entrySet().iterator();
      DefaultSubstitutionModel m = new DefaultSubstitutionModel(5);
      while (it.hasNext()) {
        Map.Entry pair = (Map.Entry) it.next();
        Variable v = (Variable) pair.getKey();
        if (!v.isSequentVar())
          m.put(v, ((Term) pair.getValue()).replaceVariables(varMap));
      }
      if (m.isEmpty())
        return Substitution.createSubstitution();
      else
        return new Substitution(m);
    }
  }

  /**
   * Return a substitution identical to the receiver except that all
   * variables that are keys in the given map are replaced with their
   * values in the map.  This is used when bringing a sequent into the
   * tableau from the theory.
   */
  public Substitution replaceVariables(HashMap map) {
    if (this.isEmpty())
      return this;
    else {
      DefaultSubstitutionModel oldMap =
        ((NonEmptySubstitution) this.state).model;
      DefaultSubstitutionModel newMap =
        new DefaultSubstitutionModel(2 * oldMap.size());
      Iterator oldEntries = oldMap.entrySet().iterator();
      while (oldEntries.hasNext()) {
        Map.Entry pair = (Map.Entry) oldEntries.next();
        Variable oldV = (Variable) pair.getKey();
        Variable newV = (Variable) oldV.replaceVariables(map);
        Term newT = ((Term) pair.getValue()).replaceVariables(map);
        newMap.put(newV, newT);
      }
      return new Substitution(newMap);
    }
  }

  /**
   * Returns <b>v</b> if <b>v</b> is not mapped by the receiver.
   * Otherwise, returns the Term to which <b>v</b> is mapped by the
   * receiver.
   * @param v the Variable to be looked up.
   * @return <b>v</b> if <b>v</b> is not mapped by the receiver.
   * Otherwise, returns the Term to which <b>v</b> is mapped by the
   * receiver.  */
  public Term get(Variable v) {
    return this.state.get(v);
  }

  /**
   * Creates and returns a substitution that maps each variable in
   * <b>bvList</b> to a different new constant term. */
  public static Substitution createBoundVarSubstitution(Collection bvList) {
    Iterator it1 = bvList.iterator();
    Function temp = Function.createTemporaryConstant();
    DefaultSubstitutionModel map =
      new DefaultSubstitutionModel((int) (1.75 * bvList.size() + 1));
    map.put((Variable) it1.next(), temp);
    while (it1.hasNext()) {
      temp = Function.createTemporaryConstant();
      map.put((Variable) it1.next(), temp);
    }
    return new Substitution(map);
  }

  /**
   * Creates and returns a substitution that maps <b>var1</b> and
   * <b>var2</b> to the same new constant.
   * @param var1 a bound variable.
   * @param var2 a bound variable.
   * @return a new Substitution that maps <b>var1</b> and <b>var2</b>
   * to the same new constant.
   */
  public static Substitution createBoundVarSubstitution(Variable var1,
                                                        Variable var2) {
    Function temp = Function.createTemporaryConstant();
    DefaultSubstitutionModel map = new DefaultSubstitutionModel(4);
    map.put(var1, temp);
    map.put(var2, temp);
    return new Substitution(map);
  }

  /**
   * Extends <b>bvs</b> and returns a substitution that maps each
   * variable in <b>bvList</b> to a different new constant term.  It's
   * OK if an element of <b>bvList</b> is mapped by <b>bvs</b>.
   * */
  public static Substitution createBoundVarSubstitution(
    Collection bvList, Substitution bvs) {
    Iterator it1 = bvList.iterator();
    Function temp = Function.createTemporaryConstant();
    bvs = bvs.acons((Variable) it1.next(), temp);
    while (it1.hasNext()) {
      temp = Function.createTemporaryConstant();
      ((NonEmptySubstitution) bvs.state).model.put(
        (Variable) it1.next(), temp);
    }
    return bvs;
  }

  /**
   * Creates and returns a substitution that maps each variable in
   * bvList1 to a different new constant term.  This substitution also
   * maps the corresponding elements of bvList2 to the same constants
   * in the same order.  This returns null only if the arguments are
   * not the same size. */
  public static Substitution createBoundVarSubstitution(Collection bvList1,
                                                        Collection bvList2) {
    if (bvList1.size() != bvList2.size())
      return null;
    Iterator it1 = bvList1.iterator();
    Iterator it2 = bvList2.iterator();
    Function temp = Function.createTemporaryConstant();
    Substitution bvs = new Substitution((Variable) it1.next(), temp);
    ((NonEmptySubstitution) bvs.state).model.put(
      (Variable) it2.next(), temp);
    while (it1.hasNext()) {
      temp = Function.createTemporaryConstant();
      ((NonEmptySubstitution) bvs.state).model.put((Variable) it1.next(),
                                                   temp);
      ((NonEmptySubstitution) bvs.state).model.put((Variable) it2.next(),
                                                   temp);
    }
    return bvs;
  }

  /**
   * This extends <b>bvs</b> and returns a substitution that maps each
   * variable in bvList1 to a different new constant term.  This
   * substitution also maps the corresponding elements of bvList2 to
   * the same constants in the same order.  This returns null only if
   * the arguments are not the same size. */
  public static Substitution createBoundVarSubstitution(
    Collection bvList1, Collection bvList2, Substitution bvs) {
    if (bvList1.size() != bvList2.size())
      return null;
    Iterator it1 = bvList1.iterator();
    Iterator it2 = bvList2.iterator();
    Function temp = Function.createTemporaryConstant();
    bvs = bvs.acons((Variable) it1.next(), temp);
    ((NonEmptySubstitution) bvs.state).model.put(
      (Variable) it2.next(), temp);
    while (it1.hasNext()) {
      temp = Function.createTemporaryConstant();
      ((NonEmptySubstitution) bvs.state).model.put(
        (Variable) it1.next(), temp);
      ((NonEmptySubstitution) bvs.state).model.put(
        (Variable) it2.next(), temp);
    }
    return bvs;
  }

  /**
   * This extends <b>bvs</b> and returns a substitution that maps the
   * two variables to a new constant term. */
  public static Substitution createBoundVarSubstitution(
    Variable bv1, Variable bv2, Substitution bvs) {
    Function temp = Function.createTemporaryConstant();
    bvs = bvs.acons(bv1, temp);
    ((NonEmptySubstitution) bvs.state).model.put(bv2, temp);
    return bvs;
  }

  /**
   * Creates and returns a Substitution that maps the vars to the
   * terms in the order returned by their iterators.
   * @exception IllegalArgumentException if the same variable occurs
   * twice in <b>vars</b> or if the two Collections are not the same
   * size.  */
  public static Substitution createSubstitution(Collection vars,
                                                Collection terms) {
    if (vars.size() != terms.size())
      throw new IllegalArgumentException("sizes don't agree");
    Iterator varsIt = vars.iterator();
    Iterator termsIt = terms.iterator();
    //    Function temp = Function.createTemporaryConstant();
    //    bvs = bvs.acons((Variable) varsIt.next(), temp);
    DefaultSubstitutionModel map =
      new DefaultSubstitutionModel((int) (1.75 * vars.size() + 1));
    while (varsIt.hasNext()) {
      if (map.put(varsIt.next(), termsIt.next()) != null)
        throw new IllegalArgumentException("The same variable occurs twice!");
    }
    return new Substitution(map);
  }

  /**
   * Creates and returns a new Substitution that maps <b>s</b> to
   * <b>t</b>.  */
  public static Substitution createSubstitution(Variable s, Term t) {
    return new Substitution(s, t);
  }

  /**
   * Returns the identity Substitution. */
  public static Substitution createSubstitution() {
    return Substitution.IDENTITY;
  }

  /**
   * If the two args are compatible, then return their composition.
   * This relies on the fact that the keyset and values are iterated
   * over in the same order.  TODO: get rid of this assumption.
   */
  // TODO give some guarantee whether this is s1 s2 or s2 s1.
  public static Substitution compatible(Substitution s1, Substitution s2) {
    if (s2.isEmpty() || s2 == s1)
      return s1;
    else if (s1.isEmpty())
      return s2;
    else {
      NonEmptySubstitution ns1 = (NonEmptySubstitution) s1.state;
      List keys = new ArrayList(ns1.model.keySet());
      List values = new ArrayList(ns1.model.values());
      Function f1 = Function.getDummyFunction(keys);
      Function f2 = Function.getDummyFunction(values);
      return f1.unify(f2, s2);
    }
  }

  /**
   * Returns the composition of two idempotent substitutions.  If
   * either is empty, then the other is returned.  This does not
   * modify either.  The caller guarantees they are compatible.
   * @param s 
   * @param t 
   * @return 
   */
  // TODO: why is this here?  Isn't it the same as compatible?
  // It may be a bit more efficient.
  // TODO: I don't like the precondition on a public method.
  // TODO give some guarantee whether this is s1 s2 or s2 s1.
  public static Substitution compose(Substitution s, Substitution t) {
    if (t == null || s == null)
      return null;
    if (s == t || s.isEmpty())
      return t;
    else if (t.isEmpty())
      return s;
    else {
      Substitution subValue = Substitution.IDENTITY;
//      DefaultSubstitutionModel newModel = new DefaultSubstitutionModel();
      Iterator sIt = s.iterator();
      // Apply t to the values in s.
      Map.Entry pair;
      while (sIt.hasNext()) {
        pair = (Map.Entry) sIt.next();
        final Variable key = (Variable) pair.getKey();
        final Term value = ((Term) pair.getValue()).apply(t);
        // TODO: Don't include entries that are nilpotent.  This is
        // only possible since I am still not enforcing an ordering on
        // variables.
        if (!value.equals(key))
          subValue = subValue.acons(key, value);
      }
      // Include entries from t that don't conflict with entries in s.
      Iterator tIt = t.iterator();
      while (tIt.hasNext()) {
        pair = (Map.Entry) tIt.next();
        if (!subValue.containsKey((Variable) pair.getKey()))
          subValue = subValue.acons((Variable) pair.getKey(),
                                    (Term) pair.getValue());
      }
      return subValue;
    }
  }

  private Substitution() {
    this.state = EmptySubstitution.INSTANCE;
  }

  /**
   * The caller guarantees v does not occur in t.
   */
  private Substitution(Variable v, Term t) {
    this.state = new NonEmptySubstitution(v, t);
    // TODO: get rid of this once I've debugged.
    if (t.contains((Variable) v))
      throw new IllegalArgumentException(v + " occurs in " + t);
  }

  /**
   * This does NOT copy table.
   * The caller guarantees idempotence.
   */
  private Substitution(DefaultSubstitutionModel table) {
    // TODO: get rid of this once I've debugged.
    if (!table.isIdempotent())
      throw new IllegalArgumentException("given map is not idempotent");
    this.state = new NonEmptySubstitution(table);
  }

  protected class NonEmptySubstitution implements Subst {

    // This model sacrifices space for speed.  This does not allow
    // shared structure.

    protected DefaultSubstitutionModel model;

    public int size() {
      return this.model.size();
    }

    /*public int compareTo(Object o) {
      if (o instanceof EmptySubstitution)
        return 1;
      else {
        assert (o instanceof NonEmptySubstitution);
        NonEmptySubstitution s = (NonEmptySubstitution) o;
        return this.model.compareTo(s.model);
      }
    }*/

    public boolean isEmpty() {
      return false;
    }

    public boolean containsKey(Variable v) {
      return this.get(v) != v;
    }

    /**
     * The next method of this iterator must return instances of
     * Map.Entry.  The remove method must throw an
     * UnsupportedOperationException.
     */
    public Iterator iterator() {
      return this.model.iterator();
    }

    public String toString() {
      StringBuffer sb = new StringBuffer();
      Iterator it = this.iterator();
      Map.Entry pair = (Map.Entry) it.next();
      sb.append("{");
      sb.append(pair.getKey().toString());
      sb.append(" / ");
      sb.append(pair.getValue().toString());
      while (it.hasNext()) {
        pair = (Map.Entry) it.next();
        sb.append(", ");
        sb.append(pair.getKey().toString());
        sb.append(" / ");
        sb.append(pair.getValue().toString());
      }
      sb.append("}");
      return sb.toString();
    }

    /**
     * Create a new Substitution consisting of the receiver and the new
     * pair.  The return value is idempotent.  Also, this does
     * <b>not</b> <b>modify</b> the receiver.  The caller guarantees
     * that the receiver does not map <b>v</b> to anything.  The caller
     * guarantees that the receiver does not map anything to <b>v</b>.
     * The caller guarantees that the receiver is nilpotent on <b>t</b>.
     * @param v the Variable to be associated with <b>t</b>.
     * @param t the Term that will replace <b>v</b>.
     * @return a new Substitution consisting of the receiver and the
     * new pair.  The return value is idempotent. */
    public Substitution acons(Variable v, Term t) {
      if (t instanceof Variable && v.compareTo(t) > 0) {
        Term temp = v;
        v = (Variable) t;
        t = temp;
      }
      DefaultSubstitutionModel newMap =
        new DefaultSubstitutionModel(
          (int) (1.75 * (this.model.size() + 1)) + 1);
      // according to the preconditions of this method, the following
      // is not necessary.
      //      t = t.apply(Substitution.this);
      Iterator sIt = this.model.iterator();
      Map.Entry pair;
      while (sIt.hasNext()) {
        pair = (Map.Entry) sIt.next();
        final Variable key = (Variable) pair.getKey();
        final Term newValue =
          ((Term) pair.getValue()).replaceUnboundOccurrencesWith(v, t);
        newMap.put(key, newValue);
      }
      newMap.put(v, t);
      return new Substitution(newMap);
    }

    public Term get(Variable v) {
      return (Term) this.model.get(v);
    }

    // Preconditions: v and t must not be null and v must not occur in
    // t.
    protected NonEmptySubstitution(Variable v, Term t) {
      this.model = new DefaultSubstitutionModel(5);
      this.model.put(v, t);
    }

    protected NonEmptySubstitution(DefaultSubstitutionModel table) {
      this.model = table;
    }
  }

  private static class DefaultSubstitutionModel extends HashMap
   implements Cloneable {//, Comparable {

//      public Object clone() {
//        try {
//          return super.clone();
//        } catch (CloneNotSupportedException cnse) {
//          cnse.printStackTrace();
//          return null;
//        }
//      }

    /**
     * The next method of this iterator must return instances of
     * Map.Entry.  The remove method must throw an
     * UnsupportedOperationException.
     */
    public Iterator iterator() {
      return new Iterator() {
          private Iterator it =
            DefaultSubstitutionModel.this.entrySet().iterator();
          public boolean hasNext() {
            return this.it.hasNext();
          }
          public Object next() {
            return this.it.next();
          }
          public void remove() {
            throw new UnsupportedOperationException();
          }
        };
    }

    /**
     * @return the term associated with <b>o</b> or <b>o</b> if there
     * is none.
     * @exception ClassCastException if <b>o</b> is not Variable.
     * */
    public Object get(Object o) {
      Object retVal = super.get((Variable) o);
      return retVal == null ? o : retVal;
    }

    /*public int compareTo(Object o) {
      DefaultSubstitutionModel dsm = (DefaultSubstitutionModel) o;
      if (this.size() > dsm.size())
        return 1;
      else if (this.size() == dsm.size())
        return -0;
      else
        return -1;
    }*/

    public boolean containsKey(Variable v) {
      return this.containsKey(v);
    }

    /**
     * This also returns true if there is a trivial pair.
     */
    private boolean isIdempotent() {
      Iterator values = this.values().iterator();
      while (values.hasNext()) {
        Iterator keys = this.keySet().iterator();
        while (keys.hasNext()) {
          if (((Term) values.next()).contains((Variable) keys.next()))
            return false;
        }
      }
      return true;
    }

    DefaultSubstitutionModel(int initialCapacity) {
      super(initialCapacity);
    }

    DefaultSubstitutionModel(/*Substitution s,*/ Map t) {
      super(t);
    }
  }

  private static class EmptySubstitution implements Subst {

    public int size() {
      return 0;
    }

    public Iterator iterator() {
      return new Iterator() {
          public void remove() {
            throw new UnsupportedOperationException();
          }
          public boolean hasNext() {
            return false;
          }
          public Object next() {
            throw new NoSuchElementException();
          }
        };
    }

    public boolean containsKey(Variable v) {
      return false;
    }

 /*   public int compareTo(Object o) {
      if (o instanceof NonEmptySubstitution)
        return -1;
      else
        return 0;
    }*/

    public boolean isEmpty() {
      return true;
    }

    public Term get(Variable v) {
      return v;
    }

    public String toString() {
      return "{}";
    }

    public Substitution acons(Variable v, Term t) {
      return new Substitution(v, t);
    }

    public static final EmptySubstitution INSTANCE =
      new EmptySubstitution();

    private EmptySubstitution() {}
  }
} // Substitution

interface Subst extends Serializable, Cloneable {//, Comparable {

  public int size();

  public Iterator iterator();

  public boolean containsKey(Variable v);

  /**
   * Returns true if and only if this is an identity substitution.
   *
   * @return true if and only if this is an identity substitution.
   */
  public boolean isEmpty();

  /**
   * Create a new Substitution consisting of the receiver and the new
   * pair.  The return value is idempotent.  Also, this does
   * <b>not</b> <b>modify</b> the receiver.  The caller guarantees
   * that the receiver does not map <b>v</b> to anything.  The caller
   * guarantees that the receiver does not map anything to <b>v</b>.
   * The caller guarantees that the receiver is nilpotent on <b>t</b>.
   *
   * @param v the Variable to be associated with <b>t</b>.
   * @param t the Term that will replace <b>v</b>.
   * @return a new Substitution consisting of the receiver and the new
   * pair.  The return value is idempotent.
   */
  public Substitution acons(Variable v, Term t);

  /**
   * Return the term associated with <b>v</b> in this substitution.
   * Returns <b>v</b> if there is no association.
   *
   * @param v the substitutable whose associated term is sought.
   * @return the term associated with <b>v</b> in this substitution or
   * <b>v</b> if there is no association.
   */
  public Term get(Variable v);

}

