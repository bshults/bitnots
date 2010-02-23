package bitnots.tableaux;

import bitnots.expressions.*;
import java.util.*;

/**
 *
 * @author Daniel W. Farmer
 * @version 1.1
 */

public abstract class PredicateFormula extends TableauFormula {

  /** used to register a class member predicate formula */
  final private static int CLASS_MEMBER = 1;

  int type;

  /**
   * This method implements the comprehension trigger rule.
   * @param ct The class term used in the creation of the new formula.
   * TODO lengthen javadoc, explain more...
   * TODO implement with new api changes.
   */
  public final void trigger(ClassTerm ct) {
/*
    // create the new formula's constructor
    PredicateConstructor ps =
        (PredicateConstructor) Symbol.putOrGet(
        PredicateConstructor.class, "a-member-of");

    // get the arguments
    ArrayList list = new ArrayList();
    Predicate p = (Predicate) this.formula;
    list.add(p.arguments().next());
    list.add(ct);

    // create the formula
    Formula newFormula =
        FormulaFactory.getFormula(ps, new Object[] {list});

    // create the node
    final TableauNode current = this.getBirthPlace();
    TableauNode child = current.spliceUnder();
    //this.addUsedAt(child);  // TODO: think about this

    // create the new TableauFormula
    ArrayList<TableauFormula> newParentList = new ArrayList<TableauFormula>(this.parents);
    newParentList.add(this); // update the parent list
    TableauFormula tf = TableauFormula.createTableauFormula(newFormula,
        this.getSign(), this.involvedSplits, newParentList,
        this.getSubstitutionDependency());

    // give the new TableauFormula to the TableauNode
    tf.insertInto(child);

    this.getBirthPlace().getTableau().removeComprehensionTrigger(this);*/
  }

  public final void expand() {
    // this can only be expanded if it is a class member predicate
    if ((this.type & PredicateFormula.CLASS_MEMBER) != 0) {
      this.attachChildNodes();
      this.getBirthPlace().getTableau().
          removeClassMemberPredicate(this);
    }
  }

  protected final void attachChildNodes() {
    final TableauNode current = this.getBirthPlace();

    // Since this is an alpha-type, createChildren will return a singleton
    Collection<TableauFormula> forms = this.createChildren().iterator().next();
    
    // splice new child node into its place.
    TableauNode child = current.spliceUnder(forms);
    this.addUsedAt(child);

    if (child.enforceRegularity())
      this.unregisterWithTableau(current.getTableau());
  }

  protected Collection<Collection<TableauFormula>> createChildren() {

    // see if this is registered as a class member predicate
    if ((this.type & PredicateFormula.CLASS_MEMBER) != 0) {
      // (is-a-member-of x (the-class-of-all (y) (p))) is replaced by
      // (p - with bound variables replaced by x)
      List args = ((Predicate) this.formula).getArgs();
      ClassTerm classT = (ClassTerm) args.get(1);
      ArrayList newParentList = new ArrayList(this.parents);
      newParentList.add(this); // update the parent list
      // create the child formula
      Formula newFormula = classT.getBody().
          replaceUnboundOccurrencesWith(classT.getBoundVar(), (Term) args.get(0));
      TableauFormula tf = TableauFormula.createTableauFormula(newFormula,
          this.getSign(), this.involvedSplits, newParentList,
          this.getSubstitutionDependency());

      return Collections.singleton((Collection<TableauFormula>) Collections.singleton(tf));
    }
    return Collections.EMPTY_LIST;
  }

  public final void unregisterWithTableau(Tableau pn) {
    // if this matches the right format, register it as a
    // class member predicate with the tableau
    Predicate p = (Predicate)this.formula;
    if (p.getConstructor().getName().equals("a-member-of")) {
      List l = p.getArgs();
      if (l.size() >= 2) {
        // the second argument's name should be
        // in the form of "the-class-of-all"
        Term t = (Term) l.get(1);
        if (t instanceof ComplexTerm) {
          ComplexTerm ct = (ComplexTerm) t;
          if (ct.getConstructor().getName().equals("the-class-of-all")) {
            // this PredicateFormula is a class member predicate
            // remember it as such
            this.type |= PredicateFormula.CLASS_MEMBER;
            pn.removeClassMemberPredicate(this);
            return;
          }
        }

        // check list of class member equalities to look for
        // a potential comprehension schema trigger
        Iterator classTermEQit = pn.getClassMemberEQs().iterator();
        while (classTermEQit.hasNext()) {
          Predicate pred = (Predicate) classTermEQit.next();
          if (pred.contains(t)) {
            List args = pred.getArgs();
            ClassTerm ct = null;
            if (args.get(0) instanceof ClassTerm)
              ct = (ClassTerm) args.get(0);
            else if (args.get(1) instanceof ClassTerm)
              ct = (ClassTerm) args.get(1);
            else
              throw new IllegalStateException("PredicateFormula " +
                                              "didn't find a " +
                                              "ClassTerm in the " +
                                              "expected location");
            pn.removeComprehensionTrigger(this);
          }
        }
      }
    }
  }

  protected final void registerWithTableau(Tableau pn) {
    // if this matches the right format, register it as a
    // class member predicate with the tableau
    Predicate p = (Predicate)this.formula;
    if (p.getConstructor().getName().equals("a-member-of")) {
      List l = p.getArgs();
      if (l.size() >= 2) {
        // the second argument's name should be
        // in the form of "the-class-of-all"
        Term t = (Term) l.get(1);
        if (t instanceof ComplexTerm) {
          ComplexTerm ct = (ComplexTerm) t;
          if (ct.getConstructor().getName().equals("the-class-of-all")) {
            // this PredicateFormula is a class member predicate
            // remember it as such
            this.type |= PredicateFormula.CLASS_MEMBER;
            pn.addClassMemberPredicate(this);
            return;
          }
        }

        // check list of class member equalities to look for
        // a potential comprehension schema trigger
        Iterator classTermEQit = pn.getClassMemberEQs().iterator();
        while (classTermEQit.hasNext()) {
          Predicate pred = (Predicate) classTermEQit.next();
          if (pred.contains(t)) {
            List args = pred.getArgs();
            ClassTerm ct = null;
            if (args.get(0) instanceof ClassTerm)
              ct = (ClassTerm) args.get(0);
            else if (args.get(1) instanceof ClassTerm)
              ct = (ClassTerm) args.get(1);
            else
              throw new IllegalStateException("PredicateFormula " +
                                              "didn't find a " +
                                              "ClassTerm in the " +
                                              "expected location");
            pn.addComprehensionTrigger(this, ct);
          }
        }
      }
    }
  }

  PredicateFormula(Formula f, FormulaHelper help) {
    super(f, help);
  }
} // PredicateFormula
