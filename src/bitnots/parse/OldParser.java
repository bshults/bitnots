package bitnots.parse;

import bitnots.expressions.*;
import bitnots.theories.*;
import bitnots.util.*;
import java.io.*;
import java.util.*;

/**
 *
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class OldParser {

  protected Tokenizer tokenizer;

  // the value "" means that the end of the file has been reached.
  /**
   * This will always be the first token of the next expression to be
   * read or "" if there are no more.
   */
  protected String currentToken = null;

  public static final String OPEN_PAREN = "(";
  public static final String CLOSED_PAREN = ")";

  public String getCurrentToken() {
    return this.currentToken;
  }

  /**
   * @exception NoSuchElementException if there are no more tokens.
   */
  public void nextToken() throws IOException {
//    System.out.println(this.currentToken);
    if (this.hasNextToken())
      this.currentToken = this.tokenizer.nextToken();
    else {
      this.currentToken = "";
      throw new NoSuchElementException();
    }
  }

  public boolean hasNextToken() throws IOException {
    return this.tokenizer.hasNextToken();
  }

  public boolean isEOF() {
    return this.currentToken == "";
  }

  /**
   * @exception NoSuchElementException if the match is unsuccessful
   * due to the fact that there are no more tokens.
   */
  public boolean match(String s) throws IOException, EOFException {
    if (s == this.currentToken) {
//       try {
//         this.nextToken();
//       } catch (NoSuchElementException nsee) {}
      return true;
    } else if (this.isEOF())
      throw new EOFException();
    else
      return false;
  }

  public boolean isSymbol() {
    return this.currentToken != OPEN_PAREN &&
      this.currentToken != CLOSED_PAREN;
  }

  /**
   * This should leave currentToken at the beginning of the next
   * expression.
   */
  public ComplexTerm readTerm() throws WrongNonterminalException, IOException,
    NestedBindingException {
    if (this.match(OPEN_PAREN)) {
      this.nextToken();
      if (this.isSymbol()) {
        return this.readTermOpen(new ArrayList(3));
      } else
        throw new WrongNonterminalException(this.currentToken);
    } else if (this.isSymbol()) {
      String constr = this.currentToken;
      this.nextToken();
      return Function.getConstant(constr);
    } else
      throw new WrongNonterminalException(this.currentToken);
  }

  /**
   * This should leave currentToken at the beginning of the next
   * expression.
   */
  // Currently, this turns variables into constants and everything
  // else as a function.
  public ComplexTerm readTermOpen(ArrayList bvs)
    throws WrongNonterminalException, IOException, NestedBindingException {
    String constr = this.currentToken;
    if (this.match("the") || this.match("the-class-of-all")) {
      this.nextToken();

      // TODO: take care of clashes!
      Variable currentBV = this.readBoundVar();
      bvs.add(currentBV);
      Formula f = null;
      if (this.match(OPEN_PAREN)) {
        this.nextToken();
        f = this.readFormulaOpen(bvs);
        if (this.match(CLOSED_PAREN)) {
          this.nextToken();
          return TermFactory.getTerm(
            (TermConstructor) Symbol.putOrGet(TermConstructor.class, constr),
            new Object[] {currentBV, f});
        } else
          throw new WrongNonterminalException(this.currentToken);
      } else
        throw new WrongNonterminalException(this.currentToken);
//     } else if (this.match("=")) {
//       this.nextToken();
    } else {
      this.nextToken();
      FunctionConstructor fs =
        (FunctionConstructor)
        Symbol.putOrGet(FunctionConstructor.class, constr);
      ArrayList list = new ArrayList();
      while (true) {
        if (this.match(OPEN_PAREN)) {
          this.nextToken();
          if (this.isSymbol())
            list.add(this.readTermOpen(bvs));
          else
            throw new WrongNonterminalException(this.currentToken);
        } else if (this.isSymbol()) {
          constr = this.currentToken;
          this.nextToken();
          //
          Iterator it = bvs.iterator();
          Variable match = null;
          while (it.hasNext()) {
            Variable temp = (Variable) it.next();
            if (temp.getExternalName() == constr) {
              match = temp;
              break;
            }
          }
          // now match is an element of the Stack that is the same symbol as
          // constr or null if there isn't one.
          if (match == null)
            list.add(Function.getConstant(constr));
          else
            list.add(match);
        } else if (this.match(CLOSED_PAREN)) {
          this.nextToken();
          return Function.createFunction(fs, list);
        } else
          throw new WrongNonterminalException(this.currentToken);
      } // END WHILE
    }
  }

//   /**
//    * This should leave currentToken at the beginning of the next
//    * expression.
//    */
//   public String readString() throws IOException, WrongNonterminalException {
//     return this.currentToken = this.tokenizer.readString();
//   }

  /**
   * This should leave currentToken at the beginning of the next
   * expression.
   */
  // TODO: unbound variables in definitions and axioms going into the
  // KB should be left free.

  public TheoryElement readDefun() throws IOException,
    WrongNonterminalException, NestedBindingException {
    if (this.match(OPEN_PAREN)) {
      this.nextToken();
      if (this.isSymbol()) {
        String constr = this.currentToken;
        if (this.match("def-term")) {
          this.nextToken();
          ComplexTerm definiendum = this.readTerm();
          ComplexTerm definiens = this.readTerm();
          String format = this.currentToken;
          this.nextToken();
          String name = this.currentToken;
          this.nextToken();
          if (this.match(CLOSED_PAREN)) {
            this.nextToken();
            return new TermDefinition(definiendum, definiens,
                                      format, name);
          } else
            throw new WrongNonterminalException(this.currentToken);
        } else if (this.match("def-predicate")) {
          this.nextToken();
          Predicate definiendum = (Predicate) this.readFormula();
          Formula definiens = this.readFormula();
          String format = this.currentToken;
          this.nextToken();
          String name = this.currentToken;
          this.nextToken();
          if (this.match(CLOSED_PAREN)) {
            this.nextToken();
            return new PredicateDefinition(definiendum, definiens,
                                           format, name);
          } else
            throw new WrongNonterminalException(this.currentToken);
        } else if (this.match("axiom")) {
          this.nextToken();
          Formula formula = this.readFormula();
          String name = this.currentToken;
          this.nextToken();
          if (this.match(CLOSED_PAREN)) {
            this.nextToken();
            return new Axiom(formula, name);
          }
          throw new WrongNonterminalException(this.currentToken);
        } else if (this.match("theorem")) {
          this.nextToken();
          Formula formula = this.readFormula();
          String name = this.currentToken;
          this.nextToken();
          if (this.match(CLOSED_PAREN)) {
            this.nextToken();
            return new Theorem(formula, name);
          }
          throw new WrongNonterminalException(this.currentToken);
        } else if (this.match("conjecture")) {
          this.nextToken();
          Formula formula = this.readFormula();
          String name = this.currentToken;
          this.nextToken();
          if (this.match(CLOSED_PAREN)) {
            this.nextToken();
            return new Conjecture(formula, name);
          }
          throw new WrongNonterminalException(this.currentToken);
        } else
          return new Conjecture(this.readFormulaOpen(new ArrayList(3)),
                                "");
      } else
        throw new WrongNonterminalException(this.currentToken);
    } else
      throw new WrongNonterminalException(this.currentToken);
  }

  /**
   * This should leave currentToken at the beginning of the next
   * expression.
   */
  // TODO: make all these methods but one wrapper take a bvs and just
  // have the wrapper send an empty list as the argument.
  public Formula readFormula()
    throws WrongNonterminalException, IOException, NestedBindingException {
    if (this.match(OPEN_PAREN)) {
      this.nextToken();
      if (this.isSymbol()) {
        return this.readFormulaOpen(new ArrayList(3));
      } else
        throw new WrongNonterminalException(this.currentToken);
    } else
      throw new WrongNonterminalException(this.currentToken);
  }

  /**
   * This should leave currentToken at the beginning of the next
   * expression.
   * Open paren has already been read and we are on a symbol.
   */
  private Formula readFormulaOpen(ArrayList bvs)
    throws WrongNonterminalException, IOException, NestedBindingException {
    String constr = this.currentToken;
    if (this.match("forall") || this.match("for-some")) {
      this.nextToken();
      ArrayList currentBVS = this.readBoundVarList();
      ArrayList allBVS = (ArrayList) currentBVS.clone();
      allBVS.addAll(bvs);
      Formula f = null;
      if (this.match(OPEN_PAREN)) {
        this.nextToken();
        f = this.readFormulaOpen(allBVS);
        if (this.match(CLOSED_PAREN)) {
          this.nextToken();
          return FormulaFactory.getFormula(
            (FOLConstructor) Symbol.putOrGet(FOLConstructor.class, constr),
            new Object[] {currentBVS, f});
        } else
          throw new WrongNonterminalException(this.currentToken);
      } else
        throw new WrongNonterminalException(this.currentToken);
    } else if (this.match("and") || this.match("or")) {
      this.nextToken();
      PropositionalConstructor ps =
        (PropositionalConstructor)
        Symbol.putOrGet(PropositionalConstructor.class, constr);
      Collection list = new ArrayList();
      while (this.match(OPEN_PAREN)) {
        this.nextToken();
        list.add(this.readFormulaOpen(bvs));
      }
      if (this.match(CLOSED_PAREN)) {
        this.nextToken();
        return FormulaFactory.getFormula(ps, new Object[] {list});
      } else
        throw new WrongNonterminalException(this.currentToken);
    } else if (this.match("not")) {
      this.nextToken();
      PropositionalConstructor ps =
        (PropositionalConstructor)
        Symbol.putOrGet(PropositionalConstructor.class, constr);
      Collection list = new ArrayList();
      if (this.match(OPEN_PAREN)) {
        this.nextToken();
        list.add(this.readFormulaOpen(bvs));
        if (this.match(CLOSED_PAREN)) {
          this.nextToken();
          return FormulaFactory.getFormula(ps, new Object[] {list});
        } else
          throw new WrongNonterminalException(this.currentToken);
      } else
        throw new WrongNonterminalException(this.currentToken);
    } else if (this.match("iff") || this.match("implies")) {
      this.nextToken();
      PropositionalConstructor ps =
        (PropositionalConstructor)
        Symbol.putOrGet(PropositionalConstructor.class, constr);
      Collection list = new ArrayList();
      if (this.match(OPEN_PAREN)) {
        this.nextToken();
        list.add(this.readFormulaOpen(bvs));
        if (this.match(OPEN_PAREN)) {
          this.nextToken();
          list.add(this.readFormulaOpen(bvs));
          if (this.match(CLOSED_PAREN)) {
            this.nextToken();
            return FormulaFactory.getFormula(ps, new Object[] {list});
          } else
            throw new WrongNonterminalException(this.currentToken);
        } else
          throw new WrongNonterminalException(this.currentToken);
      } else
        throw new WrongNonterminalException(this.currentToken);
    } else if (this.match("falsity") || this.match("truth")) {
      this.nextToken();
      LogicalConstructor ps =
        (LogicalConstructor)
        Symbol.putOrGet(LogicalConstructor.class, constr);
      if (this.match(CLOSED_PAREN)) {
        this.nextToken();
        return FormulaFactory.getFormula(ps, null);
      } else
        throw new WrongNonterminalException(this.currentToken);
    } else {
      this.nextToken();
      PredicateConstructor ps = (PredicateConstructor)
        Symbol.putOrGet(PredicateConstructor.class, constr);
      Collection list = new ArrayList();
      while (true) {
        if (this.match(OPEN_PAREN)) {
          this.nextToken();
          list.add(this.readTermOpen(bvs));
        } else if (this.isSymbol()) {
          constr = this.currentToken;
          this.nextToken();
          Iterator it = bvs.iterator();
          Variable match = null;
          while (it.hasNext()) {
            Variable temp = (Variable) it.next();
            if (temp.getExternalName() == constr) {
              match = temp;
              break;
            }
          }
          // now match is an element of the Stack that is the same symbol as
          // constr or null if there isn't one.
          if (match == null)
            list.add(Function.getConstant(constr));
          else
            list.add(match);
        } else
          break;
      }
      if (this.match(CLOSED_PAREN)) {
        this.nextToken();
        return FormulaFactory.getFormula(ps, new Object[] {list});
      } else
        throw new WrongNonterminalException(this.currentToken);
    }
  }

  /**
   * This should leave currentToken at the beginning of the next
   * expression.
   */
  ArrayList readBoundVarList()
    throws IOException, WrongNonterminalException, NestedBindingException {
    // BROKEN
    if (this.match("(")) {
      this.nextToken();
      ArrayList bvs = new ArrayList(3);
      this.readBoundVarList(bvs);
      return bvs;
    } else
      throw new WrongNonterminalException(this.currentToken);
  }

  /**
   * This should leave currentToken at the beginning of the next
   * expression.
   */
  Variable readBoundVar()
    throws IOException, WrongNonterminalException {
    // TODO: take care of clashes!  This should be able to throw a
    // NestedBindingException.
    if (this.match(OPEN_PAREN)) {
      this.nextToken();
      if (this.isSymbol()) {
//        Variable bv = new Variable(this.currentToken);
        Variable bv = Variable.createNewBoundVar(this.currentToken);
        this.nextToken();
        if (this.match(CLOSED_PAREN)) {
          this.nextToken();
          return bv;
        } else
          throw new WrongNonterminalException(this.currentToken);
      } else
        throw new WrongNonterminalException(this.currentToken);
    } else
      throw new WrongNonterminalException(this.currentToken);
  }

/**
   * This should leave currentToken at the beginning of the next
   * expression.
   */
  void readBoundVarList(ArrayList bvs)
    throws IOException, WrongNonterminalException, NestedBindingException {
    do {
      if (this.match(OPEN_PAREN)) {
        this.nextToken();
        if (this.isSymbol()) {
//          Variable bv = new Variable(this.currentToken);
          Variable bv = Variable.createNewBoundVar(this.currentToken);
          if (bvs.contains(bv))
            throw new NestedBindingException();
          else {
            this.nextToken();
            bvs.add(bv);
            if (this.match(CLOSED_PAREN))
              this.nextToken();
            else
              throw new WrongNonterminalException(this.currentToken);
          }
        } else
          throw new WrongNonterminalException(this.currentToken);
      } else if (this.match(CLOSED_PAREN)) {
        this.nextToken();
        return;
      }
    } while (true);
  }

  public OldParser(Reader r) throws FileNotFoundException, IOException {
    this(new Tokenizer(r));
  }

  public OldParser(Tokenizer t) throws IOException {
    this.tokenizer = t;
    this.nextToken();
  }

  public OldParser(String fileName) throws FileNotFoundException, IOException {
    this(new Tokenizer(fileName));
  }

  public static void main(String[] args) throws Exception {
    OldParser p = new OldParser("files/syntax-test.ipr");
    try {
      while (true) {
        System.out.println(p.readFormula());
      }
    } catch (EOFException eofe) {
      System.out.println("done");
    } catch (NestedBindingException nbe) {
      nbe.printStackTrace();
    }
  }

}// OldParser

