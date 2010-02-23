package bitnots.parse;

import java.io.*;
import java.util.*;

import bitnots.expressions.*;
import bitnots.tableaux.*;
import bitnots.theories.*;

/**
 * The Parser class creates an ANTLR generated parser and lexer that
 * matches input files.  After reading the file it splits up the
 * definitions it sees, turns them into KBSequents to be put into the
 * knowledge base, stored in a Theory object.
 *
 * The parser recognizes eight kinds of elements:
 * <blockquote>
 * <code>(format-only true)</code> -- Defines identifiers of axioms,
 * term, theorems, but doesn't add their definitions to the knowledge
 * base.<br />
 * <code>(format-only false)</code> -- Adds everything to the knowledge
 * base.  This is the setting by default.<br />
 * <code>(include "<i>filename</i>")</code> -- Parses through the file
 * at <i>filename</i> and adds that theory to this theory.
 * <code>(def-axiom <i>axiom_name</i> <i>formula</i> (string "axiom
 * description"))</code> -- An axiom definition that will be placed
 * inside the knowledge base.<br />
 * <code>(def-lemma <i>lemma_name</i> <i>formula</i> (string "lemma
 * description"))</code> -- A lemma definition that will be placed
 * inside the knowledge base.<br />
 * <code>(def-predicate <i>predicate</i> <i>formula</i> (string
 * "predicate description") (format "usage format"))</code> -- A lemma
 * definition that will be placed inside the knowledge base.<br />
 * <code>(def-target <i>target_name</i> <i>formula</i> (string "target
 * description"))</code> -- A target definition that shows the formula
 * that we desire to prove.<br />
 * <code>(def-term <i>term</i> <i>term</i> (string "term description")
 * (format "usage format"))</code> -- A term definition that will be
 * placed inside the knowledge base.<br />
 * <code>(def-theorem <i>theorem_name</i> <i>formula</i> (string
 * "theorem description"))</code> -- A theorem definition that will be
 * placed inside the knowledge base.
 * </blockquote>
 *
 * Note that all <code>(string "")</code> and <code>(format "")</code>
 * elements are optional.
 *
 * @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
 * @version 1.0
 */
public class IPRParser implements AbstractParser{

  /**
   * This differentiates between the three types of valid input files.
   * Each type requires a different ANTLR-generated scanner and parser
   * to read the file.  The three types accepted at this point are
   * our own bitnots files (with a .ipr extension), and TPTP's CNF
   * and FOF files (both with a .p extention).  Set to parse Bitnots
   * input files by default.
   */
  private String fileType = "bitnots";

  /**
   * Holds lists of the definitions matched from the input file.
   */
  private HashMap<String, Collection> defs;

  /**
   * The Theory object where the Parser will add sequents to, when
   * definitions are found and reduced.
   */
  private Theory theory;

  /**
   * Constructs a new Parser object and parses off of standard input.
   */
  public IPRParser() {
    this(new Theory());
  }

  /**
   * Constructs a new Parser object and parses off of standard input.
   * @param oldTheory a Theory object to add sequents to
   */
  public IPRParser(Theory oldTheory) {
    this.theory = oldTheory;
    this.initialize();
    this.parse(new DataInputStream(System.in));
  }

  /**
   * Constructs a new Parser object with a filename and parses from
   * that file.
   * @param filename the file to parse
   */
  public IPRParser(String filename) throws FileNotFoundException {
    this(filename, new Theory());
  }

  /**
   * Constructs a new Parser object with a filename and parses from
   * that file.
   * @param filename the file to parse
   * @param oldTheory a Theory object to add sequents to
   */
  public IPRParser(String filename, Theory oldTheory)
    throws FileNotFoundException {
    this.theory = oldTheory;
    this.initialize();
    this.fileType = this.determineFileType(new File(filename));
    this.parse(new FileInputStream(filename));
  }
  
  /**
   * Initializes the hash of definitions found while parsing through
   * the input.
   */
  private void initialize() {
    // The only things that may need order preserved are targets and
    // file includes.  For those cases, we use arrays, otherwise
    // we use sets for speed and efficiency.
    this.defs = new HashMap<String, Collection>(6);
    this.defs.put(new String("axioms"), new HashSet());
    this.defs.put(new String("lemmas"), new HashSet());
    this.defs.put(new String("predicates"), new HashSet());
    this.defs.put(new String("targets"), new ArrayList(1));
    this.defs.put(new String("terms"), new HashSet());
    this.defs.put(new String("theorems"), new HashSet());
    this.defs.put(new String("fileincludes"), new ArrayList());
  }

  /**
   * Parses the input for definitions that can be made into sequents
   * or saved as targets.
   * @param input the InputStream object that has info for the
   * parser.
   */
  private void parse(InputStream input) {
    if (this.fileType == null) {
      System.out.println("Could not determine file type.");
      return;
    }
    if (this.fileType.equals("bitnots")) {
      try {
        BitnotsScanner scanner = new BitnotsScanner(input);
        BitnotsParser parser = new BitnotsParser(scanner);
        parser.file(this.defs);
      }
      catch (Exception e) {
        System.err.println("Exception: " + e);
        e.printStackTrace();
      }
      this.registerSequents();
    }
    else if (this.fileType.equals("tptp-cnf-problem")
          || this.fileType.equals("tptp-cnf-axiom")
          || this.fileType.equals("tptp-cnf-equality")) {
      try {
        CNFScanner scanner = new CNFScanner(input);
        CNFParser parser = new CNFParser(scanner);
        parser.cnfFile((ArrayList) this.defs.get("targets"),
                       (ArrayList) this.defs.get("fileincludes"),
                       this.theory, false);
      }
      catch (Exception e) {
        System.err.println("Exception: " + e);
        e.printStackTrace();
      }
    }
    else if (this.fileType.equals("tptp-fof-problem")
          || this.fileType.equals("tptp-fof-axiom")
          || this.fileType.equals("tptp-fof-equality")) {
      try {
        FOFScanner scanner = new FOFScanner(input);
        FOFParser parser = new FOFParser(scanner);
        parser.fofFile(this.defs, false);
      }
      catch (Exception e) {
        System.err.println("Exception: " + e);
        e.printStackTrace();
      }
      this.registerSequents();
    }
    else
      System.out.println("Did not recognize file type.");

    this.runFileIncludes();
  }
  
  /**
   * This parser will clear its internal values and start over again
   * by parsing filename.
   * @param filename the filename of the file to parse.
   */
  public void parseReplacingWithFile(String filename)
    throws FileNotFoundException {
    this.theory = new Theory();
    this.initialize();
    this.fileType = this.determineFileType(new File(filename));
    parse(new FileInputStream(filename));
  }
  
  /**
   * This parser will add to its internal values with the information
   * found from parsing filename.
   * @param filename the filename of the file to parse.
   */
  public void parseAppendingWithFile(String filename)
    throws FileNotFoundException {
    this.fileType = this.determineFileType(new File(filename));
    parse(new FileInputStream(filename));
  }

  /**
   * Generates the sequents from the parsed input and stuffs the
   * theory object with those sequents.
   */
  private void registerSequents() {
    this.makeAxiomSequents();
    this.makeLemmaSequents();
    this.makePredicateSequents();
    this.makeTermSequents();
    this.makeTheoremSequents();
    this.moveConjecturesToTheory();
  }

  /**
   * Returns the Theory object that this Parser had used to store
   * KBSequents.  This theory will contain the sequents from the
   * definitions found when parsing input.
   * @return the theory object used when parsing input, already
   * loaded with sequents.
   */
  public Theory getTheory() {
    return this.theory;
  }

  /**
   * Sets the Parser's Theory object
   * @param the new Theory object for the Parser.
   */
  private void setTheory(Theory newTheory) {
    this.theory = newTheory;
  }

  /**
   * Gets the next Axiom from the input parsed.  This also removes
   * that definition from the list of Axioms.  To get them all, call
   * this method until it returns null.
   * @return the next Axiom created from the input parsed.  Will be
   * null if there are no more definitions.
   */
  private Axiom getNextAxiom() {
    HashSet axiomSet = (HashSet) this.defs.get("axioms");
    if (axiomSet.size() == 0)
        return null;
    Object value = axiomSet.iterator().next();
    axiomSet.remove(value);
    return (Axiom) value;
  }

  /**
   * Gets the next Axiom from the input parsed.  This also removes
   * that definition from the list of Axioms.  To get them all, call
   * this method until it returns null.
   * @return the next Axiom created from the input parsed.  Will be
   * null if there are no more definitions.
   */
  private Conjecture getNextConjecture() {
    Collection conjectures = this.defs.get("targets");
    if (conjectures.size() == 0)
        return null;
    Object value = conjectures.iterator().next();
    conjectures.remove(value);
    return (Conjecture) value;
  }

  /**
   * Gets the next Lemma from the input parsed.  This also removes
   * that definition from the list of Lemmas.  To get them all, call
   * this method until it returns null.
   * @return the next Lemma created from the input parsed.  Will be
   * null if there are no more definitions.
   */
  private Lemma getNextLemma() {
    HashSet lemmaSet = (HashSet) this.defs.get("lemmas");
    if (lemmaSet.size() == 0)
      return null;
    Object value = lemmaSet.iterator().next();
    lemmaSet.remove(value);
    return (Lemma) value;
  }

  /**
   * Gets the next Predicate from the input parsed.  This also
   * removes that definition from the list of Predicates.  To get
   * them all, call this method until it returns null.
   * @return the next Predicate created from the input parsed.  Will
   * be null if there are no more definitions.
   */
  private PredicateDefinition getNextPredicate() {
    HashSet predicateSet = (HashSet) this.defs.get("predicates");
    if (predicateSet.size() == 0)
      return null;
    Object value = predicateSet.iterator().next();
    predicateSet.remove(value);
    return (PredicateDefinition) value;
  }

  /**
   * Returns the list of Conjectures seen by this parser.
   * @return the list of Conjectures seen by this parser.
   */
  public List getAllConjectures() {
    return Collections.unmodifiableList(
        (ArrayList) this.defs.get("targets"));
  }
  
  /**
   * Returns the current size of the list of conjectures.
   * @return the current size of the list of conjectures.
   */
  public int numberOfTargets() {
    return ((ArrayList) this.defs.get("targets")).size();
  }

  /**
   * Adds new conjectures to the internal list of conjectures.
   * @param conjectures the list of conjectures to add.
   */
  private void addToConjectures(Collection conjectures) {
    ((ArrayList) this.defs.get("targets")).addAll(conjectures);
  }

  /**
   * Gets the next TermDefinition from the input parsed.  This also
   * removes that definition from the list of TermDefinitions.  To
   * get them all, call this method until it returns null.
   * @return the next TermDefinition created from the input parsed.
   * Will be null if there are no more definitions.
   */
  private TermDefinition getNextTermDef() {
    HashSet termSet = (HashSet) this.defs.get("terms");
    if (termSet.size() == 0)
      return null;
    Object value = termSet.iterator().next();
    termSet.remove(value);
    return (TermDefinition) value;
  }

  /**
   * Gets the next Theorem from the input parsed.  This also removes
   * that definition from the list of Theorems.  To get them all,
   * call this method until it returns null.
   * @return the next Theorem created from the input parsed.  Will be
   * null if there are no more definitions.
   */
  private Theorem getNextTheorem() {
    HashSet theoremSet = (HashSet) this.defs.get("theorems");
    if (theoremSet.size() == 0)
      return null;
    Object value = theoremSet.iterator().next();
    theoremSet.remove(value);
    return (Theorem) value;
  }

  /**
   * Gets the next filename to include from the input parsed.  This
   * also removes that filename from the list of filenames.  To get
   * them all, call this method until it returns null.
   * @return the next filename from the input parsed.  Will be null
   * if there are no more filenames.
   */
  private String getNextFileInclude() {
    ArrayList fileArray = (ArrayList) this.defs.get("fileincludes");
    if (fileArray.size() == 0) {
      return null;
    }
    return (String) fileArray.remove(0);
  }

  /**
   * Takes all of the axiom definitions from the parser and inserts
   * them, as sequents, into the theory.
   */
  private void makeAxiomSequents() {
    Axiom axiom;
    while ( (axiom = this.getNextAxiom()) != null) {
      Tableau tableau = new Tableau(axiom.getAxiom());

      (new ReduceSequentTask(tableau, this.theory,
                             axiom.getDescription())).run();
    }
  }

  /**
   * Moves conjectures from here to the theory.
   */
  private void moveConjecturesToTheory() {
    Conjecture conjecture;
    while ( (conjecture= this.getNextConjecture()) != null) {
      // add the conjecture to the theory
      this.theory.addConjecture(conjecture);
    }
  }

  /**
   * Takes all of the lemma definitions from the parser and inserts
   * them, as sequents, into the theory.
   */
  private void makeLemmaSequents() {
    Lemma lemma;
    while ( (lemma = this.getNextLemma()) != null) {
      Tableau tableau = new Tableau(lemma.getLemma());

      (new ReduceSequentTask(tableau, this.theory,
                             lemma.getDescription())).run();
    }
  }

  /**
   * Takes all of the predicate definitions from the parser and
   * inserts them, as sequents, into the theory.
   */
  private void makePredicateSequents() {
    PredicateDefinition pred;
    while ( (pred = this.getNextPredicate()) != null) {
      Collection list = new ArrayList(2);
      list.add(pred.getDefiniendum());
      list.add(pred.getDefiniens());
      PropositionalConstructor ps = (PropositionalConstructor)
          Symbol.putOrGet(PropositionalConstructor.class, "iff");

      Tableau tableau = new Tableau(FormulaFactory.getFormula(
          ps, new Object[] {list}));

      (new ReduceSequentTask(tableau, this.theory,
                             pred.getName(), pred.getFormat())).run();
    }
  }

  /**
   * Takes all of the term definitions from the parser and inserts
   * them, as sequents, into the theory.
   */
  private void makeTermSequents() {
    TermDefinition termDef;
    while ( (termDef = this.getNextTermDef()) != null) {
      Collection list = new ArrayList(2);
      list.add(termDef.getDefiniendum());
      list.add(termDef.getDefiniens());
      PredicateConstructor ps = (PredicateConstructor)
          Symbol.putOrGet(PredicateConstructor.class, "=");
      Formula f = FormulaFactory.getFormula(ps,
                                            new Object[] {list});

      KBSequent termSequent = new KBSequent();
      termSequent.addNegative(f);
      termSequent.setName(termDef.getName());
      termSequent.setFormat(termDef.getFormat());
      this.theory.addSequent(termSequent);
    }
  }

  /**
   * Takes all of the theorem definitions from the parser and inserts
   * them, as sequents, into the theory.
   */
  private void makeTheoremSequents() {
    Theorem theorem;
    while ( (theorem = this.getNextTheorem()) != null) {
      Tableau tableau = new Tableau(theorem.getTheorem());

      (new ReduceSequentTask(tableau, this.theory,
                             theorem.getDescription())).run();
    }
  }

  /**
   * takes all of the filenames found while parsing this file, and
   * combines any theory elements found there with this Parser's
   * theory and any conjectures found with this Parser's list of
   * conjectures.
   */
  private void runFileIncludes() {
    String filename;
    while ((filename = this.getNextFileInclude()) != null) {
      try {
        IPRParser newParser = new IPRParser(filename, this.theory);
        this.setTheory(newParser.getTheory());
        this.addToConjectures(newParser.getAllConjectures());
      }
      catch (FileNotFoundException fnfe) {
        System.out.println("File not found!");
        continue;
      }
    }
  }

  /**
   * Determines the type of input file give.  This lets us use the
   * correct type of parser for that file.
   * @param file The file whose type we are trying to determine.
   * @return The type of the file.  Returns null if it could not be
   * determined.
   */
  private String determineFileType(File file) {
    String filename = file.getName();
    int i = filename.lastIndexOf('.');
    if (i > 0 && i < filename.length() - 1) {
      String extension = filename.substring(i + 1).toLowerCase();
      if (extension.equals("ipr"))
        return "bitnots";
      else if (extension.equals("p")) {
        if (filename.charAt(6) == '-')
          return "tptp-cnf-problem";
        else if (filename.charAt(6) == '+')
          return "tptp-fof-problem";
      }
      else if (extension.equals("ax")) {
        if (filename.charAt(6) == '-')
          return "tptp-cnf-axiom";
        else if (filename.charAt(6) == '+')
          return "tptp-fof-axiom";
      }
      else if (extension.equals("eq")) {
        if (filename.charAt(6) == '-')
          return "tptp-cnf-equality";
        else if (filename.charAt(6) == '+')
          return "tptp-fof-equality";
      }
    }
      return null;
  }

  /**
   * Creates a parser object, and gives it the appropriate stuff to
   * run.  When that's done, takes what the parser has found and
   * generates sequents to be put into a Theory object.
   * @param args Possibly the filename of a file to parse.
   */
  public static void main(String args[]) {
    IPRParser.prepareThings();
    IPRParser parser = null;
    if (args.length == 0) {
      parser = new IPRParser();
    }
    else {
      try {
        parser = new IPRParser(args[0]);
      }
      catch (FileNotFoundException fnfe) {
        System.out.println(fnfe);
      }
    }

    Conjecture target;
    List conjectures = parser.getAllConjectures();
    for (int i = 0; i < conjectures.size(); ++i) {
      System.out.println("target:\n" + conjectures.get(i));
    }
  }

  /**
   * Gets everything ready for the parser as well as for generating
   * sequents after the parser finishes.
   */
  private static void prepareThings() {
    try {
      TableauFormula.registerConstructor(
          Predicate.class, NegativePredicate.SIGN,
          NegativePredicate.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
          Truth.class, NegativeTruth.SIGN,
          NegativeTruth.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
          Falsity.class, NegativeFalsity.SIGN,
          NegativeFalsity.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Not.class, NegativeNot.SIGN,
                                         NegativeNot.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Or.class, NegativeOr.SIGN,
                                         NegativeOr.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(And.class, NegativeAnd.SIGN,
                                         NegativeAnd.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Implies.class, NegativeImplies.SIGN,
                                         NegativeImplies.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Iff.class, NegativeIff.SIGN,
                                         NegativeIff.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Forall.class, NegativeForall.SIGN,
                                         NegativeForall.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Forsome.class, NegativeForsome.SIGN,
                                         NegativeForsome.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
          Predicate.class, PositivePredicate.SIGN,
          PositivePredicate.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
          Truth.class, PositiveTruth.SIGN,
          PositiveTruth.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(
          Falsity.class, PositiveFalsity.SIGN,
          PositiveFalsity.class.getConstructor(new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Not.class, PositiveNot.SIGN,
                                         PositiveNot.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Or.class, PositiveOr.SIGN,
                                         PositiveOr.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(And.class, PositiveAnd.SIGN,
                                         PositiveAnd.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Implies.class, PositiveImplies.SIGN,
                                         PositiveImplies.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Iff.class, PositiveIff.SIGN,
                                         PositiveIff.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Forall.class, PositiveForall.SIGN,
                                         PositiveForall.class.getConstructor(
          new Class[] {Formula.class}));
      TableauFormula.registerConstructor(Forsome.class, PositiveForsome.SIGN,
                                         PositiveForsome.class.getConstructor(
          new Class[] {Formula.class}));
    }
    catch (NoSuchMethodException e) {
      e.printStackTrace();
    }
  }
}
