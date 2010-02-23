// $ANTLR 2.7.5 (20050128): "bitnotsparse.g" -> "BitnotsParser.java"$

package bitnots.parse;

import bitnots.expressions.*;
import bitnots.theories.*;

import java.util.*;

import antlr.TokenBuffer;
import antlr.TokenStreamException;
import antlr.TokenStreamIOException;
import antlr.ANTLRException;
import antlr.LLkParser;
import antlr.Token;
import antlr.TokenStream;
import antlr.RecognitionException;
import antlr.NoViableAltException;
import antlr.MismatchedTokenException;
import antlr.SemanticException;
import antlr.ParserSharedInputState;
import antlr.collections.impl.BitSet;

/**
* A parser generated by the <a href="http://www.antlr.org/">ANTLR</a>
* tools.  When paired with a BitnotsScanner, matches files that
* contain definitions used to aid in proving any matched targets.
* Each matched element returns an object of the type that corresponds
* to it (ie. matching a formulaType will return a Formula object).
* @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
* @author ANTLR
* @version 1.0
*/
public class BitnotsParser extends antlr.LLkParser       implements BitnotsParserTokenTypes
 {

/**
* True if definitions found are only there to put their names into
* Symbol's table.  If not, the definitions are added into a list
* inside a definitions HashMap.
*/
private boolean formatOnly = false;

/**
* True if we are currently reading a def-conjecture element.  This is
* important because when we read a free variable inside a def-conjecture,
* we turn it into a constant.  All other times, we turn it into a
* sequent variable.
*/
private boolean inTarget = false;

protected BitnotsParser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
}

public BitnotsParser(TokenBuffer tokenBuf) {
  this(tokenBuf,3);
}

protected BitnotsParser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
}

public BitnotsParser(TokenStream lexer) {
  this(lexer,3);
}

public BitnotsParser(ParserSharedInputState state) {
  super(state,3);
  tokenNames = _tokenNames;
}

/**
* Matches a file, which consists of elements.  Those elements are to
* be put into a definitions HashMap.
* @param defs the HashMap to place definitions into.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
*/
	public final void file(
		HashMap defs
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		
		
		{
		_loop3:
		do {
			if ((LA(1)==OPEN_P)) {
				element(defs);
			}
			else {
				break _loop3;
			}
			
		} while (true);
		}
		match(Token.EOF_TYPE);
	}
	
/**
* Matches an element, which can be a (format-only ...) switch, an
* (include "...") file include, or a (def-??? ...) definition.  If
* formatOnly is false, the definition will be added to the
* corresponding list inside defs.
* @param defs The HashMap to place definitions into.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
*/
	public final void element(
		HashMap defs
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		
		
		TheoryElement t = null;     // The TheoryElement that the matched
		//      defintion corresponds to.
		String fileInclude = null;  // The filename in an include element.
		
		
		if ((LA(1)==OPEN_P) && (LA(2)==27)) {
			formatOnlyType();
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_include)) {
			fileInclude=fileIncludeType();
			if ( inputState.guessing==0 ) {
				
				((ArrayList) defs.get("fileincludes")).add(fileInclude);
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==5)) {
			t=defAxiomType();
			if ( inputState.guessing==0 ) {
				
				if (!this.formatOnly)
				((HashSet) defs.get("axioms")).add(t);
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==7)) {
			t=defLemmaType();
			if ( inputState.guessing==0 ) {
				
				if (!this.formatOnly)
				((HashSet) defs.get("lemmas")).add(t);
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==8)) {
			t=defPredicateType();
			if ( inputState.guessing==0 ) {
				
				if (!this.formatOnly)
				((HashSet) defs.get("predicates")).add(t);
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==9)) {
			t=defTargetType();
			if ( inputState.guessing==0 ) {
				
				if (!this.formatOnly)
				((ArrayList) defs.get("targets")).add(t);
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==10)) {
			t=defTermType();
			if ( inputState.guessing==0 ) {
				
				if (!this.formatOnly)
				((HashSet) defs.get("terms")).add(t);
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==11)) {
			t=defTheoremType();
			if ( inputState.guessing==0 ) {
				
				if (!this.formatOnly)
				((HashSet) defs.get("theorems")).add(t);
				
			}
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
	}
	
/**
* Matches a format only element, which can be either <code>(
* format-only true)</code> or <code>(format-only false)</code>.
*/
	public final void formatOnlyType() throws RecognitionException, TokenStreamException {
		
		
		if ((LA(1)==OPEN_P) && (LA(2)==27) && (LA(3)==LITERAL_true)) {
			match(OPEN_P);
			match(27);
			match(LITERAL_true);
			match(CLOSE_P);
			if ( inputState.guessing==0 ) {
				this.formatOnly = true;
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==27) && (LA(3)==LITERAL_false)) {
			match(OPEN_P);
			match(27);
			match(LITERAL_false);
			match(CLOSE_P);
			if ( inputState.guessing==0 ) {
				this.formatOnly = false;
			}
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
	}
	
/**
* Matches a file include element, which is in the form of <code>(
* include "<i>filename</i>")</code>.
* @return the filename that is between the quotes.
*/
	public final String  fileIncludeType() throws RecognitionException, TokenStreamException {
		String s = null;
		
		Token  sl = null;
		
		match(OPEN_P);
		match(LITERAL_include);
		sl = LT(1);
		match(STRING_LITERAL);
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			s = sl.getText();
		}
		return s;
	}
	
/**
* Matches an axiom definition in the form of: <code>(def-axiom
* <i>axiom_name</i> <i>fomula</i> (string "axiom description"))
* </code>.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return a TheoryElement object that corresponds to this axiom.
*/
	public final TheoryElement  defAxiomType() throws RecognitionException, TokenStreamException, NestedBindingException {
		TheoryElement te = null;
		
		
		ArrayList bvs = new ArrayList();    // The bound variables that will be
		//  tracked.
		HashMap others = new HashMap();    // The free variables that will be
		//  tracked.
		Formula f = null;                   // The Axiom formula.
		String id = null;                   // The Axiom's name.
		String desc = null;                 // The Axiom's description.
		
		
		match(OPEN_P);
		match(5);
		id=identifier();
		f=formulaType(bvs, others);
		{
		switch ( LA(1)) {
		case OPEN_P:
		{
			desc=stringType();
			break;
		}
		case CLOSE_P:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			te = new Axiom(f, id, desc);
			
		}
		return te;
	}
	
/**
* Matches a lemma definition in the form of: <code>(def-lemma
* <i>lemma_name</i> <i>formula</i> (string "lemma description"))
* </code>.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return a TheoryElement object that corresponds to this lemma.
*/
	public final TheoryElement  defLemmaType() throws RecognitionException, TokenStreamException, NestedBindingException {
		TheoryElement te = null;
		
		
		ArrayList bvs = new ArrayList();    // The bound variables that will be
		//  tracked.
		HashMap others = new HashMap();    // The free variables that will be
		//  tracked.
		Formula f = null;                   // The Lemma's formula.
		String id = null;                   // The Lemma's name.
		String desc = null;                 // The Lemma's description.
		
		
		match(OPEN_P);
		match(7);
		id=identifier();
		f=formulaType(bvs, others);
		{
		switch ( LA(1)) {
		case OPEN_P:
		{
			desc=stringType();
			break;
		}
		case CLOSE_P:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			te = new Lemma(f, id, desc);
			
		}
		return te;
	}
	
/**
* Matches a predicate definition in the form of: <code>(def-predicate
* <i>predicate</i> <i>formula</i> (string "predicate description")
* (format "usage format"))</code>.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return a TheoryElement object that corresponds to this predicate.
*/
	public final TheoryElement  defPredicateType() throws RecognitionException, TokenStreamException, NestedBindingException {
		TheoryElement te = null;
		
		
		ArrayList bvs = new ArrayList();    // The bound variables that will be
		//  tracked.
		HashMap others = new HashMap();    // The free variables that will be
		//  tracked.
		Formula pred = null;                // The Predicate's predicate.
		Formula form = null;                // The Predicate's formula.
		String desc = null;                 // The Predicate's description.
		String format = null;
		
		
		match(OPEN_P);
		match(8);
		pred=predicateType(bvs, others);
		form=formulaType(bvs, others);
		{
		if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_format)) {
			format=formatType();
		}
		else if ((LA(1)==OPEN_P||LA(1)==CLOSE_P) && (LA(2)==EOF||LA(2)==OPEN_P||LA(2)==LITERAL_name)) {
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		}
		{
		switch ( LA(1)) {
		case OPEN_P:
		{
			desc=stringType();
			break;
		}
		case CLOSE_P:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			te = new PredicateDefinition((Predicate) pred, form, format, desc);
			
		}
		return te;
	}
	
/**
* Matches a target definition in the form of: <code>(def-conjecture
* <i>target_name</i> <i>formula</i> (string "target description"))
* </code>.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return a TheoryElement object that corresponds to this target.
*/
	public final TheoryElement  defTargetType() throws RecognitionException, TokenStreamException, NestedBindingException {
		TheoryElement te = null;
		
		
		ArrayList bvs = new ArrayList();    // The bound variables that will be
		//  tracked.
		HashMap others = new HashMap();     // The constants variables that will
		//  be tracked.
		Formula f = null;                   // The Conjecture's formula.
		String id = null;                   // The Conjecture's name.
		String desc = null;                 // The Conjecture's description.
		
		
		match(OPEN_P);
		match(9);
		if ( inputState.guessing==0 ) {
			this.inTarget = true;
		}
		id=identifier();
		f=formulaType(bvs, others);
		{
		switch ( LA(1)) {
		case OPEN_P:
		{
			desc=stringType();
			break;
		}
		case CLOSE_P:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			te = new Conjecture(f, id, desc);
			this.inTarget = false;
			
		}
		return te;
	}
	
/**
* Matches a term definition in the form of: <code>(def-term
* <i>term</i> </i>term</i> (string "term description") (format "usage
* format"))</code>.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return a TheoryElement object that corresponds to this term.
*/
	public final TheoryElement  defTermType() throws RecognitionException, TokenStreamException, NestedBindingException {
		TheoryElement te = null;
		
		
		ArrayList bvs = new ArrayList();    // The bound variables that will be
		//  tracked.
		HashMap others = new HashMap();    // The free variables that will be
		//  tracked.
		ComplexTerm term1 = null;                  // The Term's definiendum.
		ComplexTerm term2 = null;                  // The Term's definiens.
		String desc = null;                 // The Term's description.
		String format = null;               // The Term's format string.
		
		
		match(OPEN_P);
		match(10);
		term1=termType(bvs, others);
		term2=termType(bvs, others);
		{
		if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_format)) {
			format=formatType();
		}
		else if ((LA(1)==OPEN_P||LA(1)==CLOSE_P) && (LA(2)==EOF||LA(2)==OPEN_P||LA(2)==LITERAL_name)) {
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		}
		{
		switch ( LA(1)) {
		case OPEN_P:
		{
			desc=stringType();
			break;
		}
		case CLOSE_P:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			te = new TermDefinition(term1, term2, format, desc);
			
		}
		return te;
	}
	
/**
* Matches a theorem definition in the form of: <code>(def-knowledge
* <i>theorem_name</i> <i>formula</i> (string "theorem description"))
* </code>.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return a TheoryElement object that corresponds to this theory.
*/
	public final TheoryElement  defTheoremType() throws RecognitionException, TokenStreamException, NestedBindingException {
		TheoryElement te = null;
		
		
		ArrayList bvs = new ArrayList();    // The bound variables that will be
		//  tracked.
		HashMap others = new HashMap();    // The free variables that will be
		//  tracked.
		Formula f = null;                   // The Theorem's formula.
		String id = null;                   // The Theorem's name.
		String desc = null;                 // The Theorem's description.
		
		
		match(OPEN_P);
		match(11);
		id=identifier();
		f=formulaType(bvs, others);
		{
		switch ( LA(1)) {
		case OPEN_P:
		{
			desc=stringType();
			break;
		}
		case CLOSE_P:
		{
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			te = new Theorem(f, id, desc);
			
		}
		return te;
	}
	
/**
* Matches an identifier, which can be any reserved words as well as
* any valid identifier.  Used for allowing theorem names and other
* identifiers to be the same as the keywords.
* @todo why are these new Strings instead of literals?
* @return the String of what was matched.
*/
	public final String  identifier() throws RecognitionException, TokenStreamException {
		String id = null;
		
		Token  s = null;
		
		{
		switch ( LA(1)) {
		case SYMBOL:
		{
			s = LT(1);
			match(SYMBOL);
			if ( inputState.guessing==0 ) {
				id = s.getText();
			}
			break;
		}
		case LITERAL_and:
		{
			match(LITERAL_and);
			if ( inputState.guessing==0 ) {
				id = "and";
			}
			break;
		}
		case 5:
		{
			match(5);
			if ( inputState.guessing==0 ) {
				id = "def-axiom";
			}
			break;
		}
		case 7:
		{
			match(7);
			if ( inputState.guessing==0 ) {
				id = "def-lemma";
			}
			break;
		}
		case 8:
		{
			match(8);
			if ( inputState.guessing==0 ) {
				id = "def-predicate";
			}
			break;
		}
		case 9:
		{
			match(9);
			if ( inputState.guessing==0 ) {
				id = "def-conjecture";
			}
			break;
		}
		case 10:
		{
			match(10);
			if ( inputState.guessing==0 ) {
				id = "def-term";
			}
			break;
		}
		case 11:
		{
			match(11);
			if ( inputState.guessing==0 ) {
				id = "def-knowledge";
			}
			break;
		}
		case LITERAL_false:
		{
			match(LITERAL_false);
			if ( inputState.guessing==0 ) {
				id = "false";
			}
			break;
		}
		case LITERAL_falsity:
		{
			match(LITERAL_falsity);
			if ( inputState.guessing==0 ) {
				id = "falsity";
			}
			break;
		}
		case LITERAL_format:
		{
			match(LITERAL_format);
			if ( inputState.guessing==0 ) {
				id = "format";
			}
			break;
		}
		case 27:
		{
			match(27);
			if ( inputState.guessing==0 ) {
				id = "format-only";
			}
			break;
		}
		case LITERAL_iff:
		{
			match(LITERAL_iff);
			if ( inputState.guessing==0 ) {
				id = "iff";
			}
			break;
		}
		case LITERAL_implies:
		{
			match(LITERAL_implies);
			if ( inputState.guessing==0 ) {
				id = "implies";
			}
			break;
		}
		case LITERAL_include:
		{
			match(LITERAL_include);
			if ( inputState.guessing==0 ) {
				id = "include";
			}
			break;
		}
		case LITERAL_not:
		{
			match(LITERAL_not);
			if ( inputState.guessing==0 ) {
				id = "not";
			}
			break;
		}
		case LITERAL_or:
		{
			match(LITERAL_or);
			if ( inputState.guessing==0 ) {
				id = "or";
			}
			break;
		}
		case LITERAL_string:
		{
			match(LITERAL_string);
			if ( inputState.guessing==0 ) {
				id = "string";
			}
			break;
		}
		case LITERAL_identifier:
		{
			match(LITERAL_identifier);
			if ( inputState.guessing==0 ) {
				id = "identifier";
			}
			break;
		}
		case LITERAL_name:
		{
			match(LITERAL_name);
			if ( inputState.guessing==0 ) {
				id = "name";
			}
			break;
		}
		case LITERAL_true:
		{
			match(LITERAL_true);
			if ( inputState.guessing==0 ) {
				id = "true";
			}
			break;
		}
		case LITERAL_truth:
		{
			match(LITERAL_truth);
			if ( inputState.guessing==0 ) {
				id = "truth";
			}
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		return id;
	}
	
/**
* Matches a formula, which could be either <code>(truth)</code> or
* <code>(falsity)</code>, a propositional formula, a first-order logic
* formula, or a predicate formula.
* @param bvs the list of bound varibles so far.
* @param others the list of free varibles or constants so far.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return A Formula object corresponding to this formula.
*/
	public final Formula  formulaType(
		ArrayList bvs, HashMap others
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		Formula f = null;
		
		
		if ((LA(1)==OPEN_P) && (LA(2)==CLOSE_P)) {
			match(OPEN_P);
			match(CLOSE_P);
		}
		else {
			boolean synPredMatched22 = false;
			if (((LA(1)==OPEN_P) && ((LA(2) >= LITERAL_and && LA(2) <= LITERAL_iff)))) {
				int _m22 = mark();
				synPredMatched22 = true;
				inputState.guessing++;
				try {
					{
					match(OPEN_P);
					{
					switch ( LA(1)) {
					case LITERAL_and:
					{
						match(LITERAL_and);
						break;
					}
					case LITERAL_or:
					{
						match(LITERAL_or);
						break;
					}
					case LITERAL_not:
					{
						match(LITERAL_not);
						break;
					}
					case LITERAL_implies:
					{
						match(LITERAL_implies);
						break;
					}
					case LITERAL_iff:
					{
						match(LITERAL_iff);
						break;
					}
					default:
					{
						throw new NoViableAltException(LT(1), getFilename());
					}
					}
					}
					}
				}
				catch (RecognitionException pe) {
					synPredMatched22 = false;
				}
				rewind(_m22);
				inputState.guessing--;
			}
			if ( synPredMatched22 ) {
				f=propType(bvs, others);
			}
			else {
				boolean synPredMatched25 = false;
				if (((LA(1)==OPEN_P) && (LA(2)==LITERAL_forall||LA(2)==18))) {
					int _m25 = mark();
					synPredMatched25 = true;
					inputState.guessing++;
					try {
						{
						match(OPEN_P);
						{
						switch ( LA(1)) {
						case LITERAL_forall:
						{
							match(LITERAL_forall);
							break;
						}
						case 18:
						{
							match(18);
							break;
						}
						default:
						{
							throw new NoViableAltException(LT(1), getFilename());
						}
						}
						}
						}
					}
					catch (RecognitionException pe) {
						synPredMatched25 = false;
					}
					rewind(_m25);
					inputState.guessing--;
				}
				if ( synPredMatched25 ) {
					f=folType(bvs, others);
				}
				else if ((LA(1)==OPEN_P) && (LA(2)==SYMBOL)) {
					f=predicateType(bvs, others);
				}
				else if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_truth)) {
					match(OPEN_P);
					match(LITERAL_truth);
					match(CLOSE_P);
					if ( inputState.guessing==0 ) {
						
						LogicalConstructor ps = (LogicalConstructor) Symbol.putOrGet(
						LogicalConstructor.class, "truth");
						f = FormulaFactory.getFormula(ps, null);
						
					}
				}
				else if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_falsity)) {
					match(OPEN_P);
					match(LITERAL_falsity);
					match(CLOSE_P);
					if ( inputState.guessing==0 ) {
						
						LogicalConstructor ps = (LogicalConstructor) Symbol.putOrGet(
						LogicalConstructor.class, "falsity");
						f = FormulaFactory.getFormula(ps, null);
						
					}
				}
				else {
					throw new NoViableAltException(LT(1), getFilename());
				}
				}}
				return f;
			}
			
/**
* Matches a string desctription in the form: <code>(string "
* <i>text</i>")</code>.
* @return a String of what's inside the quotes.
*/
	public final String  stringType() throws RecognitionException, TokenStreamException {
		String s = null;
		
		Token  sl = null;
		
		match(OPEN_P);
		match(LITERAL_name);
		sl = LT(1);
		match(STRING_LITERAL);
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			s = sl.getText();
		}
		return s;
	}
	
/**
* Matches a predicate formula in the form of: <code>(<i>predicate_name
* terms</i>)</code>, where terms is one or more terms or variables.
* @param bvs the list of bound varibles so far.
* @param others the list of free varibles or constants so far.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return A Formula object corresponding to this predicate formula.
*/
	public final Formula  predicateType(
		ArrayList bvs, HashMap others
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		Formula f = null;
		
		Token  s = null;
		
		List list = new ArrayList();    // The arguments for this predicate
		//   formula.
		Term t = null;                  // Any term type matched as an argument.
		String id = null;               // Any variable name matched as an
		//   argument.
		
		
		match(OPEN_P);
		s = LT(1);
		match(SYMBOL);
		{
		_loop39:
		do {
			switch ( LA(1)) {
			case OPEN_P:
			{
				{
				t=termType(bvs, others);
				if ( inputState.guessing==0 ) {
					list.add(t);
				}
				}
				break;
			}
			case 5:
			case 7:
			case 8:
			case 9:
			case 10:
			case 11:
			case LITERAL_and:
			case LITERAL_or:
			case LITERAL_not:
			case LITERAL_implies:
			case LITERAL_iff:
			case LITERAL_truth:
			case LITERAL_falsity:
			case SYMBOL:
			case LITERAL_name:
			case LITERAL_format:
			case 27:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_include:
			case LITERAL_string:
			case LITERAL_identifier:
			{
				id=identifier();
				if ( inputState.guessing==0 ) {
					
					Variable bMatch = null;
					
					// Is the identifier in the list of bound variables?
					for (int i = 0; i < bvs.size(); ++i)
					{
					if (((Variable) bvs.get(i)).getExternalName().equals(id))
					{
					bMatch = (Variable) bvs.get(i);
					break;
					}
					}
					if (bMatch != null)
					list.add(bMatch);
					else
					{
					if (this.inTarget)
					{
					// Is the identifier in the map of constants?
					if (others.containsKey(id))
					list.add(others.get(id));
					else
					{
					Function newFunc = Function.createNewConstant(id);
					list.add(newFunc);
					others.put(id, newFunc);
					}
					}
					else
					{
					// Is the identifier in the list of free variables?
					Variable freeMatch = null;
					if (others.containsKey(id))
					list.add(others.get(id));
					else
					{
					Variable newVar = Variable.createNewSequentVar(id);
					list.add(newVar);
					others.put(id, newVar);
					}
					}
					}        
					
				}
				break;
			}
			default:
			{
				break _loop39;
			}
			}
		} while (true);
		}
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			PredicateConstructor ps;
			ps = (PredicateConstructor) Symbol.putOrGet(
			PredicateConstructor.class, s.getText());
			f = FormulaFactory.getFormula(ps, new Object[] {list});
			
		}
		return f;
	}
	
/**
* Matches a format desctription in the form: <code>(format "
* <i>text</i>")</code>.
* @return a String of what's inside the quotes.
*/
	public final String  formatType() throws RecognitionException, TokenStreamException {
		String s = null;
		
		Token  sl = null;
		
		match(OPEN_P);
		match(LITERAL_format);
		sl = LT(1);
		match(STRING_LITERAL);
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			s = sl.getText();
		}
		return s;
	}
	
/**
* Matches a term, which is either a class type, an iota type, or a
* function type.
* @param bvs the list of bound varibles so far.
* @param others the list of free varibles or constants so far.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return A Term object corresponding to this term.
*/
	public final ComplexTerm  termType(
		ArrayList bvs, HashMap others
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		ComplexTerm t = null;
		
		
		if ((LA(1)==OPEN_P) && (LA(2)==CLOSE_P)) {
			match(OPEN_P);
			match(CLOSE_P);
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==22)) {
			t=classType(bvs, others);
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_the)) {
			t=iotaType(bvs, others);
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==SYMBOL)) {
			t=functionType(bvs, others);
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		return t;
	}
	
/**
* Matches a propositional formula in the form of: <code>(<i>type
* formula more formulas?</i>)</code>, where type and the number of
* formulas for that type are "and":2+, "or":2+, "not":1, "implies":2,
* "iff":2.
* @param bvs the list of bound varibles so far.
* @param others the list of free varibles or constants so far.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return A Formula object corresponding to this propositional
* formula.
*/
	public final Formula  propType(
		ArrayList bvs, HashMap others
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		Formula f = null;
		
		
		List list = new ArrayList();    // The arguments in this propositional
		//   formula.
		Formula form = null;            // A formula object that will be matched
		//   in this propositional formula.
		
		
		if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_and)) {
			match(OPEN_P);
			match(LITERAL_and);
			form=formulaType(bvs, others);
			if ( inputState.guessing==0 ) {
				list.add(form);
			}
			{
			int _cnt28=0;
			_loop28:
			do {
				if ((LA(1)==OPEN_P)) {
					form=formulaType(bvs, others);
					if ( inputState.guessing==0 ) {
						list.add(form);
					}
				}
				else {
					if ( _cnt28>=1 ) { break _loop28; } else {throw new NoViableAltException(LT(1), getFilename());}
				}
				
				_cnt28++;
			} while (true);
			}
			match(CLOSE_P);
			if ( inputState.guessing==0 ) {
				
				PropositionalConstructor ps;
				ps = (PropositionalConstructor) Symbol.putOrGet(
				PropositionalConstructor.class, "and");
				f = FormulaFactory.getFormula(ps, new Object[] {list});
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_or)) {
			match(OPEN_P);
			match(LITERAL_or);
			form=formulaType(bvs, others);
			if ( inputState.guessing==0 ) {
				list.add(form);
			}
			{
			int _cnt30=0;
			_loop30:
			do {
				if ((LA(1)==OPEN_P)) {
					form=formulaType(bvs, others);
					if ( inputState.guessing==0 ) {
						list.add(form);
					}
				}
				else {
					if ( _cnt30>=1 ) { break _loop30; } else {throw new NoViableAltException(LT(1), getFilename());}
				}
				
				_cnt30++;
			} while (true);
			}
			match(CLOSE_P);
			if ( inputState.guessing==0 ) {
				
				PropositionalConstructor ps;
				ps = (PropositionalConstructor) Symbol.putOrGet(
				PropositionalConstructor.class, "or");
				f = FormulaFactory.getFormula(ps, new Object[] {list});
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_not)) {
			match(OPEN_P);
			match(LITERAL_not);
			form=formulaType(bvs, others);
			if ( inputState.guessing==0 ) {
				list.add(form);
			}
			match(CLOSE_P);
			if ( inputState.guessing==0 ) {
				
				PropositionalConstructor ps;
				ps = (PropositionalConstructor) Symbol.putOrGet(
				PropositionalConstructor.class, "not");
				f = FormulaFactory.getFormula(ps, new Object[] {list});
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_implies)) {
			match(OPEN_P);
			match(LITERAL_implies);
			form=formulaType(bvs, others);
			if ( inputState.guessing==0 ) {
				list.add(form);
			}
			form=formulaType(bvs, others);
			if ( inputState.guessing==0 ) {
				list.add(form);
			}
			match(CLOSE_P);
			if ( inputState.guessing==0 ) {
				
				PropositionalConstructor ps;
				ps = (PropositionalConstructor) Symbol.putOrGet(
				PropositionalConstructor.class, "implies");
				f = FormulaFactory.getFormula(ps, new Object[] {list});
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_iff)) {
			match(OPEN_P);
			match(LITERAL_iff);
			form=formulaType(bvs, others);
			if ( inputState.guessing==0 ) {
				list.add(form);
			}
			form=formulaType(bvs, others);
			if ( inputState.guessing==0 ) {
				list.add(form);
			}
			match(CLOSE_P);
			if ( inputState.guessing==0 ) {
				
				PropositionalConstructor ps;
				ps = (PropositionalConstructor) Symbol.putOrGet(
				PropositionalConstructor.class, "iff");
				f = FormulaFactory.getFormula(ps, new Object[] {list});
				
			}
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		return f;
	}
	
/**
* Matches a first-order logic formula in the form of: <code>(<i>type
* </i>(<i>variables</i>) <i>formula</i>)</code>, where type is either
* "forall" or "for-some", and variables is one or more variables
* inside parens.
* @param bvs the list of bound varibles so far.
* @param others the list of free varibles or constants so far.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return A Formula object corresponding to this first-order logic
* formula.
*/
	public final Formula  folType(
		ArrayList bvs, HashMap others
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		Formula f = null;
		
		
		ArrayList localBVS = new ArrayList();   // The list of variables made
		// bound by this FOL formula.
		ArrayList childBVS = (ArrayList) bvs.clone();   // The list of bound
		// variables that the internal formula will
		// be subject to.
		Formula form = null;        // The formula for this FOL formula.
		String id = null;           // Used for matching identifiers in the
		// bound variables list.
		
		
		if ((LA(1)==OPEN_P) && (LA(2)==LITERAL_forall)) {
			match(OPEN_P);
			match(LITERAL_forall);
			match(OPEN_P);
			{
			int _cnt33=0;
			_loop33:
			do {
				if ((LA(1)==OPEN_P)) {
					match(OPEN_P);
					id=identifier();
					if ( inputState.guessing==0 ) {
						
						for (int i = 0; i < bvs.size(); ++i)
						{
						if (((Variable) bvs.get(i)).getExternalName().equals(id))
						throw new NestedBindingException();
						}
						Variable var = Variable.createNewBoundVar(id);
						localBVS.add(var);
						childBVS.add(var);
						
					}
					match(CLOSE_P);
				}
				else {
					if ( _cnt33>=1 ) { break _loop33; } else {throw new NoViableAltException(LT(1), getFilename());}
				}
				
				_cnt33++;
			} while (true);
			}
			match(CLOSE_P);
			form=formulaType(childBVS, others);
			match(CLOSE_P);
			if ( inputState.guessing==0 ) {
				
				FOLConstructor folc = (FOLConstructor) Symbol.putOrGet(
				FOLConstructor.class, "forall");
				f = FormulaFactory.getFormula(folc, new Object[] {localBVS, form});
				
			}
		}
		else if ((LA(1)==OPEN_P) && (LA(2)==18)) {
			match(OPEN_P);
			match(18);
			match(OPEN_P);
			{
			int _cnt35=0;
			_loop35:
			do {
				if ((LA(1)==OPEN_P)) {
					match(OPEN_P);
					id=identifier();
					if ( inputState.guessing==0 ) {
						
						for (int i = 0; i < bvs.size(); ++i)
						{
						if (((Variable) bvs.get(i)).getExternalName().equals(id))
						throw new NestedBindingException();
						}
						Variable var = Variable.createNewBoundVar(id);
						localBVS.add(var);
						childBVS.add(var);
						
					}
					match(CLOSE_P);
				}
				else {
					if ( _cnt35>=1 ) { break _loop35; } else {throw new NoViableAltException(LT(1), getFilename());}
				}
				
				_cnt35++;
			} while (true);
			}
			match(CLOSE_P);
			form=formulaType(childBVS, others);
			match(CLOSE_P);
			if ( inputState.guessing==0 ) {
				
				FOLConstructor folc = (FOLConstructor) Symbol.putOrGet(
				FOLConstructor.class, "for-some");
				f = FormulaFactory.getFormula(folc, new Object[] {localBVS, form});
				
			}
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		return f;
	}
	
/**
* Matches a class type, in the form of: <code>(the-class-of-all (
* <i>variable</i>) <i>formula</i>)</code>.
* @param bvs the list of bound varibles so far.
* @param others the list of free varibles or constants so far.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return A Term object corresponding to this class term.
*/
	public final ComplexTerm  classType(
		ArrayList bvs, HashMap others
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		ComplexTerm t = null;
		
		
		ArrayList childBVS = (ArrayList) bvs.clone();   // The list of bound
		//  variables that the internal
		//  formula will be subject to.
		Variable var = null;        // The bound variable for this class term.
		String id = null;           // The variable name matched as an argument.
		Formula f = null;           // The formula for this class term.
		
		
		match(OPEN_P);
		match(22);
		match(OPEN_P);
		id=identifier();
		if ( inputState.guessing==0 ) {
			
			for (int i = 0; i < bvs.size(); ++i)
			{
			if (((Variable) bvs.get(i)).getExternalName().equals(id))
			throw new NestedBindingException();
			}
			var = Variable.createNewBoundVar(id);
			childBVS.add(var);
			
		}
		match(CLOSE_P);
		f=formulaType(childBVS, others);
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			TermConstructor tc = (TermConstructor) Symbol.putOrGet(
			TermConstructor.class, "the-class-of-all");
			t = TermFactory.getTerm(tc, new Object[] {var, f});
			
		}
		return t;
	}
	
/**
* Matches a class type, in the form of: <code>(the (
* <i>variable</i>) <i>formula</i>)</code>.
* @param bvs the list of bound varibles so far.
* @param others the list of free varibles or constants so far.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return A Term object corresponding to this iota term.
*/
	public final ComplexTerm  iotaType(
		ArrayList bvs, HashMap others
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		ComplexTerm t = null;
		
		
		ArrayList childBVS = (ArrayList) bvs.clone();   // The list of bound
		//  variables that the internal
		//  formula will be subject to.
		Variable var = null;        // The bound variable for this class term.
		String id = null;           // The variable name matched as an argument.
		Formula f = null;           // The formula for this class term.
		
		
		match(OPEN_P);
		match(LITERAL_the);
		match(OPEN_P);
		id=identifier();
		if ( inputState.guessing==0 ) {
			
			for (int i = 0; i < bvs.size(); ++i)
			{
			if (((Variable) bvs.get(i)).getExternalName().equals(id))
			throw new NestedBindingException();
			}
			var = Variable.createNewBoundVar(id);
			childBVS.add(var);
			
		}
		match(CLOSE_P);
		f=formulaType(childBVS, others);
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			TermConstructor tc = (TermConstructor) Symbol.putOrGet(
			TermConstructor.class, "the");
			t = TermFactory.getTerm(tc, new Object[] {var, f});
			
		}
		return t;
	}
	
/**
* Matches a class type, in the form of: <code>(<i>function_name
* terms</i>)</code>, where terms is one or more terms or variables.
* @param bvs the list of bound varibles so far.
* @param others the list of free varibles or constants so far.
* @throws NestedBindingException if a bound variable is used later as
* the bound variable in an FOLFormula, a class term, or an iota term.
* @return A Term object corresponding to this function.
*/
	public final ComplexTerm  functionType(
		ArrayList bvs, HashMap others
	) throws RecognitionException, TokenStreamException, NestedBindingException {
		ComplexTerm t = null;
		
		Token  s = null;
		
		List list = new ArrayList();
		String id = null;
		
		
		match(OPEN_P);
		s = LT(1);
		match(SYMBOL);
		{
		_loop46:
		do {
			switch ( LA(1)) {
			case OPEN_P:
			{
				{
				t=termType(bvs, others);
				if ( inputState.guessing==0 ) {
					list.add(t);
				}
				}
				break;
			}
			case 5:
			case 7:
			case 8:
			case 9:
			case 10:
			case 11:
			case LITERAL_and:
			case LITERAL_or:
			case LITERAL_not:
			case LITERAL_implies:
			case LITERAL_iff:
			case LITERAL_truth:
			case LITERAL_falsity:
			case SYMBOL:
			case LITERAL_name:
			case LITERAL_format:
			case 27:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_include:
			case LITERAL_string:
			case LITERAL_identifier:
			{
				id=identifier();
				if ( inputState.guessing==0 ) {
					
					Variable bMatch = null;
					
					// Is the identifier in the list of bound variables?
					for (int i = 0; i < bvs.size(); ++i)
					{
					Variable temp = (Variable) bvs.get(i);
					if (((Variable) bvs.get(i)).getExternalName().equals(id))
					{
					bMatch = (Variable) bvs.get(i);
					break;
					}
					}
					if (bMatch != null)
					list.add(bMatch);
					else
					{
					if (this.inTarget)
					{
					// Is the identifier in the map of constants?
					if (others.containsKey(id))
					list.add(others.get(id));
					else
					{
					Function newFunc = Function.createNewConstant(id);
					list.add(newFunc);
					others.put(id, newFunc);
					}
					}
					else
					{
					// Is the identifier in the list of free variables?
					Variable freeMatch = null;
					if (others.containsKey(id))
					list.add(others.get(id));
					else
					{
					Variable newVar = Variable.createNewSequentVar(id);
					list.add(newVar);
					others.put(id, newVar);
					}
					}
					}
					
				}
				break;
			}
			default:
			{
				break _loop46;
			}
			}
		} while (true);
		}
		match(CLOSE_P);
		if ( inputState.guessing==0 ) {
			
			FunctionConstructor fs;
			fs = (FunctionConstructor) Symbol.putOrGet(
			FunctionConstructor.class, s.getText());
			t = Function.createFunction(fs, list);
			
		}
		return t;
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"an opening paren",
		"\"def-axiom\"",
		"a closing paren",
		"\"def-lemma\"",
		"\"def-predicate\"",
		"\"def-conjecture\"",
		"\"def-term\"",
		"\"def-knowledge\"",
		"\"and\"",
		"\"or\"",
		"\"not\"",
		"\"implies\"",
		"\"iff\"",
		"\"forall\"",
		"\"for-some\"",
		"\"truth\"",
		"\"falsity\"",
		"SYMBOL",
		"\"the-class-of-all\"",
		"\"the\"",
		"\"name\"",
		"a quoted string",
		"\"format\"",
		"\"format-only\"",
		"\"true\"",
		"\"false\"",
		"\"include\"",
		"\"string\"",
		"\"identifier\"",
		"COMMENT",
		"WS",
		"NEWLINE"
	};
	
	
	}
