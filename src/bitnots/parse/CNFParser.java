// $ANTLR 2.7.5 (20050128): "cnfparse.g" -> "CNFParser.java"$

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
* tools.  When paired with a CNFScanner, matches files that contain
* definitions used to aid in proving any matched targets.
* @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
* @author ANTLR
* @version 1.0
*/
public class CNFParser extends antlr.LLkParser       implements CNFParserTokenTypes
 {

/**
* True if we are currently reading a conjecture element.  This is
* important because when we read a free variable inside a def-target,
* we turn it into a constant.  All other times, we turn it into a
* sequent variable.
*/
private boolean inTarget = false;

/**
* This determines whether the built-in version of equals will be used.
* If this is true, any predicates matched with "equal" as its name
* will be changed into "=", which is what bitnots uses.
*/
private boolean useBitnotsEquality;

protected CNFParser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
}

public CNFParser(TokenBuffer tokenBuf) {
  this(tokenBuf,3);
}

protected CNFParser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
}

public CNFParser(TokenStream lexer) {
  this(lexer,3);
}

public CNFParser(ParserSharedInputState state) {
  super(state,3);
  tokenNames = _tokenNames;
}

/**
* Matches a file, which consists of elements.  Those elements are to
* be put into either the list of targets, the list of includes or the
* theory object.
* @param targets The list of targets found while parsing.
* @param includes The list of file includes found while parsing.
* @param theory The theory where sequents are placed.
*/
	public final void cnfFile(
		ArrayList targets, ArrayList includes,
         Theory theory, boolean enableBitnotsEquality
	) throws RecognitionException, TokenStreamException {
		
		
		this.useBitnotsEquality = enableBitnotsEquality;
		
		
		{
		int _cnt3=0;
		_loop3:
		do {
			if ((LA(1)==LITERAL_input_clause||LA(1)==LITERAL_include)) {
				cnfInput(targets, includes, theory);
			}
			else {
				if ( _cnt3>=1 ) { break _loop3; } else {throw new NoViableAltException(LT(1), getFilename());}
			}
			
			_cnt3++;
		} while (true);
		}
		match(Token.EOF_TYPE);
	}
	
/**
* Matches an element, which can be a file include, or an input_clause
* definition.
* @param targets The list of targets found while parsing.
* @param includes The list of file includes found while parsing.
* @param theory The theory where sequents are placed.
*/
	public final void cnfInput(
		ArrayList targets, ArrayList includes, Theory theory
	) throws RecognitionException, TokenStreamException {
		
		
		String filename = null;
		
		
		switch ( LA(1)) {
		case LITERAL_input_clause:
		{
			inputClause(targets, theory);
			break;
		}
		case LITERAL_include:
		{
			filename=include();
			includes.add(filename);
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
	}
	
/**
* Matches an input_clause definition, which defines what conjectures
* we have to prove, and what axioms, hypotheses and other elements we
* have to help us with the proof.
* @param targets The list of targets found while parsing.
* @param theory The theory where sequents are placed.
*/
	public final void inputClause(
		ArrayList targets, Theory theory
	) throws RecognitionException, TokenStreamException {
		
		Token  s = null;
		
		String inputType = null;
		HashSet set = null;
		HashMap defs = new HashMap();
		
		
		match(LITERAL_input_clause);
		match(OPEN_P);
		s = LT(1);
		match(LOWER_WORD);
		match(COMMA);
		inputType=type();
		if (inputType.equals("conjecture")) this.inTarget = true;
		match(COMMA);
		set=literals(defs);
		match(CLOSE_P);
		match(PERIOD);
		
		
		if (inputType.equals("conjecture"))
		{
		this.inTarget= false;
		targets.add(
		new Conjecture(((CNFLiteral) set.iterator().next()).getFormula(),
		s.getText(), null));
		}
		else if (inputType.equals("axiom") || inputType.equals("hypothesis"))
		{
		KBSequent sequent = new KBSequent();
		sequent.setName(s.getText());
		Iterator iter = set.iterator();
		while (iter.hasNext())
		{
		CNFLiteral lit = (CNFLiteral) iter.next();
		if (lit.getSign())
		sequent.addPositive(lit.getFormula());
		else
		sequent.addNegative(lit.getFormula());
		theory.addToFormulaMap(lit.getFormula(), sequent);
		if (lit.getFormula() instanceof Predicate)
		{
		Predicate p = (Predicate) lit.getFormula();
		
		if (p.getConstructor().toString().equals("="))
		{
		Iterator terms = p.arguments();
		Term t1 = (Term) terms.next();
		Term t2 = (Term) terms.next();
		
		if (t1 instanceof ComplexTerm)
		{
		ComplexTerm ct1 = (ComplexTerm) t1;
		if (ct1.getConstructor().getName().equals("the-class-of-all"))
		theory.addClassMemberEQ(p);
		}
		if (t2 instanceof ComplexTerm)
		{
		ComplexTerm ct2 = (ComplexTerm) t2;
		if (ct2.getConstructor().getName().equals("the-class-of-all"))
		theory.addClassMemberEQ(p);
		}
		}
		}
		}
		
		theory.addSequent(sequent);
		}
		else
		System.out.println(inputType + " is a type I haven't dealt with yet.");
		
	}
	
/**
* Matches a file include element.
* @return the filename that is between the quotes.
*/
	public final String  include() throws RecognitionException, TokenStreamException {
		String filename = null;
		
		Token  f = null;
		
		match(LITERAL_include);
		match(OPEN_P);
		f = LT(1);
		match(QUOTED_STRING);
		{
		switch ( LA(1)) {
		case LOWER_WORD:
		case OPEN_B:
		{
			formulaSelection();
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
		match(PERIOD);
		filename = f.getText();
		return filename;
	}
	
/**
* Matches a type, which tells us the type of input_clause we matched.
* @return a String representation of the type.
*/
	public final String  type() throws RecognitionException, TokenStreamException {
		String s = null;
		
		
		String opt = null;
		
		
		switch ( LA(1)) {
		case LITERAL_axiom:
		case LITERAL_definition:
		case LITERAL_knowledge:
		case LITERAL_assumption:
		case LITERAL_hypothesis:
		case LITERAL_conjecture:
		case LITERAL_lemma:
		case LITERAL_theorem:
		case LITERAL_plain:
		case LITERAL_unknown:
		{
			s=userType();
			{
			switch ( LA(1)) {
			case DASH:
			{
				opt=optSourceType();
				break;
			}
			case COMMA:
			{
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
			}
			
			if (opt != null)
			s = s.concat(opt);
			
			break;
		}
		case LITERAL_derived:
		{
			s=sourceType();
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return s;
	}
	
/**
* Matches a comma separated list of literals.  The list is enclosed in
* brackets.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the set of literals matched.
*/
	public final HashSet  literals(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		HashSet set = new HashSet();
		
		
		CNFLiteral lit = null;
		
		
		if ((LA(1)==OPEN_B) && (LA(2)==CLOSE_B)) {
			match(OPEN_B);
			match(CLOSE_B);
		}
		else if ((LA(1)==OPEN_B) && (LA(2)==SIGN)) {
			match(OPEN_B);
			lit=literal(defs);
			set.add(lit);
			{
			_loop13:
			do {
				if ((LA(1)==COMMA)) {
					match(COMMA);
					lit=literal(defs);
					set.add(lit);
				}
				else {
					break _loop13;
				}
				
			} while (true);
			}
			match(CLOSE_B);
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		return set;
	}
	
/**
* Matches a user type.
* @return the String representation of the user type.
*/
	public final String  userType() throws RecognitionException, TokenStreamException {
		String type = null;
		
		
		switch ( LA(1)) {
		case LITERAL_axiom:
		{
			match(LITERAL_axiom);
			type = new String("axiom");
			break;
		}
		case LITERAL_definition:
		{
			match(LITERAL_definition);
			type = new String("definition");
			break;
		}
		case LITERAL_knowledge:
		{
			match(LITERAL_knowledge);
			type = new String("knowledge");
			break;
		}
		case LITERAL_assumption:
		{
			match(LITERAL_assumption);
			type = new String("assumption");
			break;
		}
		case LITERAL_hypothesis:
		{
			match(LITERAL_hypothesis);
			type = new String("hypothesis");
			break;
		}
		case LITERAL_conjecture:
		{
			match(LITERAL_conjecture);
			type = new String("conjecture");
			break;
		}
		case LITERAL_lemma:
		{
			match(LITERAL_lemma);
			type = new String("lemma");
			break;
		}
		case LITERAL_theorem:
		{
			match(LITERAL_theorem);
			type = new String("theorem");
			break;
		}
		case LITERAL_plain:
		{
			match(LITERAL_plain);
			type = new String("plain");
			break;
		}
		case LITERAL_unknown:
		{
			match(LITERAL_unknown);
			type = new String("unknown");
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return type;
	}
	
/**
* Matches an optional source type.
* @return a String representation of the optional source type.
*/
	public final String  optSourceType() throws RecognitionException, TokenStreamException {
		String type = null;
		
		
		match(DASH);
		type=sourceType();
		
		type = "-".concat(type);
		
		return type;
	}
	
/**
* Matches a source type.
* @return the String representation of the source type.
*/
	public final String  sourceType() throws RecognitionException, TokenStreamException {
		String type = null;
		
		
		match(LITERAL_derived);
		type = new String("derived");
		return type;
	}
	
/**
* Matches a literal, which is a sign followed by an atomic formula.
* @param defs the collection of variable and constant definitions seen
* before.
* @return a CNFLiteral object, which stores the sign and formula of
* the literal matched.
*/
	public final CNFLiteral  literal(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		CNFLiteral l = null;
		
		Token  s = null;
		
		Formula form = null;
		
		
		s = LT(1);
		match(SIGN);
		form=atom(defs);
		l = new CNFLiteral(form, s.getText().equals("++"));
		return l;
	}
	
/**
* Matches an atomic formula.  This can be "true" and "false", or a
* predicate, including the equals predicate.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the formula matched.
*/
	public final Formula  atom(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		Formula form = null;
		
		
		Term t = null;
		ArrayList list = new ArrayList(2);
		
		
		switch ( LA(1)) {
		case LITERAL_true:
		{
			match(LITERAL_true);
			
			LogicalConstructor ps = (LogicalConstructor) Symbol.putOrGet(
			LogicalConstructor.class, "truth");
			form = FormulaFactory.getFormula(ps, null);
			
			break;
		}
		case LITERAL_false:
		{
			match(LITERAL_false);
			
			LogicalConstructor ps = (LogicalConstructor) Symbol.putOrGet(
			LogicalConstructor.class, "truth");
			form = FormulaFactory.getFormula(ps, null);
			
			break;
		}
		case LITERAL_equal:
		{
			match(LITERAL_equal);
			match(OPEN_P);
			t=term(defs);
			
			list.add(t);
			
			match(COMMA);
			t=term(defs);
			
			list.add(t);
			
			match(CLOSE_P);
			
			String name = "equals";
			if (this.useBitnotsEquality)
			name = "=";
			form = FormulaFactory.getFormula(
			(PredicateConstructor) Symbol.putOrGet(
			PredicateConstructor.class,
			name), new Object[] {list});
			
			break;
		}
		default:
			if ((LA(1)==LOWER_WORD) && (LA(2)==COMMA||LA(2)==CLOSE_B)) {
				form=proposition(defs);
			}
			else if ((LA(1)==LOWER_WORD) && (LA(2)==OPEN_P)) {
				form=predicate(defs);
			}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return form;
	}
	
/**
* Matches a term, which is either a function or a variable.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the term matched.
*/
	public final Term  term(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		Term t = null;
		
		
		switch ( LA(1)) {
		case LOWER_WORD:
		case NUMBER:
		case QUOTED_STRING:
		{
			t=function(defs);
			break;
		}
		case UPPER_WORD:
		{
			t=variable(defs);
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return t;
	}
	
/**
* Matches a proposition, which is simply a lower-case word.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the formula representing this proposition.
*/
	public final Formula  proposition(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		Formula form = null;
		
		Token  a = null;
		
		a = LT(1);
		match(LOWER_WORD);
		
		form = FormulaFactory.getFormula(
		(PredicateConstructor) Symbol.putOrGet(
		PredicateConstructor.class, a.getText()),
		new Object[] {Collections.EMPTY_LIST});
		
		return form;
	}
	
/**
* Matches a predicate.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the formula representing this predicate.
*/
	public final Formula  predicate(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		Formula form = null;
		
		Token  s = null;
		
		ArrayList list = null;
		
		
		s = LT(1);
		match(LOWER_WORD);
		match(OPEN_P);
		list=arguments(defs);
		match(CLOSE_P);
		
		String name = s.getText();
		form = FormulaFactory.getFormula(
		(PredicateConstructor) Symbol.putOrGet(
		PredicateConstructor.class,
		name), new Object[] {list});
		
		return form;
	}
	
/**
* Matches a list of arguments, which is a comma-separated list of
* terms.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the list of terms.
*/
	public final ArrayList  arguments(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		ArrayList list = new ArrayList(1);
		
		
		Term t = null;
		
		
		t=term(defs);
		list.add(t);
		{
		_loop20:
		do {
			if ((LA(1)==COMMA)) {
				match(COMMA);
				t=term(defs);
				list.add(t);
			}
			else {
				break _loop20;
			}
			
		} while (true);
		}
		return list;
	}
	
/**
* Matches a function, which can be a simple constant.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the term representing this function.
*/
	public final Term  function(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		Term t = null;
		
		Token  s = null;
		
		ArrayList list = null;
		
		
		if ((LA(1)==LOWER_WORD||LA(1)==NUMBER||LA(1)==QUOTED_STRING) && (LA(2)==COMMA||LA(2)==CLOSE_P)) {
			t=constant(defs);
		}
		else if ((LA(1)==LOWER_WORD) && (LA(2)==OPEN_P)) {
			s = LT(1);
			match(LOWER_WORD);
			match(OPEN_P);
			list=arguments(defs);
			match(CLOSE_P);
			
			t = Function.createFunction(
			(FunctionConstructor) Symbol.putOrGet(
			FunctionConstructor.class, s.getText()), list);
			
		}
		else {
			throw new NoViableAltException(LT(1), getFilename());
		}
		
		return t;
	}
	
/**
* Matches a variable, which is simple an upper-case word.
* @param defs the collection of variable and constant definitions seen
* before.
* @return a variable with the matched name.  If we've seen that
* variable before, use that object instead of making a new one.
*/
	public final Term  variable(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		Term t = null;
		
		Token  a = null;
		
		a = LT(1);
		match(UPPER_WORD);
		
		String name = a.getText();
		if (this.inTarget)
		{
		// Is the identifier in the map of constants?
		if (defs.containsKey(name))
		t = (Term) defs.get(name);
		else
		{
		t = Function.createNewConstant(name);
		defs.put(name, t);
		}
		}
		else
		{
		// Is the identifier in the list of free variables?
		Variable freeMatch = null;
		if (defs.containsKey(name))
		t = (Term) defs.get(name);
		else
		{
		t = Variable.createNewSequentVar(name);
		defs.put(name, t);
		}
		}
		
		return t;
	}
	
/**
* Matches a constant, which can be a lower-case word, a number or a
* quoted string.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the function representing this constant.
*/
	public final Term  constant(
		HashMap defs
	) throws RecognitionException, TokenStreamException {
		Term t = null;
		
		Token  a = null;
		Token  b = null;
		Token  c = null;
		
		String name = null;
		
		
		{
		switch ( LA(1)) {
		case LOWER_WORD:
		{
			a = LT(1);
			match(LOWER_WORD);
			name = a.getText();
			break;
		}
		case NUMBER:
		{
			b = LT(1);
			match(NUMBER);
			name = b.getText();
			break;
		}
		case QUOTED_STRING:
		{
			c = LT(1);
			match(QUOTED_STRING);
			name = c.getText();
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		}
		
		if (this.inTarget)
		{
		// Is the identifier in the map of constants?
		if (defs.containsKey(name))
		t = (Term) defs.get(name);
		else
		{
		t = Function.createNewConstant(name);
		defs.put(name, t);
		}
		}
		else
		{
		// Is the identifier in the list of free variables?
		Variable freeMatch = null;
		if (defs.containsKey(name))
		t = (Term) defs.get(name);
		else
		{
		t = Variable.createNewSequentVar(name);
		defs.put(name, t);
		}
		}
		
		return t;
	}
	
/**
* Matches a formula selection in the file include.  Currently this is
* not being used.
*/
	public final void formulaSelection() throws RecognitionException, TokenStreamException {
		
		
		switch ( LA(1)) {
		case LOWER_WORD:
		{
			match(LOWER_WORD);
			break;
		}
		case OPEN_B:
		{
			match(OPEN_B);
			match(LOWER_WORD);
			{
			int _cnt30=0;
			_loop30:
			do {
				if ((LA(1)==COMMA)) {
					match(COMMA);
					match(LOWER_WORD);
				}
				else {
					if ( _cnt30>=1 ) { break _loop30; } else {throw new NoViableAltException(LT(1), getFilename());}
				}
				
				_cnt30++;
			} while (true);
			}
			match(CLOSE_B);
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"\"input_clause\"",
		"an opening paren",
		"LOWER_WORD",
		"COMMA",
		"a closing paren",
		"PERIOD",
		"DASH",
		"\"axiom\"",
		"\"definition\"",
		"\"knowledge\"",
		"\"assumption\"",
		"\"hypothesis\"",
		"\"conjecture\"",
		"\"lemma\"",
		"\"theorem\"",
		"\"plain\"",
		"\"unknown\"",
		"\"derived\"",
		"an opening bracket",
		"a closing bracket",
		"SIGN",
		"\"true\"",
		"\"false\"",
		"\"equal\"",
		"NUMBER",
		"a quoted string",
		"UPPER_WORD",
		"\"include\"",
		"UNDERSCORE",
		"COMMENT",
		"WS",
		"NEWLINE"
	};
	
	
	}