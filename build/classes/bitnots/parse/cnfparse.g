header {
package bitnots.parse;

import bitnots.expressions.*;
import bitnots.theories.*;

import java.util.*;
}

/**
* A parser generated by the <a href="http://www.antlr.org/">ANTLR</a>
* tools.  When paired with a CNFScanner, matches files that contain
* definitions used to aid in proving any matched targets.
* @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
* @author ANTLR
* @version 1.0
*/
class CNFParser extends Parser;
options
{
    k = 3;
    defaultErrorHandler=false;
}

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
}

/**
* Matches a file, which consists of elements.  Those elements are to
* be put into either the list of targets, the list of includes or the
* theory object.
* @param targets The list of targets found while parsing.
* @param includes The list of file includes found while parsing.
* @param theory The theory where sequents are placed.
*/
cnfFile [ArrayList targets, ArrayList includes,
         Theory theory, boolean enableBitnotsEquality]
    {
        this.useBitnotsEquality = enableBitnotsEquality;
    }
    : (cnfInput[targets, includes, theory])+ EOF
    ;

/**
* Matches an element, which can be a file include, or an input_clause
* definition.
* @param targets The list of targets found while parsing.
* @param includes The list of file includes found while parsing.
* @param theory The theory where sequents are placed.
*/
cnfInput [ArrayList targets, ArrayList includes, Theory theory]
    {
        String filename = null;
    }
    : inputClause[targets, theory]
    | filename=include
        { includes.add(filename); }
    ;

/**
* Matches an input_clause definition, which defines what conjectures
* we have to prove, and what axioms, hypotheses and other elements we
* have to help us with the proof.
* @param targets The list of targets found while parsing.
* @param theory The theory where sequents are placed.
*/
inputClause [ArrayList targets, Theory theory]
    {
        String inputType = null;
        HashSet set = null;
        HashMap defs = new HashMap();
    }
    : "input_clause" OPEN_P s:LOWER_WORD COMMA inputType=type
            { if (inputType.equals("conjecture")) this.inTarget = true; }
        COMMA set=literals[defs] CLOSE_P PERIOD
        {

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
    ;

/**
* Matches a type, which tells us the type of input_clause we matched.
* @return a String representation of the type.
*/
type
    returns [String s = null]
    {
        String opt = null;
    }
    : s=userType (opt=optSourceType)?
        {
            if (opt != null)
                s = s.concat(opt);
        }
    | s=sourceType
    ;

/**
* Matches an optional source type.
* @return a String representation of the optional source type.
*/
optSourceType
    returns [String type = null]
    : DASH type=sourceType
        {
            type = "-".concat(type);
        }
    ;

/**
* Matches a user type.
* @return the String representation of the user type.
*/
userType
    returns [String type = null]
    : "axiom"               { type = new String("axiom"); }
    | "definition"          { type = new String("definition"); }
    | "knowledge"           { type = new String("knowledge"); }
    | "assumption"          { type = new String("assumption"); }
    | "hypothesis"          { type = new String("hypothesis"); }
    | "conjecture"          { type = new String("conjecture"); }
    | "lemma"               { type = new String("lemma"); }
    | "theorem"             { type = new String("theorem"); }
    | "plain"               { type = new String("plain"); }
    | "unknown"             { type = new String("unknown"); }
    ;

/**
* Matches a source type.
* @return the String representation of the source type.
*/
sourceType
    returns [String type = null]
    : "derived"             { type = new String("derived"); }
    ;

/**
* Matches a comma separated list of literals.  The list is enclosed in
* brackets.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the set of literals matched.
*/
literals [HashMap defs]
    returns [HashSet set = new HashSet()]
    {
        CNFLiteral lit = null;
    }
    : OPEN_B CLOSE_B
    | OPEN_B lit=literal[defs]
            { set.add(lit); }
        (COMMA lit=literal[defs]
            { set.add(lit); }
        )* CLOSE_B
    ;

/**
* Matches a literal, which is a sign followed by an atomic formula.
* @param defs the collection of variable and constant definitions seen
* before.
* @return a CNFLiteral object, which stores the sign and formula of
* the literal matched.
*/
literal [HashMap defs]
    returns [CNFLiteral l = null]
    {
        Formula form = null;
    }
    : s:SIGN form=atom[defs]
        { l = new CNFLiteral(form, s.getText().equals("++")); }
    ;

/**
* Matches an atomic formula.  This can be "true" and "false", or a
* predicate, including the equals predicate.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the formula matched.
*/
atom [HashMap defs]
    returns [Formula form = null]
    {
        Term t = null;
        ArrayList list = new ArrayList(2);
    }
    : "true"
        {
            LogicalConstructor ps = (LogicalConstructor) Symbol.putOrGet(
                                            LogicalConstructor.class, "truth");
            form = FormulaFactory.getFormula(ps, null);
        }
    | "false"
        {
            LogicalConstructor ps = (LogicalConstructor) Symbol.putOrGet(
                                            LogicalConstructor.class, "truth");
            form = FormulaFactory.getFormula(ps, null);
        }
    | "equal" OPEN_P t=term[defs]
            {
                list.add(t);
            }
        COMMA t=term[defs]
            {
                list.add(t);
            }
        CLOSE_P
        {
            String name = "equals";
            if (this.useBitnotsEquality)
                name = "=";
            form = FormulaFactory.getFormula(
                            (PredicateConstructor) Symbol.putOrGet(
                            PredicateConstructor.class,
                            name), new Object[] {list});
        }
    | form=proposition[defs]
    | form=predicate[defs]
    ;

/**
* Matches a proposition, which is simply a lower-case word.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the formula representing this proposition.
*/
proposition [HashMap defs]
    returns [Formula form = null]
    : a:LOWER_WORD
        {
            form = FormulaFactory.getFormula(
                            (PredicateConstructor) Symbol.putOrGet(
                            PredicateConstructor.class, a.getText()),
                            new Object[] {Collections.EMPTY_LIST});
        }
    ;

/**
* Matches a predicate.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the formula representing this predicate.
*/
predicate [HashMap defs]
    returns [Formula form = null]
    {
        ArrayList list = null;
    }
    : s:LOWER_WORD OPEN_P list=arguments[defs] CLOSE_P
        {
            String name = s.getText();
            form = FormulaFactory.getFormula(
                            (PredicateConstructor) Symbol.putOrGet(
                            PredicateConstructor.class,
                            name), new Object[] {list});
        }
    ;

/**
* Matches a list of arguments, which is a comma-separated list of
* terms.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the list of terms.
*/
arguments [HashMap defs]
    returns [ArrayList list = new ArrayList(1)]
    {
        Term t = null;
    }
    : t=term[defs]
            { list.add(t); }
        (COMMA t=term[defs]
            { list.add(t); }
        )*
    ;

/**
* Matches a term, which is either a function or a variable.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the term matched.
*/
term [HashMap defs]
    returns [Term t = null]
    : t=function[defs]
    | t=variable[defs]
    ;

/**
* Matches a function, which can be a simple constant.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the term representing this function.
*/
function [HashMap defs]
    returns [Term t = null]
    {
        ArrayList list = null;
    }
    : t=constant[defs]
    | s:LOWER_WORD OPEN_P list=arguments[defs] CLOSE_P
        {
            t = Function.createFunction(
                    (FunctionConstructor) Symbol.putOrGet(
                    FunctionConstructor.class, s.getText()), list);
        }
    ;

/**
* Matches a constant, which can be a lower-case word, a number or a
* quoted string.
* @param defs the collection of variable and constant definitions seen
* before.
* @return the function representing this constant.
*/
constant [HashMap defs]
    returns [Term t = null]
    {
        String name = null;
    }
    : (a:LOWER_WORD
            { name = a.getText(); }
        | b:NUMBER
            { name = b.getText(); }
        | c:QUOTED_STRING
            { name = c.getText(); }
        )
        {
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
        }
    ;

/**
* Matches a variable, which is simple an upper-case word.
* @param defs the collection of variable and constant definitions seen
* before.
* @return a variable with the matched name.  If we've seen that
* variable before, use that object instead of making a new one.
*/
variable [HashMap defs]
    returns [Term t = null]
    : a:UPPER_WORD
        {
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
        }
    ;

/**
* Matches a file include element.
* @return the filename that is between the quotes.
*/
include
    returns [String filename = null]
    : "include" OPEN_P f:QUOTED_STRING (formulaSelection)? CLOSE_P PERIOD
        { filename = f.getText(); }
    ;

/**
* Matches a formula selection in the file include.  Currently this is
* not being used.
*/
formulaSelection
    : LOWER_WORD
    | OPEN_B LOWER_WORD (COMMA LOWER_WORD)+ CLOSE_B
    ;


/**
* A lexer generated by the <a href="http://www.antlr.org/">ANTLR</a>
* tools.  Matches a stream of characters, packaging them into tokens
* to be shipped off to a CNFParser.
* @author ANTLR
* @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
* @version 1.0
*/
class CNFScanner extends Lexer;

options
{
    k = 2;
    charVocabulary='\3'..'\377';
    filter = WS;
}

/**
* Matches a quoted string, which is a single or double quote, followed
* by several characters, followed by another single or double quote.
* Note that this does not allow for the '\' to be used to escape a
* single or double quote.
*/
QUOTED_STRING
    options
    {
        paraphrase = "a quoted string";
    }
    : '"'! (~('"'))* '"'!
    | '\''! (~('\''))* '\''!
    ;

/**
* Matches an opening parenthesis.
*/
OPEN_P
    options
    {
        paraphrase = "an opening paren";
    }
    : '('
    ;

/**
* Matches a closing parenthesis.
*/
CLOSE_P
    options
    {
        paraphrase = "a closing paren";
    }
    : ')'
    ;

/**
* Matches a opening bracket.
*/
OPEN_B
    options
    {
        paraphrase = "an opening bracket";
    }
    : '['
    ;

/**
* Matches a closing bracket.
*/
CLOSE_B
    options
    {
        paraphrase = "a closing bracket";
    }
    : ']'
    ;

/**
* Matches a upper-case word.
*/
UPPER_WORD
    : ('A'..'Z') (('a'..'z')|('A'..'Z')|('0'..'9')|UNDERSCORE)*
    ;

/**
* Matches an lower-case word.
*/
LOWER_WORD
    : ('a'..'z') (('a'..'z')|('A'..'Z')|('0'..'9')|UNDERSCORE)*
    ;

/**
* Matches a number.
*/
NUMBER
    : ('0'..'9')+ (PERIOD ('0'..'9')+)?
    ;

/**
* Matches a comma.
*/
COMMA
    : ','
    ;

/**
* Matches a period.
*/
PERIOD
    : '.'
    ;

/**
* Matches a dash.
*/
DASH
    : '-'
    ;

/**
* Matches a CNF sign, which is either ++ or --.
*/
SIGN
    : "++"
    | "--"
    ;

/**
* Matches an underscore.
*/
UNDERSCORE
    : '_'
    ;

/**
* Matches a comment, and promptly ignores it.  They do not get passed
* to the parser
*/
COMMENT
    : ( '%' (~('\n'|'\r'))* ('\n'|'\r'('\n')?)
      | '#' (~('\n'|'\r'))* ('\n'|'\r'('\n')?)
      )
        {
            $setType(Token.SKIP);
            newline();
        }
    ;

/**
* Matches whitespace, which is also ignored and not passed to the
* parser.
*/
protected
WS
    : (' '|'\t'|'\f')
        {
            $setType(Token.SKIP);
        }
    ;

/**
* Matches newline characters.  They do not get passed to the parser.
*/
NEWLINE
    : ('\r' | '\n' | '\r' '\n')
        {
            $setType(Token.SKIP);
            newline();
        }
    ;
