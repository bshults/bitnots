// $ANTLR 2.7.5 (20050128): "cnfparse.g" -> "CNFParser.java"$

package bitnots.parse;

import bitnots.expressions.*;
import bitnots.theories.*;

import java.util.*;

public interface CNFParserTokenTypes {
	int EOF = 1;
	int NULL_TREE_LOOKAHEAD = 3;
	int LITERAL_input_clause = 4;
	int OPEN_P = 5;
	int LOWER_WORD = 6;
	int COMMA = 7;
	int CLOSE_P = 8;
	int PERIOD = 9;
	int DASH = 10;
	int LITERAL_axiom = 11;
	int LITERAL_definition = 12;
	int LITERAL_knowledge = 13;
	int LITERAL_assumption = 14;
	int LITERAL_hypothesis = 15;
	int LITERAL_conjecture = 16;
	int LITERAL_lemma = 17;
	int LITERAL_theorem = 18;
	int LITERAL_plain = 19;
	int LITERAL_unknown = 20;
	int LITERAL_derived = 21;
	int OPEN_B = 22;
	int CLOSE_B = 23;
	int SIGN = 24;
	int LITERAL_true = 25;
	int LITERAL_false = 26;
	int LITERAL_equal = 27;
	int NUMBER = 28;
	int QUOTED_STRING = 29;
	int UPPER_WORD = 30;
	int LITERAL_include = 31;
	int UNDERSCORE = 32;
	int COMMENT = 33;
	int WS = 34;
	int NEWLINE = 35;
}
