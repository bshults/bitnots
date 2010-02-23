// $ANTLR 2.7.5 (20050128): "fofparse.g" -> "FOFParser.java"$

package bitnots.parse;

import bitnots.expressions.*;
import bitnots.theories.*;

import java.util.Collections;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public interface FOFParserTokenTypes {
	int EOF = 1;
	int NULL_TREE_LOOKAHEAD = 3;
	int LITERAL_input_formula = 4;
	int OPEN_P = 5;
	int LOWER_WORD = 6;
	int COMMA = 7;
	int CLOSE_P = 8;
	int PERIOD = 9;
	int QUANTIFIER = 10;
	int OPEN_B = 11;
	int CLOSE_B = 12;
	int COLON = 13;
	int NUMBER = 14;
	int UPPER_WORD = 15;
	int AND = 16;
	int N_AND = 17;
	int OR = 18;
	int N_OR = 19;
	int IFF = 20;
	int XOR = 21;
	int IMPLIES = 22;
	int R_IMPLIES = 23;
	int TILDE = 24;
	int DASH = 25;
	int LITERAL_axiom = 26;
	int LITERAL_definition = 27;
	int LITERAL_knowledge = 28;
	int LITERAL_assumption = 29;
	int LITERAL_hypothesis = 30;
	int LITERAL_conjecture = 31;
	int LITERAL_lemma = 32;
	int LITERAL_theorem = 33;
	int LITERAL_plain = 34;
	int LITERAL_unknown = 35;
	int LITERAL_derived = 36;
	int LITERAL_true = 37;
	int LITERAL_false = 38;
	int LITERAL_equal = 39;
	int QUOTED_STRING = 40;
	int LITERAL_include = 41;
	int UNDERSCORE = 42;
	int COMMENT = 43;
	int WS = 44;
	int NEWLINE = 45;
}
