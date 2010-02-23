// $ANTLR 2.7.5 (20050128): "bitnotsparse.g" -> "BitnotsScanner.java"$

package bitnots.parse;

import bitnots.expressions.*;
import bitnots.theories.*;

import java.util.*;

public interface BitnotsParserTokenTypes {
	int EOF = 1;
	int NULL_TREE_LOOKAHEAD = 3;
	int OPEN_P = 4;
	// "def-axiom" = 5
	int CLOSE_P = 6;
	// "def-lemma" = 7
	// "def-predicate" = 8
	// "def-conjecture" = 9
	// "def-term" = 10
	// "def-knowledge" = 11
	int LITERAL_and = 12;
	int LITERAL_or = 13;
	int LITERAL_not = 14;
	int LITERAL_implies = 15;
	int LITERAL_iff = 16;
	int LITERAL_forall = 17;
	// "for-some" = 18
	int LITERAL_truth = 19;
	int LITERAL_falsity = 20;
	int SYMBOL = 21;
	// "the-class-of-all" = 22
	int LITERAL_the = 23;
	int LITERAL_name = 24;
	int STRING_LITERAL = 25;
	int LITERAL_format = 26;
	// "format-only" = 27
	int LITERAL_true = 28;
	int LITERAL_false = 29;
	int LITERAL_include = 30;
	int LITERAL_string = 31;
	int LITERAL_identifier = 32;
	int COMMENT = 33;
	int WS = 34;
	int NEWLINE = 35;
}
