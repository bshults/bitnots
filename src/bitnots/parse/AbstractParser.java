package bitnots.parse;

import bitnots.theories.Theory;
import java.io.FileNotFoundException;
import java.util.List;

/**
 * The AbstractParser interface specifies what a parser should look
 * like.  The parser should include a way to get a Theory object as
 * well as getting a list of Conjectures.
 */
public interface AbstractParser {
  /**
   * This parser will clear its internal values and start over again
   * by parsing filename.
   * @param filename the filename of the file to parse.
   */
  public void parseReplacingWithFile(String filename) throws FileNotFoundException;

  /**
   * This parser will add to its internal values with the information
   * found from parsing filename.
   * @param filename the filename of the file to parse.
   */
  public void parseAppendingWithFile(String filename) throws FileNotFoundException;

  /**
   * Returns the Theory object that this Parser had used to store
   * KBSequents.  This theory will contain the sequents from the
   * definitions found when parsing input.
   * @return the theory object used when parsing input, already
   * loaded with sequents.
   */
  public Theory getTheory();

  /**
   * Returns the list of Conjectures seen by this parser.
   * @return the list of Conjectures seen by this parser.
   */
  public List getAllConjectures();
}

