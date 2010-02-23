package bitnots.parse;

import java.util.*;
import java.io.*;

/**
 * @author Benjamin Shults
 * @version .2
 */

public class Tokenizer {
  
  /** This is the input stream from which the tokens will be rebbad.  */
  private BufferedReader input;

  /** currentChar keeps track of the first character of the next token to
   * be read.  */
  // This needs to start as a space so that the initial call to
  // clearWhitespace will work correctly.
  private int currentChar = ' ';

  private char nextChar() throws IOException {
    return (char) (this.currentChar = this.input.read());
  }

  public void close() throws IOException {
    this.input.close();
  }

  /** Reads until we get to a nonwhitespace character or EOF. */
  private void clearWhitespace() throws IOException {
    while (this.currentChar != -1 &&
           Character.isWhitespace((char) this.currentChar)) {
      this.nextChar();
    }
  }

  /**
   * Returns true if and only if there is another token to be read
   * from the Reader.
   * @return true if and only if there is another token to be read
   * from the Reader.  */
  public boolean hasNextToken() throws IOException {
    if (this.currentChar == ';') {
      // a comment, ignore the rest of the line.
      this.input.readLine();
      this.nextChar();
      this.clearWhitespace();
      return this.hasNextToken();
    }
    return this.currentChar != -1;
  }

  public static boolean isSymbolPart(char c) {
    return                      // Character.isUnicodeIdentifierPart(c);
      Character.isJavaIdentifierPart(c) ||
      c == '*' ||
      c == '+' ||
      c == '-' ||
      c == '=' ||
      c == '/' ||
      c == '|' ||
      c == '<' ||
      c == '>' ||
      c == '?' ||
      c == ':';
  }

  public static boolean isSymbolStart(char c) {
    return // Character.isUnicodeIdentifierStart(c);
      Character.isJavaIdentifierStart(c) ||
      c == '*' ||
      c == '+' ||
      c == '-' ||
      c == '=' ||
      c == '/' ||
      c == '|' ||
      c == '<' ||
      c == '>' ||
      c == '?' ||
      c == ':' ||
      (c <= '9' && c >= '0');
  }

  /** This deals intelligently with apostrophes and hyphenated tokens.
   * It treats punctuation as separate tokens.
   * @return the next token read from input.  It will always be
   * interned. */
  // This can assume that currentChar is not whitespace.
  public String nextToken() throws IOException {

    if (!this.hasNextToken())
      throw new NoSuchElementException();

    // The token will be stored in this one character at a time.
    StringBuffer returnToken = new StringBuffer();

    // c will always be (char) this.currentChar;
    char c = (char) this.currentChar;

    if (Tokenizer.isSymbolStart(c)) {
      // Now read a symbol token.  At this point, I am sure that I am
      // looking at a letter.

      // The first character of the token is the last character read by
      // the last call to this method.
      returnToken.append(c);
      c = this.nextChar();

      // Read the rest of this token.
      while (this.currentChar != -1 && (Tokenizer.isSymbolPart(c))) {
        returnToken.append(c);
        c = this.nextChar();
      }
    } else if (c == ';') {
      // a comment, ignore the rest of the line.
      this.input.readLine();
      c = this.nextChar();
      this.clearWhitespace();
      if (this.hasNextToken())
        return this.nextToken();
      else
        throw new NoSuchElementException();
    } else if (c == '"') {
      this.readString(returnToken);
      c = this.nextChar();
    } else {
      // special one-char symbol
      returnToken.append(c);
      c = this.nextChar();
    }
    this.clearWhitespace();
    return returnToken.toString().intern();
  }

//   /**
//    * This assumes that the leading DOUBLE_QUOTE has been read.  This
//    * clears whitespace when it is done.  This returns the interned
//    * String.
//    */
//   public String readString() throws WrongNonterminalException, IOException {
//     if (this.currentChar != '"')
//       throw new WrongNonterminalException(
//         "- " + (char) this.currentChar + " -");
//     StringBuffer value = new StringBuffer();
//     this.readString(value);
//     this.nextChar();
//     this.clearWhitespace();
//     return value.toString().intern();
//   }

  /**
   * This assumes that the leading DOUBLE_QUOTE has been read.  This
   * leaves currentChar at the trailing DOUBLE_QUOTE.
   */
  private void readString(StringBuffer sb) throws IOException {
    do {
      this.nextChar();
      if (this.currentChar == -1)
        throw new IOException("File ended while parsing string.");
      else if (this.currentChar == '\\') {
        this.nextChar();
        if (this.currentChar == -1)
          throw new IOException("File ended while parsing string.");
        else if (this.currentChar == '"')
          sb.append('"');
        else {
          sb.append('\\');
          sb.append(this.currentChar);
        }
      } else if (this.currentChar == '"') {
        return;
      } else
        sb.append((char) this.currentChar);
    } while (true);
  }

  /**
   * Construct a Tokenizer that will read from the file named in
   * the argument.
   * @param fileName Is the name of the file that will be read.  */
  public Tokenizer(String fileName) throws FileNotFoundException,
                                           IOException {
    // This constructor insures that nobody else can mess with the
    // reader.
    this.input = new BufferedReader(new FileReader(fileName));
    this.clearWhitespace();
  }

  /**
   * Construct a Tokenizer that will read from the file named in
   * the argument.
   * @param fileName Is the name of the file that will be read.  */
  public Tokenizer(Reader is) throws IOException {
    // This constructor insures that nobody else can mess with the
    // reader.
    if (is instanceof BufferedReader)
      this.input = (BufferedReader) is;
    else
      this.input = new BufferedReader(is);
    this.clearWhitespace();
  }

  public static void main(String[] args) {

    Tokenizer me = null;
    try {

      me = new Tokenizer("files/new-imply.ipr");
    
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      return;
    } catch (IOException e) {
      e.printStackTrace();
      return;
    }
    try {
      while (me.hasNextToken())
        // print it out one token per line so that I can see what it is
        // treating as tokens.
        System.out.println(me.nextToken());
    } catch (IOException e) {
      e.printStackTrace();
      return;
    } finally {
      try {
        me.close();
      } catch (IOException e) {}
    }
  }
}

