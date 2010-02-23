package bitnots.parse;

/**
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version .2
 */

public class WrongNonterminalException extends Exception {

//   private String token;

//   public String getToken() {
//     return this.token;
//   }

  public WrongNonterminalException(String token) {
    super(token);
//    this.token = token;
  }
  
}// WrongNonterminalException

