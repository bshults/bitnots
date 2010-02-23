package bitnots.expressions;



/**

 * @author Benjamin Shults

 * @version .2

 */



public class NoArityException extends Exception {

  

  public Symbol getSymbol() {

    return this.sym;

  }



  private Symbol sym;



  public NoArityException(Symbol s) {

    this.sym = s;

  }

  

} // NoArityException

