package bitnots.parse;

import bitnots.expressions.Formula;

/**
 * A wrapper object for a literal matched in the CNFParser class.  It
 * stores the sign and the formula matched for that literal.
 * @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
 * @version 1.0
 */
public class CNFLiteral
{
    /**
     * The formula contained in this literal.
     */
    Formula formula;
    
    /**
     * The sign of this literal.  ++ is true and -- is false.
     */
    boolean sign;
    
    /**
     * Returns the formula contained in this literal.
     * @return the formula contained in this literal.
     */
    public Formula getFormula() {
        return this.formula;
    }
    
    /**
     * Returns the sign of this literal.
     * @return the sign of this literal.
     */
    public boolean getSign() {
        return this.sign;
    }
    
    /**
     * Returns the String representation of this literal.  The string
     * is the sign (++ or -- for true and false, respectively)
     * followed by the literal's formula.
     * @return the String representation of this literal.
     */
    public String toString() {
        String result;
        if (this.sign)
            result = "-- ";
        else
            result = "++ ";
        if (this.formula == null)
            return result;
        return result.concat(this.formula.toString());
    }
    
    /**
     * Constructs a new CNFLiteral with a Formula and a boolean for
     * the literals formula and its sign.
     * @param f the formula for this literal.
     * @param s the sign for this literal.
     */
    public CNFLiteral(Formula f, boolean s) {
        this.formula = f;
        this.sign = s;
    }
}