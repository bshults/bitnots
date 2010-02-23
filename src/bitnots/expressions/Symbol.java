package bitnots.expressions;

import java.lang.reflect.*;
import java.util.*;

/**
 * @author Benjamin Shults
 * @version .2
 */

public class Symbol {
  
  protected String name;

  public int hashCode() {
    return this.name.hashCode();
  }

  public boolean equals(Symbol s) {
    return this.name.equals(s.name);
  }

  public String toString() {
    return this.name;
  }

  /**
   * This will map classes to tables.  The table values will map
   * strings to symbols.  */
  private static HashMap TABLE_OF_TABLES = new HashMap(10);

  /**
   * Returns the symbol associated with the given pair.
   * @param type the type of the Constructor.  Should be a subclass of
   * Symbol.   */
  public static Symbol get(Class type, String s) {
    HashMap map = (HashMap) TABLE_OF_TABLES.get(type);
    if (map == null)
      return null;
    else
      return (Symbol) map.get(s);
  }

  /**
   * Associates the pair <b>type</b>, <b>s</b> with <b>symbol</b>.
   * @param type the type of the Constructor.  Should be a subclass of
   * Symbol.   */
  public static Symbol put(Class type, String s, Symbol symbol) {
    HashMap map = (HashMap) TABLE_OF_TABLES.get(type);
    if (map == null) {
      map = new HashMap();
      TABLE_OF_TABLES.put(type, map);
    }
    return (Symbol) map.put(s, symbol); 
  }

//    private static Symbol registerSymbol(String s, Symbol sym) {
//      return (Symbol) TABLE.put(s, sym);
//    }

  /**
   * If there is no symbol associated with the given pair, then create
   * one, associate it and return it.  If there is a symbol associated
   * with the given pair, then return it.  Don't use this with
   * variables.
   * @param type the type of the Constructor.  Should be a subclass of
   * Symbol. */
  public static Symbol putOrGet(Class type, String s) {
    HashMap map = (HashMap) TABLE_OF_TABLES.get(type);
    Symbol value;
    if (map == null) {
      map = new HashMap();
      TABLE_OF_TABLES.put(type, map);
      value = null;
    } else {
      value = (Symbol) map.get(s);
    }
    // map is nonnull
    if (value == null) {
      try {
        value = (Symbol) type.getConstructor(
          new Class[] {String.class}).newInstance(new Object[] {s});
        map.put(s, value);
      } catch (NoSuchMethodException nsme) {
        nsme.printStackTrace();
        return null;
      } catch (IllegalAccessException iae) {
        iae.printStackTrace();
        return null;
      } catch (InstantiationException ie) {
        ie.printStackTrace();
        return null;
      } catch (InvocationTargetException ite) {
        ite.printStackTrace();
        return null;
      }
    }
    // value is nonnull and is in map.
    return value;
  }

//    /** Currently, this is first come - first served: The first symbol
//     * to register a string is saved in the hash table.  If another type
//     * of symbol wants to use the same string, then a new instance of
//     * that second type will have to be created at each call.
//     */
//    public static Symbol getSymbol(String symbol, Class symbolType) {
//      Symbol s = (Symbol) TABLE.get(symbol);
//      try {
//        if (s == null) {
//          s = (Symbol) symbolType.getConstructor(
//            new Class[] {String.class}).newInstance(new Object[] {symbol});
//          registerSymbol(symbol, s);
//          return s;
//        } else if (symbolType.isAssignableFrom(s.getClass()))
//          return s;
//        else
//          return (Symbol) symbolType.getConstructor(
//            new Class[] {String.class}).newInstance(new Object[] {symbol});
//      } catch (NoSuchMethodException e) {
//        e.printStackTrace();
//        return null;
//      } catch (InvocationTargetException e) {
//        e.printStackTrace();
//        return null;
//      } catch (InstantiationException e) {
//        e.printStackTrace();
//        return null;
//      } catch (IllegalAccessException e) {
//        e.printStackTrace();
//        return null;
//      }
//    }

  public String getName() {
    return this.name;
  }

  public Symbol(String guts) {
    this.name = guts;
    Symbol.put(this.getClass(), guts, this);
  }
  
} // Symbol
