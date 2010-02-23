package bitnots.test;

import java.util.Arrays;
import java.util.List;


/**
 * 
 * @author bshults
 *
 */
public class JavaTests {

  /**
   * 
   * @param args
   */
  public static void main(String[] args) {
    String[] stuff = new String[] {"1", "2"};
    List<String> otherStuff = Arrays.asList(stuff);
    for (String thing: otherStuff) {
      System.out.println(thing);
    }
    stuff[1] = "3";
    for (String thing: otherStuff) {
      System.out.println(thing);
    }
  }
}
