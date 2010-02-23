/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package bitnots.expressions;

import java.util.Map;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author bshults
 */
public class ForallTest {

  public ForallTest() {
  }

  @BeforeClass
  public static void setUpClass() throws Exception {
  }

  @AfterClass
  public static void tearDownClass() throws Exception {
  }

  @Before
  public void setUp() {
  }

  @After
  public void tearDown() {
  }

  /**
   * Test of replaceVariables method, of class Forall.
   */
  @Test
  public void testReplaceVariables() {
    System.out.println("replaceVariables");
    Map map = null;
    Forall instance = null;
    Formula expResult = null;
    Formula result = instance.replaceVariables(map);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of replaceUnboundOccurrencesWith method, of class Forall.
   */
  @Test
  public void testReplaceUnboundOccurrencesWith() {
    System.out.println("replaceUnboundOccurrencesWith");
    Variable v = null;
    Term t = null;
    Forall instance = null;
    Formula expResult = null;
    Formula result = instance.replaceUnboundOccurrencesWith(v, t);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }

  /**
   * Test of apply method, of class Forall.
   */
  @Test
  public void testApply() {
    System.out.println("apply");
    Substitution s = null;
    Forall instance = null;
    Formula expResult = null;
    Formula result = instance.apply(s);
    assertEquals(expResult, result);
    // TODO review the generated test code and remove the default call to fail.
    fail("The test case is a prototype.");
  }
}
