package bitnots.util;



import java.util.*;

import java.io.*;



/**

 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>

 * @version .2

 */



public class BitnotsResourceBundle extends PropertyResourceBundle {



  public BitnotsResourceBundle(InputStream is) throws IOException {

    super(is);

  }

  

  public BitnotsResourceBundle(String fileName) throws FileNotFoundException,

                                                       IOException {

    super(new FileInputStream(fileName));

  }

  

}// BitnotsResourceBundle

