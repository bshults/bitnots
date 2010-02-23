package bitnots.prover;

import java.util.*;
import bitnots.tableaux.*;
import bitnots.theories.*;

/**
 * @author Daniel W. Farmer
 * @version 1.0
 */

public class PrintKBTask extends AbstractTableauTask {
  public void run() {
    // get knowledge base
    List sequents = this.tableau.getTheory().getKB();

    // print knowledge base
    System.out.println(" -- Knowledge Base --\n**********************\n");
    for (int x = 0; x < sequents.size(); x++) {
      KBSequent s = (KBSequent) sequents.get(x);
      System.out.print(s);
      System.out.println("-----------");
    }
  }

  public PrintKBTask(Tableau tab) {
    super(tab);
  }
}
