package bitnots.output;

import java.util.Iterator;
/*
import com.lowagie.text.Paragraph;
import com.lowagie.text.Table;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
*/
import bitnots.tableaux.Tableau;
import bitnots.tableaux.TableauNode;
import bitnots.util.QueueList;


/**
 * 
 * @author bshults
 *
 */
public class Output {
  
  public static String tableauToText(Tableau tableau) {
    
    return null;
  }

  public static void tableauToPDF(Tableau tableau) {
//    PdfPTable table = new PdfPTable(1);
    
    // call setWidths after the page table has content in it.
    
    int rows = 1;
    
    TableauNode node = tableau.getRoot();
    QueueList<TableauNode> queue = new QueueList<TableauNode>();
    queue.enqueue(node);
    do {
      node = queue.dequeue();
      for (TableauNode child: node.getChildren()) {
        queue.enqueue(child);
      }
      
      // put the new forulas here into the current column
      
      
    } while (!queue.isEmpty());

/*    PdfPCell cell =
      new PdfPCell(new Paragraph("header with colspan 3"));
    cell.setColspan(3);
    table.addCell(cell);
    */
  /*  table.addCell("1.1");
    table.addCell("2.1");
    table.addCell("3.1");
    table.addCell("1.2");
    table.addCell("2.2");
    table.addCell("3.2");
    // document.add(table);
   * */
  }
  
  public static void printTableau(Tableau tableau) {
    
  }

}
