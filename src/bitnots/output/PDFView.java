package bitnots.output;
/*
import com.sun.pdfview.PDFFile;
import com.sun.pdfview.PDFPage;
import com.sun.pdfview.PagePanel;
*/
import java.awt.Image;
import java.awt.Rectangle;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

/**
 * 
 * Got the beginnings of this from 
 * https://pdf-renderer.dev.java.net/examples.html
 *
 */
public class PDFView extends JPanel {

  public PDFView(String filename) throws IOException {
  //  PagePanel panel = new PagePanel();

    //load a pdf from a byte buffer
    File file = new File(filename);
    RandomAccessFile raf = new RandomAccessFile(file, "r");
    FileChannel channel = raf.getChannel();
    ByteBuffer buf = channel.map(FileChannel.MapMode.READ_ONLY,
                                 0, channel.size());
    /*PDFFile pdffile = new PDFFile(buf);

    // show the first page
    PDFPage page = pdffile.getPage(0);
    panel.showPage(page);
    this.add(panel);
     */
  }
}

/**
 * From here down came from
 * 


 */
/**
 * An example of drawing a PDF to an image.
 *
 * @author joshua.marinacci@sun.com
 */
class ImageMain {

  public static void setup() throws IOException {

    //load a pdf from a byte buffer
    File file = new File("test.pdf");
    RandomAccessFile raf = new RandomAccessFile(file, "r");
    FileChannel channel = raf.getChannel();
    ByteBuffer buf = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size());
    /*PDFFile pdffile = new PDFFile(buf);

    // draw the first page to an image
    PDFPage page = pdffile.getPage(0);

    //get the width and height for the doc at the default zoom 
    Rectangle rect = new Rectangle(0,0,
                                   (int)page.getBBox().getWidth(),
                                   (int)page.getBBox().getHeight());

    //generate the image
    Image img = page.getImage(
                              rect.width, rect.height, //width & height
                              rect, // clip rect
                              null, // null for the ImageObserver
                              true, // fill background with white
                              true  // block until drawing is done
    );

    //show the image in a frame
    JFrame frame = new JFrame("PDF Test");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.add(new JLabel(new ImageIcon(img)));
    frame.pack();
    frame.setVisible(true);
     */
  }

  public static void main(final String[] args) {
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        try {
          ImageMain.setup();
        } catch (IOException ex) {
          ex.printStackTrace();
        }
      }
    });
  }
}

/* How do I draw a PDF directly to my own Graphics2D object?

Sometimes you may need to draw directly to some other Graphics2D object instead of directly to an image. A common example is printing. The PDFRenderer lets you draw directly to a Graphics2D object rather than returning an image.

The following code draws a pdf into the Graphics2D from a BufferedImage
 

File file = new File("/Users/joshy/splats.pdf");

// set up the PDF reading
RandomAccessFile raf = new RandomAccessFile(file, "r");
FileChannel channel = raf.getChannel();
ByteBuffer buf = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size());
PDFFile pdffile = new PDFFile(buf);

// get the first page
PDFPage page = pdffile.getPage(0);


// create and configure a graphics object
BufferedImage img = new BufferedImage(100, 100, BufferedImage.TYPE_INT_ARGB);
Graphics2D g2 = img.createGraphics();
g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

// do the actual drawing
PDFRenderer renderer = new PDFRenderer(page, g2, 
                                       new Rectangle(0, 0, 500, 500), null, Color.RED);
page.waitForFinish();
renderer.run();

*/