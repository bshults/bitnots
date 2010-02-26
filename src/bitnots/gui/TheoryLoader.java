package bitnots.gui;

import java.awt.Component;
import java.io.FileNotFoundException;

import javax.swing.JFileChooser;

import bitnots.parse.IPRParser;
import bitnots.theories.Theory;

/**
 * 
 * @author bshults
 *
 */
public class TheoryLoader {

  public static Theory getTheoryFromFile() {
    return TheoryLoader.getTheoryFromFile(Bitnots.FRAME, null);
  }

  public static Theory getTheoryFromFile(String filename) {
    return TheoryLoader.getTheoryFromFile(Bitnots.FRAME, filename);
  }

  public static Theory getTheoryFromFile(Component parent, String filename) {
    while (true) {
      final IPRParser parser;
      try {
        if (filename == null) {
          // have the user choose a file on load
          JFileChooser chooser = (System.getProperties().containsKey("user.dir"))
                                 ? new JFileChooser(System.getProperty(
              "user.dir")) : new JFileChooser();
          int returnVal = chooser.showOpenDialog(parent);
          if (returnVal != JFileChooser.APPROVE_OPTION)
            System.exit(0);
          filename = chooser.getSelectedFile().getAbsolutePath();
        }
        parser = new IPRParser(filename);
      } catch (FileNotFoundException e) {
        JFileChooser chooser = (System.getProperties().containsKey("user.dir"))
                               ? new JFileChooser(System.getProperty(
            "user.dir")) : new JFileChooser();
        /*chooser.addChoosableFileFilter(MenuMainGUI.bitnotsFilter);
        chooser.addChoosableFileFilter(MenuMainGUI.tptpFilter);
        chooser.setFileFilter(MenuMainGUI.bitnotsFilter); */
        int returnVal = chooser.showOpenDialog(parent);
        if (returnVal != JFileChooser.APPROVE_OPTION)
          System.exit(0);
        filename = chooser.getSelectedFile().getAbsolutePath();
        continue;
      }
      // use the parser to initialize conjectures and knowledge base
      return parser.getTheory();
    }
  }

  public static Theory loadDefaultFile() {
//    return TheoryLoader.getTheoryFromFile("files/bitnots/locally-compact.ipr");
    return TheoryLoader.getTheoryFromFile("files/bitnots/vector-spaces.ipr");
//    return TheoryLoader.getTheoryFromFile("files/bitnots/theorems.ipr");
  }
}

