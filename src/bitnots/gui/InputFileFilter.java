package bitnots.gui;

import java.io.File;
import javax.swing.filechooser.FileFilter;

/**
 * @author <a href="mailto:walpet@bethel.edu">Pete Wall</a>
 * @version 1.0
 */
public class InputFileFilter extends FileFilter
{
    /**
     * The acceptable extension for this file filter.
     */
    private String extension;
    
    /**
     * The description for this file filter.
     */
    private String description;

    /**
     * Whether the given file is accepted by this filter.
     * @return true if the given file is accepted by this filter.
     */
    public boolean accept(File f)
    {
        if (f.isDirectory()) {
            return true;
        }

        String ext = this.getExtensionFromFile(f);
        if (ext != null && this.openableExtension(ext)) {
            return true;
        }

        return false;
    }

    /**
     * Returns the description of this filter.
     * @return the description of this filter.
     */
    public String getDescription() {
        return this.description;
    }
    
    /**
     * Returns true if Bitnots can open the file.
     * @param extension the extension of the file attempting to be
     * loaded.
     * @return true if Bitnots can open the file.
     */
    private boolean openableExtension(String extension)
    {
        if (extension.toLowerCase().equals(this.extension)) {
            return true;
        }
        return false;
    }

    /**
     * Returns the extension of a file.
     * @param f the file to get the extension of.
     * @return the extension of a file.
     */
    private String getExtensionFromFile(File f)
    {
        String s = f.getName();
        int i = s.lastIndexOf('.');

        if (i > 0 && i < s.length() - 1) {
            return s.substring(i + 1).toLowerCase();
        }
        return null;
    }
    
    /**
     * Constructs a new InputFileFilter with an extension and a
     * description for that extension.
     */
    public InputFileFilter(String ext, String desc)
    {
        this.extension = ext;
        this.description = desc;
    }
}
