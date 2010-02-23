package bitnots.util;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;


/**
 * 
 * @author bshults
 *
 */
public class ImageUtils {
  /**
   * Scale {@link javax.swing.ImageIcon}
   * @param icon {@link javax.swing.ImageIcon}
   * @param width {@link java.lang.Integer}
   * @param height {@link java.lang.Integer}
   * @return icon {@link javax.swing.ImageIcon}
   */
  public static ImageIcon scaleImageIcon(ImageIcon icon, int width, int height) {
      return new ImageIcon(icon.getImage().getScaledInstance(width, height, 
                                                             Image.SCALE_AREA_AVERAGING));
  }
  
  /**
   * Scale {@link javax.swing.ImageIcon} according to given scale
   * @param icon {@link javax.swing.ImageIcon}
   * @param scale {@link java.lang.Double}
   * @return {@link javax.swing.ImageIcon}
   */
  public static ImageIcon scaleImageIcon(ImageIcon icon, double scale) {
    int newWidth = (int)(icon.getIconWidth() * scale);
    int newHeight = (int) (icon.getIconHeight() * scale);
    return ImageUtils.scaleImageIcon(icon, newWidth, newHeight);
  }
}
