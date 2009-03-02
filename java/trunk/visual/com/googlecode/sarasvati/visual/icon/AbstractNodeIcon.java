package com.googlecode.sarasvati.visual.icon;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

import javax.swing.Icon;

import com.googlecode.sarasvati.visual.common.NodeDrawConfig;

public abstract class AbstractNodeIcon implements Icon
{
  public static final int WIDTH  = 100;
  public static final int HEIGHT = NodeDrawConfig.getMaxNodeRadius() << 1;

  protected BufferedImage image;

  @Override
  public int getIconHeight ()
  {
    return HEIGHT;
  }

  @Override
  public int getIconWidth ()
  {
    return WIDTH;
  }

  @Override
  public void paintIcon(Component c, Graphics g, int x, int y)
  {
    g.drawImage( image, x, y, c );
  }

  public void redrawImage ()
  {
    image = new BufferedImage( getIconWidth(), getIconHeight(), BufferedImage.TYPE_4BYTE_ABGR );
    Graphics2D g = image.createGraphics();
    redrawImage( g );
    g.dispose();
  }

  public abstract void redrawImage (Graphics2D g);
}
