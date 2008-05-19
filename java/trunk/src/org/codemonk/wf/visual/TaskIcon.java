/**
 * Created on May 16, 2008
 */
package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;

import javax.swing.Icon;

import org.codemonk.wf.test.NodeTask;

public class TaskIcon implements Icon
{
  public static final int WIDTH  = 60;
  public static final int HEIGHT = 40;

  private static final Color darkBlue = new Color( 0, 0, 128 );
  private static final Color cream    = new Color( 255, 255, 245 );

  protected NodeTask task;

  public TaskIcon( NodeTask task )
  {
    this.task = task;
  }

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
  public void paintIcon (Component c, Graphics g, int x, int y)
  {
    g.setColor( cream );
    g.fillRoundRect( x, y, getIconWidth(), getIconHeight(), 10, 10 );
    g.setColor( darkBlue );
    g.drawRoundRect( x, y, getIconWidth(), getIconHeight(), 10, 10 );

    g.setColor( Color.black );
    String taskName = task.getTaskName();

    //int width = g.getFontMetrics().stringWidth( taskName );
    //int height = g.getFontMetrics().getHeight();
    String[] lines = FontUtil.split( taskName );

    int padding = 2;
    int startX = x + padding;

    int maxWidth = getIconWidth() - (padding << 1);

    if ( lines.length == 1 )
    {
      FontUtil.setSizedFont( g, taskName, 11, maxWidth );
      g.drawString( taskName, startX, y + (getIconWidth() >> 1) );
    }
    else
    {
      FontUtil.setSizedFont( g, lines[0], 11, maxWidth );

      int strWidth = (int)Math.ceil( g.getFontMetrics().getStringBounds( lines[0], g ).getWidth() );
      int left = startX + ((maxWidth - strWidth) >> 1);
      g.drawString( lines[0], left, y + (getIconHeight() >> 1) );

      FontUtil.setSizedFont( g, lines[1], 11, maxWidth );

      int height = (int)Math.ceil( g.getFontMetrics().getStringBounds( lines[1], g ).getHeight() );
      strWidth = (int)Math.ceil( g.getFontMetrics().getStringBounds( lines[1], g ).getWidth() );
      left = startX + ((maxWidth - strWidth) >> 1);

      g.drawString( lines[1], left, y + (getIconHeight() >> 1) + (height + 1) );
    }
  }
}
