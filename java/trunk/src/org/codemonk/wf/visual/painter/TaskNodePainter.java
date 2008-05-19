/**
 * Created on May 6, 2008
 */
package org.codemonk.wf.visual.painter;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import org.codemonk.wf.db.NodeRef;
import org.codemonk.wf.test.NodeTask;
import org.codemonk.wf.visual.NodeDrawConfig;

public class TaskNodePainter extends BaseNodePainter
{
  public static Font getMaxFont (Graphics g, String str, int maxWidth, int maxSize)
  {
    Font font = g.getFont();
    FontMetrics metrics= g.getFontMetrics( font );
    return font;
  }

  @Override
  public void paintNode (Graphics g, NodeRef node, int x, int y)
  {
    g.setColor( Color.blue );

    int maxRadius = NodeDrawConfig.getMaxNodeRadius();

    g.draw3DRect( x - maxRadius, y - maxRadius, maxRadius << 1, maxRadius << 1, false );

    NodeTask nodeTask = (NodeTask)node.getNode();

    String taskName = nodeTask.getTaskName();


    int width = g.getFontMetrics().stringWidth( taskName );
    // int height = g.getFontMetrics().getHeight();
    g.drawString( taskName, x - (width >> 1), y );
  }

  @Override
  protected int getOffset ()
  {
    return NodeDrawConfig.getMaxNodeRadius();
  }
}