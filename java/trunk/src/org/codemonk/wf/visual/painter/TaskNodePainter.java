/**
 * Created on May 6, 2008
 */
package org.codemonk.wf.visual.painter;

import java.awt.Color;
import java.awt.Graphics;

import org.codemonk.wf.db.NodeRef;
import org.codemonk.wf.test.NodeTask;
import org.codemonk.wf.visual.NodeDrawConfig;

public class TaskNodePainter extends BaseNodePainter
{
  /*
  public static Font getMaxFont (Graphics g, String str, int maxWidth, int maxSize)
  {
    Font font = g.getFont();
    FontMetrics metrics= g.getFontMetrics( font );
    return font;
  }
  */



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

  @Override
  public void paintLeftIncomingAnchor( Graphics g, NodeRef node, int x, int y )
  {
    int size = NodeDrawConfig.getAnchorSize();
    int base = x - getOffset();

    g.fillPolygon( new int[] { base - size, base, base - size },
                   new int[] { y    - size, y,    y    + size },
                   3 );
  }

  @Override
  public void paintRightIncomingAnchor( Graphics g, NodeRef node, int x, int y )
  {
    int size = NodeDrawConfig.getAnchorSize();
    int base = x + getOffset();

    g.fillPolygon( new int[] { base + size, base, base + size },
                   new int[] { y    - size, y,    y    + size },
                   3 );
  }


}