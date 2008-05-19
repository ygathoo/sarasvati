/**
 * Created on May 6, 2008
 */
package org.codemonk.wf.visual.painter;

import java.awt.Color;
import java.awt.Graphics;

import org.codemonk.wf.db.NodeRef;
import org.codemonk.wf.visual.NodeDrawConfig;

public class StartNodePainter extends BaseNodePainter
{
  protected Color color = new Color( 102, 152, 102 );

  @Override
  public void paintNode (Graphics g, NodeRef node, int x, int y)
  {
    g.setColor( color );

    int maxRadius = NodeDrawConfig.getMaxNodeRadius();
    int offset = getOffset();
    g.fillOval( x - offset, y - offset, maxRadius, maxRadius);

    offset = maxRadius >> 2;

    int xd = x + (maxRadius / 10);
    g.setColor( Color.white );
    g.fillPolygon( new int[] { xd - offset, xd + offset, xd - offset },
                   new int[] { y  - offset, y,           y  + offset },
                   3 );
  }

  @Override
  protected int getOffset ()
  {
    return NodeDrawConfig.getMaxNodeRadius() >> 1;
  }
}
