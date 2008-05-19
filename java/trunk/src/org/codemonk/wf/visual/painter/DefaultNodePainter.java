/**
 * Created on May 6, 2008
 */
package org.codemonk.wf.visual.painter;

import java.awt.Color;
import java.awt.Graphics;

import org.codemonk.wf.db.NodeRef;
import org.codemonk.wf.visual.NodeDrawConfig;

public class DefaultNodePainter extends BaseNodePainter
{
  protected Color color = new Color( 0, 102, 204 );

  @Override
  public void paintNode (Graphics g, NodeRef node, int x, int y)
  {
    g.setColor( color );

    int maxRadius = NodeDrawConfig.getMaxNodeRadius();
    int offset = getOffset();
    g.fillOval( x - offset, y - offset, maxRadius, maxRadius);
  }

  @Override
  protected int getOffset ()
  {
    return NodeDrawConfig.getMaxNodeRadius() >> 1;
  }
}