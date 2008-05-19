/**
 * Created on May 6, 2008
 */
package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Graphics;

import org.codemonk.wf.db.NodeRef;

public class DefaultNodePainter extends BaseNodePainter
{
  @Override
  public void paintNode (Graphics g, NodeRef node, int x, int y)
  {
    g.setColor( Color.blue );

    int maxRadius = NodeDrawConfig.getMaxNodeRadius();

    g.fillRect( x - maxRadius, y - maxRadius, maxRadius << 1, maxRadius << 1 );
  }

  @Override
  protected int getOffset ()
  {
    return NodeDrawConfig.getMaxNodeRadius();
  }
}