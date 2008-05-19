package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Graphics;

import org.codemonk.wf.db.NodeRef;

public class StartGraphTreeNode extends GraphTreeNode
{
  protected Color color = new Color( 102, 152, 102 );

  public StartGraphTreeNode( GraphTreeNode parent, NodeRef node )
  {
    super( parent, node );
  }

  @Override
  public void paintNode( Graphics g )
  {
    g.setColor( color );

    int maxRadius = NodeDrawConfig.getMaxNodeRadius();
    int offset = getOffset();
    g.fillOval( originX - offset, originY - offset, maxRadius, maxRadius);

    offset = maxRadius >> 2;

    int xd = originX + (maxRadius / 10);
    g.setColor( Color.white );
    g.fillPolygon( new int[] { xd - offset,       xd + offset, xd - offset },
                   new int[] { originY  - offset, originY,     originY  + offset },
                   3 );
  }

  protected int getOffset ()
  {
    return NodeDrawConfig.getMaxNodeRadius() >> 1;
  }
}
