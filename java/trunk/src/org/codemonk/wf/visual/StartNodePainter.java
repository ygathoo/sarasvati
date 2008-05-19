/**
 * Created on May 6, 2008
 */
package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Graphics;

import org.codemonk.wf.db.NodeRef;

public class StartNodePainter implements NodePainter
{
  protected Color color = new Color( 102, 152, 102 );

  @Override
  public void paintNode (Graphics g, NodeRef node, int x, int y)
  {
    g.setColor( color );
    g.fillOval( x, y, 20, 20 );
  }
}
