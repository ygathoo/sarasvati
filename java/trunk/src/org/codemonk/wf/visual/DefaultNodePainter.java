/**
 * Created on May 6, 2008
 */
package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Graphics;

import org.codemonk.wf.db.NodeRef;

public class DefaultNodePainter implements NodePainter
{
  @Override
  public void paintNode (Graphics g, NodeRef node, int x, int y)
  {
    g.setColor( Color.blue );
    g.fillRect( x, y, 20, 20 );
  }
}
