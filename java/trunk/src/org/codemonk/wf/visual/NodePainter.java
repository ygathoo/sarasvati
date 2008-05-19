/**
 * Created on May 6, 2008
 */
package org.codemonk.wf.visual;

import java.awt.Graphics;

import org.codemonk.wf.db.NodeRef;

public interface NodePainter
{
  void paintNode (Graphics g, NodeRef node, int x, int y);

  Point getLeftAnchor (int originX, int originY);

  Point getRightAnchor (int originX, int originY);

  Point getTopAnchor (int originX, int originY);
}