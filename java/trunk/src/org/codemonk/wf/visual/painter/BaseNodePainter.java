/**
 * Created on May 7, 2008
 */
package org.codemonk.wf.visual.painter;

import org.codemonk.wf.visual.NodeDrawConfig;
import org.codemonk.wf.visual.Point;

public abstract class BaseNodePainter implements NodePainter
{
  protected abstract int getOffset ();

  @Override
  public Point getLeftAnchor (int originX, int originY)
  {
    return new Point( originX - getOffset() - NodeDrawConfig.getAnchorSize(), originY );
  }

  @Override
  public Point getRightAnchor (int originX, int originY)
  {
    return new Point( originX + getOffset() + NodeDrawConfig.getAnchorSize(), originY );
  }

  @Override
  public Point getTopAnchor (int originX, int originY)
  {
    return new Point( originX, originY  + getOffset() + NodeDrawConfig.getAnchorSize());
  }
}
