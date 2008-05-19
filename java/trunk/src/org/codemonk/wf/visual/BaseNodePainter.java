/**
 * Created on May 7, 2008
 */
package org.codemonk.wf.visual;

public abstract class BaseNodePainter implements NodePainter
{
  protected abstract int getOffset ();

  @Override
  public Point getLeftAnchor (int originX, int originY)
  {
    return new Point( originX - getOffset(), originY );
  }

  @Override
  public Point getRightAnchor (int originX, int originY)
  {
    return new Point( originX + getOffset(), originY );
  }

  @Override
  public Point getTopAnchor (int originX, int originY)
  {
    return new Point( originX, originY  + getOffset() );
  }
}
