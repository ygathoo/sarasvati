/**
 * Created on May 15, 2008
 */
package org.codemonk.wf.visual;

import java.awt.Shape;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;

import org.apache.commons.collections15.Transformer;
import org.codemonk.wf.db.HibNodeRef;

public class NodeShapeTransformer implements Transformer<HibNodeRef, Shape>
{
  @Override
  public Shape transform (HibNodeRef nodeRef)
  {
    if ( "task".equals( nodeRef.getType() ) )
    {
      return new Rectangle2D.Float( -25, -25, 25, 25 );
    }
    else
    {
      return new Ellipse2D.Float(-10,-10,20,20);
    }
  }

}
