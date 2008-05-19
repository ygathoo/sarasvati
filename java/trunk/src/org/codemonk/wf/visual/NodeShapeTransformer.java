/**
 * Created on May 15, 2008
 */
package org.codemonk.wf.visual;

import java.awt.Shape;

import org.apache.commons.collections15.Transformer;
import org.codemonk.wf.db.HibNodeRef;

import edu.uci.ics.jung.visualization.VertexShapeFactory;

public class NodeShapeTransformer implements Transformer<HibNodeRef, Shape>
{
  protected Transformer<HibNodeRef, Integer> sizeTrans = new Transformer<HibNodeRef, Integer>()
  {
    @Override
    public Integer transform( HibNodeRef arg0 )
    {
      return 40;
    }
  };

  protected Transformer<HibNodeRef, Float> aspectTrans = new Transformer<HibNodeRef, Float>()
  {
    @Override
    public Float transform( HibNodeRef arg0 )
    {
      return 1f;
    }
  };

  protected VertexShapeFactory<HibNodeRef> factory =
    new VertexShapeFactory<HibNodeRef>( sizeTrans, aspectTrans );

  @Override
  public Shape transform (HibNodeRef nodeRef)
  {
    if ( "task".equals( nodeRef.getType() ) )
    {
      return factory.getRoundRectangle( nodeRef );
    }
    else
    {
      return factory.getEllipse( nodeRef );
    }
  }

}
