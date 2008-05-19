package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Paint;

import org.apache.commons.collections15.Transformer;
import org.codemonk.wf.db.HibNodeRef;

public class NodeColorTransformer implements Transformer<HibNodeRef, Paint>
{
  protected Color startColor = new Color( 102, 152, 102 );
  protected Color nodeColor  = new Color( 102, 102, 152 );

  @Override
  public Paint transform( HibNodeRef nodeRef )
  {
    if ( "start".equals( nodeRef.getType() ) )
    {
      return startColor;
    }
    else if ( "task".equals(  nodeRef.getType() ) )
    {
      return Color.blue;
    }

    return nodeColor;
  }
}
