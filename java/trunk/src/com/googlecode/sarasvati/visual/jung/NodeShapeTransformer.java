/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/
/**
 * Created on May 15, 2008
 */
package com.googlecode.sarasvati.visual.jung;

import java.awt.Shape;

import org.apache.commons.collections15.Transformer;

import com.googlecode.sarasvati.Node;

import edu.uci.ics.jung.visualization.VertexShapeFactory;

public class NodeShapeTransformer implements Transformer<Node, Shape>
{
  protected Transformer<Node, Integer> sizeTrans = new Transformer<Node, Integer>()
  {
    @Override
    public Integer transform( Node nodeRef )
    {
      if ( "task".equals( nodeRef.getType() ) )
      {
        return TaskIcon.WIDTH;
      }

      return 20;
    }
  };

  protected Transformer<Node, Float> aspectTrans = new Transformer<Node, Float>()
  {
    @Override
    public Float transform( Node nodeRef )
    {
      if ( "task".equals( nodeRef.getType() ) )
      {
        return (float)TaskIcon.HEIGHT / TaskIcon.WIDTH;
      }

      return 1f;
    }
  };

  protected VertexShapeFactory<Node> factory =
    new VertexShapeFactory<Node>( sizeTrans, aspectTrans );

  @Override
  public Shape transform (Node nodeRef)
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
