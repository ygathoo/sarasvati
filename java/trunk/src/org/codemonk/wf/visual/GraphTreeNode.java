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
package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Graphics;
import java.util.List;

import org.codemonk.wf.db.HibNodeRef;

public class GraphTreeNode
{
  protected GraphTreeNode parent;

  protected HibNodeRef node;
  protected int     depth;
  protected int     index;

  protected int     originX;
  protected int     originY;

  protected Color   color = new Color( 102, 152, 102 );

  public static GraphTreeNode newInstance (GraphTreeNode parent, HibNodeRef nodeRef)
  {
    if ( "start".equalsIgnoreCase( nodeRef.getType() ) )
    {
      return new StartGraphTreeNode( parent, nodeRef );
    }
    else if ( "task".equalsIgnoreCase( nodeRef.getType() ) )
    {
      return new TaskGraphTreeNode( parent, nodeRef );
    }
    else
    {
      return new GraphTreeNode( parent, nodeRef );
    }
  }


  public GraphTreeNode (GraphTreeNode parent, HibNodeRef node)
  {
    this.node = node;
    this.parent = parent;

    if ( parent != null )
    {
      this.depth = parent.getDepth() + 1;
    }
    else
    {
      depth = -1;
    }
  }

  public HibNodeRef getNode ()
  {
    return node;
  }

  public int getDepth ()
  {
    return depth;
  }

  public int getIndex()
  {
    return index;
  }

  public void setIndex( int index )
  {
    this.index = index;
  }

  public void addToLayer (List<GraphTreeNode> layer)
  {
    this.index = layer.size();
    layer.add( this );
    recalculateOrigin();
  }

  public void recalculateOrigin ()
  {
    originX = ((getDepth() + 1) * NodeDrawConfig.getNodeSpacing()) +
              (getDepth() * (NodeDrawConfig.getMaxNodeRadius() << 1)) +
              NodeDrawConfig.getMaxNodeRadius();

    originY = ((getIndex() + 1) * NodeDrawConfig.getNodeSpacing()) +
              (getIndex() * (NodeDrawConfig.getMaxNodeRadius() << 1)) +
              NodeDrawConfig.getMaxNodeRadius();
  }

  public int getOriginX ()
  {
    return originX;
  }

  public int getOriginY ()
  {
    return originY;
  }

  public void paintNode (Graphics g)
  {
    g.setColor( color );

    int maxRadius = NodeDrawConfig.getMaxNodeRadius();
    int offset = getOffset();
    g.fillOval( originX - offset, originY - offset, maxRadius, maxRadius);
  }

  public int getOffset ()
  {
    return NodeDrawConfig.getMaxNodeRadius() >> 1;
  }

  public Point getLeftAnchor ()
  {
    return new Point( originX - getOffset() - NodeDrawConfig.getAnchorSize(), originY );
  }

  public Point getRightAnchor ()
  {
    return new Point( originX + getOffset() + NodeDrawConfig.getAnchorSize(), originY );
  }

  public Point getTopAnchor ()
  {
    return new Point( originX, originY  + getOffset() + NodeDrawConfig.getAnchorSize());
  }

  public void paintLeftIncomingAnchor( Graphics g )
  {
    int size = NodeDrawConfig.getAnchorSize();
    int base = originX - getOffset();

    g.fillPolygon( new int[] { base    - size, base,    base    - size },
                   new int[] { originY - size, originY, originY + size },
                   3 );
  }

  public void paintRightIncomingAnchor( Graphics g )
  {
    int size = NodeDrawConfig.getAnchorSize();
    int base = originX + getOffset();

    g.fillPolygon( new int[] { base    + size, base,   base    + size },
                   new int[] { originY - size, originY,originY + size },
                   3 );
  }
}