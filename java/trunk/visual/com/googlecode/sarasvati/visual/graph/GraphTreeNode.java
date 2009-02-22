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
package com.googlecode.sarasvati.visual.graph;

import java.util.List;

import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.visual.common.NodeDrawConfig;

public class GraphTreeNode
{
  protected Node    node;
  protected int     depth;
  protected int     index;

  protected int     originX;
  protected int     originY;

  public GraphTreeNode (int depth, Node node)
  {
    this.node = node;
    this.depth = depth;
  }

  public Node getNode ()
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
    int xBasis = NodeDrawConfig.isVertical() ? getIndex() : getDepth();
    int yBasis = NodeDrawConfig.isVertical() ? getDepth() : getIndex();

    originX = ((xBasis) * NodeDrawConfig.getHorizontalNodeSpacing()) +
              (xBasis * (NodeDrawConfig.getMaxNodeRadius() << 1)) +
              NodeDrawConfig.getMaxNodeRadius();

    originY = ((yBasis) * NodeDrawConfig.getVerticalNodeSpacing()) +
              (yBasis * (NodeDrawConfig.getMaxNodeRadius() << 1)) +
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

  public int getOffset ()
  {
    return NodeDrawConfig.getMaxNodeRadius() >> 1;
  }
}