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
package com.googlecode.sarasvati.visual.process;

import java.util.List;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.visual.NodeDrawConfig;

public class ProcessTreeNode
{
  protected NodeToken token;
  protected Node      node;
  protected int       depth;
  protected int       index;

  protected int       originX;
  protected int       originY;

  private List<ProcessTreeArc> children;

  public ProcessTreeNode (Node node)
  {
    this.node = node;
  }

  public ProcessTreeNode (NodeToken token)
  {
    this.token = token;
  }

  public Node getNode ()
  {
    return token == null ? node : token.getNode();
  }

  public NodeToken getToken ()
  {
    return token;
  }

  public List<ProcessTreeArc> getChildren ()
  {
    return children;
  }

  public void addChild (ProcessTreeArc child)
  {
    children.add( child );
  }

  public int getDepth ()
  {
    return depth;
  }

  public void setDepth (int depth)
  {
    this.depth = depth;
  }

  public int getIndex()
  {
    return index;
  }

  public void setIndex( int index )
  {
    this.index = index;
  }

  public void addToLayer (List<ProcessTreeNode> layer)
  {
    this.index = layer.size();
    layer.add( this );
    recalculateOrigin();
  }

  public boolean isStartTokenNode ()
  {
    return token != null && token.getParentTokens().isEmpty() && token.getNode().isStart();
  }

  public boolean isTokenOnArc (Arc arc)
  {
    for (ProcessTreeArc ptArc : children )
    {
      if ( ptArc.getArc().equals( arc ) && ptArc.getToken() != null )
      {
        return true;
      }
    }

    return false;
  }

  public void recalculateOrigin ()
  {
    int xBasis = NodeDrawConfig.isVertical() ? getIndex() : getDepth();
    int yBasis = NodeDrawConfig.isVertical() ? getDepth() : getIndex();

    originX = ((xBasis + 1) * NodeDrawConfig.getHorizontalNodeSpacing()) +
              (xBasis * (NodeDrawConfig.getMaxNodeRadius() << 1)) +
              NodeDrawConfig.getMaxNodeRadius();

    originY = ((yBasis + 1) * NodeDrawConfig.getVerticalNodeSpacing()) +
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