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

    Copyright 2008-2009 Paul Lorenz
*/
package com.googlecode.sarasvati.visual.graph;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public abstract class AbstractLayoutTree<N>
{
  protected Map<N, GraphLayoutNode<N>> nodeMap = new HashMap<N, GraphLayoutNode<N>>();

  protected abstract Collection<N> getNodes ();
  protected abstract Collection<N> getStartNodes ();
  protected abstract boolean hasNoInputs(N node);
  protected abstract Collection<N> getOutputs (N node);

  @SuppressWarnings("unchecked")
  public void init ()
  {
    List<GraphLayoutNode<N>> nextLayer = new LinkedList<GraphLayoutNode<N>>();

    Collection<N> startNodes = getStartNodes();

    for ( N node : getNodes() )
    {
      if ( hasNoInputs( node ) && !startNodes.contains( node ))
      {
        startNodes.add( node );
      }
    }

    if ( startNodes.isEmpty() )
    {
      Collection<N> nodes = getNodes();

      if ( !nodes.isEmpty() )
      {
        startNodes = new ArrayList<N>(1);
        startNodes.add( nodes.iterator().next() );
      }
    }

    for ( N node : startNodes )
    {
      GraphLayoutNode<N> treeNode = new GraphLayoutNode<N>( 0, node );

      nodeMap.put( node, treeNode );
      treeNode.addToLayer( nextLayer );
    }

    List<GraphLayoutNode<N>> layer = null;

    int depth = 1;

    while ( !nextLayer.isEmpty() )
    {
      layer = nextLayer;
      nextLayer = new LinkedList<GraphLayoutNode<N>>();

      for ( GraphLayoutNode<N> treeNode : layer )
      {
        for ( N target : getOutputs( treeNode.getNode() ) )
        {
          GraphLayoutNode<N> targetTreeNode = nodeMap.get( target );

          if (targetTreeNode == null)
          {
            targetTreeNode = new GraphLayoutNode( depth, target );
            nodeMap.put( target, targetTreeNode );
            targetTreeNode.addToLayer( nextLayer );
          }
        }
      }
      depth++;
    }
  }

  public GraphLayoutNode<N> getTreeNode (N node)
  {
    return nodeMap.get( node );
  }

  public boolean isBackArc (N start, N end)
  {
    GraphLayoutNode<N> startNode = getTreeNode( start );
    GraphLayoutNode<N> endNode = getTreeNode( end );
    return endNode.getDepth() < startNode.getDepth();
  }
}