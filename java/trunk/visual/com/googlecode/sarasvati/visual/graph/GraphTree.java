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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;

public class GraphTree
{
  protected Map<Node, GraphTreeNode> nodeMap = new HashMap<Node, GraphTreeNode>();

  @SuppressWarnings("unchecked")
  public GraphTree (Graph graph)
  {
    List<GraphTreeNode> nextLayer = new LinkedList<GraphTreeNode>();

    List<Node> startNodes = (List<Node>)graph.getStartNodes();

    for ( Node node : graph.getNodes() )
    {
      if ( graph.getInputArcs( node ).isEmpty() && !startNodes.contains( node ))
      {
        startNodes.add( node );
      }
    }

    if ( startNodes.isEmpty() )
    {
      List<? extends Node> nodeRefs = graph.getNodes();

      if ( !nodeRefs.isEmpty() )
      {
        startNodes = new ArrayList<Node>(1);
        startNodes.add( nodeRefs.get( 0 ) );
      }
    }

    for ( Node node : startNodes )
    {
      GraphTreeNode treeNode = new GraphTreeNode( 0, node );

      nodeMap.put( node, treeNode );
      treeNode.addToLayer( nextLayer );
    }

    List<GraphTreeNode> layer = null;

    int depth = 1;

    while ( !nextLayer.isEmpty() )
    {
      layer = nextLayer;
      nextLayer = new LinkedList<GraphTreeNode>();

      for ( GraphTreeNode treeNode : layer )
      {
        List<? extends Arc> arcs = graph.getOutputArcs( treeNode.getNode() );

        for ( Arc arc : arcs )
        {
          Node target = arc.getEndNode();
          GraphTreeNode targetTreeNode = nodeMap.get( target );

          if (targetTreeNode == null)
          {
            targetTreeNode = new GraphTreeNode( depth, target );
            nodeMap.put( target, targetTreeNode );
            targetTreeNode.addToLayer( nextLayer );
          }
        }
      }
      depth++;
    }
  }

  public GraphTreeNode getTreeNode (Node node)
  {
    return nodeMap.get( node );
  }

  public boolean isBackArc (Arc arc)
  {
    GraphTreeNode startNode = getTreeNode( arc.getStartNode() );
    GraphTreeNode endNode = getTreeNode( arc.getEndNode() );
    return endNode.getDepth() < startNode.getDepth();
  }
}