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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;

public class ProcessTree
{
  protected Map<NodeToken, ProcessTreeNode> nodeTokenMap = new HashMap<NodeToken, ProcessTreeNode>();

  protected List<List<ProcessTreeNode>> layers = new LinkedList<List<ProcessTreeNode>>();

  /*
   * 1. Find start node tokens
   * 2. Add start node tokens to first layer
   * 3. For each layer: for each element in the layer
   *   a. If it's a node token, process all arc token children
   *   b. If it's a node, process all arc children
   */
  public ProcessTree (GraphProcess process)
  {
    Graph graph = process.getGraph();

    for ( NodeToken token : process.getNodeTokens() )
    {
      nodeTokenMap.put( token, new ProcessTreeNode( token ) );
    }

    List<NodeToken> sortedTokenList = new ArrayList<NodeToken>( process.getNodeTokens() );

    Collections.sort( sortedTokenList, new Comparator<NodeToken>()
    {
      @Override
      public int compare( NodeToken o1, NodeToken o2 )
      {
        return o1.getCreateDate().compareTo( o2.getCreateDate() );
      }
    });

    for ( NodeToken token : nodeTokenMap.keySet() )
    {
      for ( ArcToken parent : token.getParentTokens() )
      {
        ProcessTreeArc processTreeArc =
          new ProcessTreeArc( parent,
                               nodeTokenMap.get( parent.getParentToken() ),
                               nodeTokenMap.get( token ) );
        nodeTokenMap.get( parent.getParentToken() ).addChild( processTreeArc );
      }
    }

    // active tokens won't have been processed in the previous step
    for ( ArcToken arcToken : process.getActiveArcTokens() )
    {
      ProcessTreeArc arcTokenWrapper =
        new ProcessTreeArc( arcToken,
                             nodeTokenMap.get( arcToken.getParentToken() ),
                             null );
      nodeTokenMap.get( arcToken.getParentToken() ).addChild( arcTokenWrapper );
    }

    List<ProcessTreeNode> queue = new LinkedList<ProcessTreeNode>();

    for ( ProcessTreeNode ptNode : nodeTokenMap.values() )
    {
      for ( Arc arc : graph.getOutputArcs( ptNode.getNode() ) )
      {
        if ( !ptNode.isTokenOnArc( arc ) )
        {
          ProcessTreeArc arcTokenWrapper =
            new ProcessTreeArc( arc,
                                nodeTokenMap.get( ptNode ),
                                null );
          ptNode.addChild( arcTokenWrapper );
        }
      }
    }

    List<ProcessTreeNode> nextLayer = new LinkedList<ProcessTreeNode>();

    for ( ProcessTreeNode ptNode : nodeTokenMap.values() )
    {
      if ( ptNode.isStartTokenNode() )
      {
        ptNode.addToLayer( nextLayer );
        ptNode.setDepth( 0 );
      }
    }

    int depth = 1;

    while ( !nextLayer.isEmpty() )
    {
      layers.add( nextLayer );
      List<ProcessTreeNode> prevLayer = nextLayer;
      nextLayer = new LinkedList<ProcessTreeNode>();

      for ( ProcessTreeNode treeNode : prevLayer )
      {
        for ( ProcessTreeArc wrapper : treeNode.getChildren() )
        {
          wrapper.getChild().addToLayer( nextLayer );
          wrapper.getChild().setDepth( depth );
        }
      }
      depth++;
    }
  }

//  public GraphTreeNode getTreeNode (Node node)
//  {
//    return nodeMap.get( node );
//  }

  public int getLayerCount ()
  {
    return layers.size();
  }

  public List<ProcessTreeNode> getLayer (int index)
  {
    return layers.get( index );
  }

  public List<List<ProcessTreeNode>> getLayers ()
  {
    return layers;
  }

//  public Iterable<GraphTreeNode> getGraphTreeNodes ()
//  {
//    return nodeMap.values();
//  }
}