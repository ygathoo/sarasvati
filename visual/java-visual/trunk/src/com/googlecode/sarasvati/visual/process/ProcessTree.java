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
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;

public class ProcessTree
{
  protected Map<NodeToken, ProcessTreeNode> nodeTokenMap = new HashMap<NodeToken, ProcessTreeNode>();
  protected Map<Node, ProcessTreeNode>      nodeMap      = new HashMap<Node, ProcessTreeNode>();

  protected List<NodeToken> sortedTokenList = null;

  protected List<ProcessTreeNode> queue = new LinkedList<ProcessTreeNode>();

  protected ProcessTreeNode getProcessTreeNode (Node node)
  {
    for ( NodeToken token : sortedTokenList )
    {
      if (token.getNode().equals( node ) )
      {
        return nodeTokenMap.get( token );
      }
    }

    ProcessTreeNode endNode = nodeMap.get( node );

    if ( endNode == null )
    {
      endNode = new ProcessTreeNode( node );
      nodeMap.put( node, endNode );
      queue.add( endNode );
    }

    return endNode;
  }

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

    sortedTokenList = new ArrayList<NodeToken>( process.getNodeTokens() );

    Collections.sort( sortedTokenList, new Comparator<NodeToken>()
    {
      @Override
      public int compare( NodeToken o1, NodeToken o2 )
      {
        return o2.getCreateDate().compareTo( o1.getCreateDate() );
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
                            getProcessTreeNode( arcToken.getArc().getEndNode() ) );
      nodeTokenMap.get( arcToken.getParentToken() ).addChild( arcTokenWrapper );
    }

    // Process all arcs which don't have arc tokens on them
    for ( ProcessTreeNode ptNode : nodeTokenMap.values() )
    {
      for ( Arc arc : graph.getOutputArcs( ptNode.getNode() ) )
      {
        if ( !ptNode.isTokenOnArc( arc ) )
        {
          ProcessTreeArc arcTokenWrapper = new ProcessTreeArc( arc, ptNode, getProcessTreeNode( arc.getEndNode() ) );
          ptNode.addChild( arcTokenWrapper );
        }
      }
    }

    // Process all nodes without tokens entries in queue
    while ( !queue.isEmpty() )
    {
      ProcessTreeNode ptNode = queue.remove( 0 );
      for ( Arc arc : graph.getOutputArcs( ptNode.getNode() ) )
      {
        ProcessTreeArc arcTokenWrapper = new ProcessTreeArc( arc, ptNode, getProcessTreeNode( arc.getEndNode() ) );
        ptNode.addChild( arcTokenWrapper );
      }
    }

    List<ProcessTreeNode> nextLayer = new LinkedList<ProcessTreeNode>();
    Set<ProcessTreeNode> processed = new HashSet<ProcessTreeNode>();


    for ( ProcessTreeNode ptNode : nodeTokenMap.values() )
    {
      if ( ptNode.isStartTokenNode() )
      {
        ptNode.setDepth( 0 );
        ptNode.addToLayer( nextLayer );
        processed.add( ptNode );
      }
    }

    int depth = 1;

    while ( !nextLayer.isEmpty() )
    {
      List<ProcessTreeNode> prevLayer = nextLayer;
      nextLayer = new LinkedList<ProcessTreeNode>();

      for ( ProcessTreeNode treeNode : prevLayer )
      {
        for ( ProcessTreeArc ptArc : treeNode.getChildren() )
        {
          if ( !processed.contains( ptArc.getChild() ) )
          {
            ptArc.getChild().setDepth( depth );
            ptArc.getChild().addToLayer( nextLayer );
            processed.add( ptArc.getChild() );
          }
        }
      }
      depth++;
    }
  }

  public Iterable<ProcessTreeNode> getProcessTreeNodes ()
  {
    ArrayList<ProcessTreeNode> result = new ArrayList<ProcessTreeNode>( nodeTokenMap.size() + nodeMap.size() );
    result.addAll( nodeTokenMap.values() );
    result.addAll( nodeMap.values() );
    return result;
  }
}