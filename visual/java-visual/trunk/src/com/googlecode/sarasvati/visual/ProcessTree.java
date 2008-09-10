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
package com.googlecode.sarasvati.visual;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;

public class ProcessTree
{
  protected Map<NodeToken, NodeTokenWrapper> nodeMap = new HashMap<NodeToken, NodeTokenWrapper>();

  protected GraphTreeNode root = new GraphTreeNode( null, null );

  protected List<List<GraphTreeNode>> layers = new LinkedList<List<GraphTreeNode>>();

  private static class NodeTokenWrapper
  {
    private NodeToken token;
    private List<ArcToken> children;

    public NodeTokenWrapper (NodeToken token)
    {
      this.token = token;
      this.children = new LinkedList<ArcToken>();
    }

    public NodeToken getToken ()
    {
      return token;
    }

    public List<ArcToken> getParents ()
    {
      return token.getParentTokens();
    }

    public List<ArcToken> getChildren ()
    {
      return children;
    }

    public void addChild (ArcToken child)
    {
      children.add( child );
    }
  }

  @SuppressWarnings("unchecked")
  public ProcessTree (GraphProcess process)
  {
    List<NodeToken> tokenList = new ArrayList<NodeToken>( process.getNodeTokens() );

    Collections.sort( tokenList, new Comparator<NodeToken>()
    {
      @Override
      public int compare( NodeToken o1, NodeToken o2 )
      {
        return o1.getCreateDate().compareTo( o2.getCreateDate() );
      }
    });

    for ( NodeToken token : tokenList )
    {
      nodeMap.put( token, new NodeTokenWrapper( token ) );
    }

    for ( NodeToken token : tokenList )
    {
      for ( ArcToken parent : token.getParentTokens() )
      {
        nodeMap.get(  parent.getParentToken() ).addChild( parent );
      }
    }

    List<ProcessTreeNode> nextLayer = new LinkedList<ProcessTreeNode>();


  }

//  public GraphTreeNode getTreeNode (Node node)
//  {
//    return nodeMap.get( node );
//  }

  public int getLayerCount ()
  {
    return layers.size();
  }

  public List<GraphTreeNode> getLayer (int index)
  {
    return layers.get( index );
  }

  public List<List<GraphTreeNode>> getLayers ()
  {
    return layers;
  }

//  public Iterable<GraphTreeNode> getGraphTreeNodes ()
//  {
//    return nodeMap.values();
//  }
}