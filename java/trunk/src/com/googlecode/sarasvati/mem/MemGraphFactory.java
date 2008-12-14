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

package com.googlecode.sarasvati.mem;

import java.util.List;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.CustomNode;
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.ExecutionType;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.load.AbstractGraphFactory;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.load.NodeFactory;

public class MemGraphFactory extends AbstractGraphFactory<MemGraph>
{
  public static final MemGraphFactory INSTANCE = new MemGraphFactory();

  public MemGraphFactory ()
  {
    super( MemNode.class );
    this.defaultNodeFactory.addType( "wait",   MemWaitNode.class );
    this.defaultNodeFactory.addType( "script", MemScriptNode.class );
    this.defaultNodeFactory.addType( "nested", MemNestedProcessNode.class );
  }

  @Override
  public MemGraph newGraph (String name, int version)
  {
    return new MemGraph( name );
  }

  @Override
  public Arc newArc (MemGraph graph, Node startNode, Node endNode, String name)
      throws LoadException
  {
    MemArc arc = new MemArc( name, startNode, endNode );
    graph.getArcs().add( arc );
    return arc;
  }

  @Override
  public Node newNode (MemGraph graph, String name, String type, boolean isJoin, boolean isStart, String guard, List<Object> customList)
    throws LoadException
  {
    NodeFactory nodeFactory = getNodeFactory( type );

    Node newNode = nodeFactory.newNode( type );

    MemNode node = null;

    if ( newNode instanceof CustomNode )
    {
      node = new MemCustomNodeWrapper( (CustomNode)newNode );
    }
    else
    {
      node = (MemNode)newNode;
    }

    node.initId();
    node.setGraph( graph );
    node.setName( name );
    node.setType( type );
    node.setJoin( isJoin );
    node.setStart( isStart );
    node.setGuard( guard );

    if ( customList != null )
    {
      for ( Object custom : customList )
      {
        nodeFactory.loadCustom( newNode, custom );
      }
    }

    graph.getNodes().add( node );

    return node;
  }

  @Override
  public Node importNode (MemGraph graph, Node node, String instanceName)
  {
    MemNode memNode = (MemNode)node;
    MemNode newNode = memNode.clone();
    newNode.setGraph( graph );
    newNode.setExternal( true );

    graph.getNodes().add( newNode );

    return newNode;
  }

  @Override
  public ArcToken newArcToken (GraphProcess process, Arc arc, ExecutionType executionType, NodeToken parent)
  {
    return new MemArcToken( arc, process, executionType, parent );
  }

  @Override
  public NodeToken newNodeToken (GraphProcess process, Node node, ExecutionType executionType, List<ArcToken> parents)
  {
    MemGraphProcess memGraphProcess = (MemGraphProcess)process;

    MemNodeToken token = new MemNodeToken( memGraphProcess.nextTokenId(), node, process, executionType, parents );
    Env env = token.getEnv();

    for ( ArcToken t : parents )
    {
      env.importEnv( t.getParentToken().getEnv() );
    }

    return token;
  }

  @Override
  public MemGraphProcess newProcess (Graph graph)
  {
    return new MemGraphProcess( graph );
  }

  @Override
  public GraphProcess newNestedProcess (Graph graph, NodeToken parentToken)
  {
    MemGraphProcess process = newProcess( graph );
    process.setParentToken( parentToken );
    return process;
  }
}