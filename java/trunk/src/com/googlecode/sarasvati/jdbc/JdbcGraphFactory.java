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
package com.googlecode.sarasvati.jdbc;

import java.util.List;
import java.util.Map;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.CustomNode;
import com.googlecode.sarasvati.ExecutionType;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.jdbc.dialect.DatabaseDialect;
import com.googlecode.sarasvati.load.AbstractGraphFactory;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.load.NodeFactory;

public class JdbcGraphFactory extends AbstractGraphFactory<JdbcGraph>
{
  private final JdbcEngine engine;

  public JdbcGraphFactory (final JdbcEngine engine)
  {
    super( JdbcNode.class );
    this.engine = engine;
  }

  public JdbcEngine getEngine ()
  {
    return engine;
  }

  private DatabaseDialect getDialect ()
  {
    return engine.getDatabaseDialect();
  }

  @Override
  public Node importNode (final JdbcGraph graph,
                          final Node node,
                          final String instanceName)
  {
    JdbcNodeRef nodeRef = (JdbcNodeRef)node;

    String label = getInstance( nodeRef.getInstance(), instanceName );

    JdbcNodeRef newRef = new JdbcNodeRef( graph, nodeRef.getNode(), label );
    getDialect().newNodeRefInsertStatement( newRef ).execute( engine );
    graph.getNodes().add( newRef );
    return newRef;
  }

  @Override
  public Arc newArc (final JdbcGraph graph,
                     final Node startNode,
                     final Node endNode,
                     final String name)
  {
    JdbcNodeRef startNodeRef = (JdbcNodeRef)startNode;
    JdbcNodeRef endNodeRef   = (JdbcNodeRef)endNode;

    JdbcArc arc = new JdbcArc( graph, startNodeRef, endNodeRef, name );
    getDialect().newArcInsertStatement( arc ).execute( engine );
    return arc;
  }

  @Override
  public JdbcGraph newGraph (final String name, final int version)
  {
    JdbcGraph graph = new JdbcGraph( name, version );
    getDialect().newGraphInsertStatement( graph ).execute( engine );
    return graph;
  }

  @Override
  public Node newNode (final JdbcGraph graph,
                       final String name,
                       final String type,
                       final boolean isJoin,
                       final boolean isStart,
                       final String guard,
                       final List<Object> customList)
    throws LoadException
  {
    NodeFactory nodeFactory = getNodeFactory( type );
    Node newNode = nodeFactory.newNode( type );

    JdbcNode node = null;
    JdbcCustomNodeWrapper customNodeWrapper = null;

    if (newNode instanceof CustomNode )
    {
      customNodeWrapper = new JdbcCustomNodeWrapper( (CustomNode)newNode );
      node = customNodeWrapper;
    }
    else
    {
      node = (JdbcNode)newNode;
    }

    if ( customList != null )
    {
      for ( Object custom : customList )
      {
        Map<String, String> customProps = nodeFactory.loadCustom( newNode, custom );

        // If this is a custom node, we need save the properties in the CustomNodeWrapper
        // as well as in the CustomNode, so that they can be set back in when the CustomNode
        // is re-created, after being loaded from the database
        if ( customNodeWrapper != null )
        {
          customNodeWrapper.importProperties( customProps );
        }
      }
    }

    node.setGraph( graph );
    node.setName( name );
    node.setType( type );
    node.setStart( isStart );
    node.setJoin( isJoin );
    node.setGuard( guard );

    getDialect().newNodeInsertStatement( node ).execute( engine );

    node.afterCreate( engine );

    JdbcNodeRef nodeRef = new JdbcNodeRef( graph, node, "" );
    getDialect().newNodeRefInsertStatement( nodeRef ).execute( engine );
    return nodeRef;
  }

  @Override
  public GraphProcess newProcess (final Graph graph)
  {
    return newNestedProcess( graph, null );
  }

  @Override
  public GraphProcess newNestedProcess (final Graph graph, final NodeToken parentToken)
  {
    JdbcGraphProcess process = new JdbcGraphProcess( (JdbcGraph)graph, (JdbcNodeToken)parentToken );
    getDialect().newProcessInsertStatement( process ).execute( engine );
    return process;
  }

  @Override
  public NodeToken newNodeToken (final GraphProcess process,
                                 final Node node,
                                 final ExecutionType executionType,
                                 final List<ArcToken> parents,
                                 final NodeToken envToken)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public ArcToken newArcToken (final GraphProcess process,
                               final Arc arc,
                               final ExecutionType executionType,
                               final NodeToken parent)
  {
    // TODO Auto-generated method stub
    return null;
  }
}