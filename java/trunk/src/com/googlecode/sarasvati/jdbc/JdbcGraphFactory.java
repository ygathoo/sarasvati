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

import java.sql.Connection;
import java.util.Date;
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
import com.googlecode.sarasvati.jdbc.stmt.AbstractInsertStatement;
import com.googlecode.sarasvati.load.AbstractGraphFactory;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.load.NodeFactory;

public class JdbcGraphFactory extends AbstractGraphFactory<JdbcGraph>
{
  protected DatabaseDialect dialect;
  protected Connection connection;

  public JdbcGraphFactory (final DatabaseDialect dialect, final Connection connection)
  {
    super( JdbcNode.class );
    this.dialect = dialect;
    this.connection = connection;
  }

  protected Connection getConnection ()
  {
    return connection;
  }

  protected long insertNodeRef (final JdbcGraph graph,
                                final JdbcNode node,
                                final String instance)
  {
    AbstractInsertStatement ex = dialect.newNodeRefInsertStatement( graph, node, instance );
    ex.execute( connection );
    return ex.getGeneratedId();
  }

  @Override
  public Node importNode (final JdbcGraph graph,
                          final Node node,
                          final String instanceName)
  {
    JdbcNodeRef nodeRef = (JdbcNodeRef)node;

    String label = getInstance( nodeRef.getInstance(), instanceName );

    long nodeRefId = insertNodeRef( graph, nodeRef.getNode(), label );
    JdbcNodeRef newRef = new JdbcNodeRef( nodeRefId, graph, nodeRef.getNode(), label );
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

    AbstractInsertStatement ex = dialect.newArcInsertStatement( graph, startNodeRef, endNodeRef, name );
    ex.execute( connection );
    long arcId = ex.getGeneratedId();

    return new JdbcArc( arcId, graph, startNodeRef, endNodeRef, name );
  }

  @Override
  public JdbcGraph newGraph (final String name, final int version)
  {
    AbstractInsertStatement ex = dialect.newGraphInsertStatement( name, version );
    ex.execute( connection );
    long graphId = ex.getGeneratedId();
    return new JdbcGraph( graphId, name, version );
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
    AbstractInsertStatement ex =
      dialect.newNodeInsertStatement( graph, name, type, guard, isStart, isJoin );
    ex.execute( connection );

    long nodeId = ex.getGeneratedId();

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

    node.setId( nodeId );
    node.setGraph( graph );
    node.setName( name );
    node.setType( type );
    node.setStart( isStart );
    node.setJoin( isJoin );
    node.setGuard( guard );

    long nodeRefId = insertNodeRef( graph, node, "" );
    return new JdbcNodeRef( nodeRefId, graph, node, "" );
  }

  @Override
  public GraphProcess newProcess (final Graph graph)
  {
    return newNestedProcess( graph, null );
  }

  @Override
  public GraphProcess newNestedProcess (final Graph graph, final NodeToken parentToken)
  {
    JdbcGraph g = (JdbcGraph)graph;
    JdbcNodeToken token = (JdbcNodeToken)parentToken;

    Date createDate = new Date();
    AbstractInsertStatement stmt = dialect.newProcessInsertStatement( g, token, createDate );
    stmt.execute( connection );

    return new JdbcGraphProcess( stmt.getGeneratedId(), g, token, createDate );
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