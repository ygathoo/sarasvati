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
import java.util.List;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
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

  protected long insertNodeRef (JdbcGraph graph, JdbcNode node, String instance)
  {
    AbstractInsertStatement ex = dialect.newNodeRefInsertStatement( graph, node, instance );
    ex.execute( connection );
    return ex.getGeneratedId();
  }

  @Override
  public Node importNode (JdbcGraph graph, Node node, String instanceName)
  {
    JdbcNodeRef nodeRef = (JdbcNodeRef)node;

    String label = getInstance( nodeRef.getInstance(), instanceName );

    long nodeRefId = insertNodeRef( graph, nodeRef.getNode(), label );
    JdbcNodeRef newRef = new JdbcNodeRef( nodeRefId, graph, nodeRef.getNode(), label );
    graph.getNodes().add( newRef );

    return newRef;
  }

  @Override
  public Arc newArc (JdbcGraph graph, Node startNode, Node endNode, String name)
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
  public Node newNode (JdbcGraph graph, String name, String type, boolean isJoin, boolean isStart,
                       String guard, List<Object> customList) throws LoadException
  {
    AbstractInsertStatement ex =
      dialect.newNodeInsertStatement( graph, name, type, guard, isStart, isJoin );
    ex.execute( connection );

    long nodeId = ex.getGeneratedId();

    NodeFactory nodeFactory = getNodeFactory( type );
    JdbcNode newNode = (JdbcNode)nodeFactory.newNode( type );

    newNode.setId( nodeId );
    newNode.setGraph( graph );
    newNode.setName( name );
    newNode.setType( type );
    newNode.setStart( isStart );
    newNode.setJoin( isJoin );
    newNode.setGuard( guard );

    long nodeRefId = insertNodeRef( graph, newNode, "" );
    JdbcNodeRef nodeRef = new JdbcNodeRef( nodeRefId, graph, newNode, "" );

    return nodeRef;
  }

  @Override
  public GraphProcess newProcess (Graph graph)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public GraphProcess newNestedProcess (Graph graph, NodeToken parentToken)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public NodeToken newNodeToken (GraphProcess process,
                                 Node node,
                                 ExecutionType executionType,
                                 List<ArcToken> parents,
                                 NodeToken envToken)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public ArcToken newArcToken (GraphProcess process,
                               Arc arc,
                               ExecutionType executionType,
                               NodeToken parent)
  {
    // TODO Auto-generated method stub
    return null;
  }
}