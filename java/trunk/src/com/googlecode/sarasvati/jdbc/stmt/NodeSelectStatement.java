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
package com.googlecode.sarasvati.jdbc.stmt;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.jdbc.JdbcGraph;
import com.googlecode.sarasvati.jdbc.JdbcGraphFactory;
import com.googlecode.sarasvati.jdbc.JdbcNode;
import com.googlecode.sarasvati.jdbc.JdbcNodeRef;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.load.NodeFactory;

public class NodeSelectStatement extends AbstractSelectStatement<JdbcNodeRef>
{
  protected JdbcGraph graph;
  protected JdbcGraphFactory factory;

  protected Map<Long, JdbcNode> nodeMap = new HashMap<Long,JdbcNode>();

  public NodeSelectStatement (String sql, JdbcGraph graph, JdbcGraphFactory factory)
  {
    super( sql, false );
    this.graph = graph;
    this.factory = factory;
  }

  @Override
  protected JdbcNodeRef loadObject (ResultSet row) throws SQLException, LoadException
  {
    long nodeRefId  = row.getLong( 1 );
    String instance = row.getString( 2 );
    long nodeId     = row.getLong( 3 );
    String name     = row.getString( 4 );
    String type     = row.getString( 5 );
    boolean isStart = "Y".equalsIgnoreCase( row.getString( 6 ) );
    boolean isJoin  = "Y".equalsIgnoreCase( row.getString( 7 ) );
    String guard    = row.getString( 8 );

    JdbcNode node = nodeMap.get( nodeId );

    if ( node == null )
    {
      NodeFactory nodeFactory = factory.getNodeFactory( type );
      node = (JdbcNode)nodeFactory.newNode( type );

      node.setId( nodeId );
      node.setGraph( graph );
      node.setName( name );
      node.setType( type );
      node.setStart( isStart );
      node.setJoin( isJoin );
      node.setGuard( guard );

      nodeMap.put( nodeId, node );
    }

    JdbcNodeRef nodeRef = new JdbcNodeRef( nodeRefId, graph, node, instance );

    graph.getNodes().add( nodeRef );
    return nodeRef;
  }

  @Override
  protected void setParameters (PreparedStatement stmt) throws SQLException
  {
    stmt.setLong( 1, graph.getId() );
  }
}
